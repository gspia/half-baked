{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

{-
  Idea shortly: client initiates the connection by calling msgHandler-url
  and then server starts waiting for files: first comes short text and file size,
  then file name and as a last thing, the file. When the file is received, the
  file is written into the downloads-directory.

  TBD / questions:
  - see msgReadLoop
-}



------------------------------------------------------------------------------
import           Control.Concurrent (forkIO, MVar, newMVar, modifyMVar_,
                    modifyMVar, readMVar)
import qualified Control.Exception as CE
import           Control.Exception (try)
import           Control.Monad (forever, unless)
import           Control.Monad.Except
import           Control.Monad.Trans.Either
import           Control.Lens

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
import           Data.Time
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import           Data.Text.Encoding

import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import           System.Directory
import           System.Exit

import           Snap
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

------------------------------------------------------------------------------
-- See
-- https://github.com/jaspervdj/websockets-snap/blob/master/example/server.hs
-- and
-- https://github.com/jaspervdj/websockets/tree/master/example

newWSServerState :: WSServerState
newWSServerState = []

clientExists :: Client -> WSServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> WSServerState -> WSServerState
addClient client clients = client : clients

removeClient :: Client -> WSServerState -> WSServerState
removeClient client = filter ((/= fst client) . fst)

type Client = (Text, WS.Connection)
type WSServerState = [Client]
type MVWSSS = MVar WSServerState

------------------------------------------------------------------------------

type AppHandler = Handler App App

data App = App
    { -- _heist :: Snaplet (Heist App)
    _sess :: Snaplet SessionManager
    -- , _auth :: Snaplet (AuthManager App)
    , _wsss :: MVWSSS
    }

makeLenses ''App

-- instance HasHeist App where heistLens = subSnaplet heist

------------------------------------------------------------------------------
-- Source: a stackoverflow q about exceptions and its answer.
data MsgError = MsgError {meState :: String,
                          meErrorMsg :: String}
                deriving (Eq, Show, Read) -- , Typeable)

instance CE.Exception MsgError

tryIO :: (MonadError e m, MonadIO m, CE.Exception e) => IO a -> m a
tryIO = (>>= either throwError return) . liftIO . try

anyException :: EitherT CE.SomeException m a -> EitherT CE.SomeException m a
anyException = id

message :: (Show e, Functor m) => EitherT e m a -> EitherT String m a
message = bimapEitherT show id

connectionError :: EitherT WS.ConnectionException m a -> EitherT WS.ConnectionException m a
connectionError = id

handshakeError :: EitherT WS.HandshakeException m a -> EitherT WS.HandshakeException m a
handshakeError = id

msgError :: EitherT MsgError m a -> EitherT MsgError m a
msgError = id

------------------------------------------------------------------------------

msgHandler :: AppHandler ()
msgHandler = do
  -- usr <- with auth currentUser
  sapp <- getSnapletState
  let vapp = view snapletValue sapp
      usr = "User name" :: Text
  WS.runWebSocketsSnap $ msgHandlerApp vapp usr

msgHandlerApp :: (MonadIO m) => App -> Text -> WS.PendingConnection -> m ()
msgHandlerApp sApp usr pending = do
  liftIO $ T.putStrLn  $ "msgHandlerApp, 1. line, usr=" <> usr
  -- let requesthead = WS.pendingRequest pending
  conn <- liftIO $ WS.acceptRequest pending
  liftIO $ WS.forkPingThread conn 30 -- Ping every 30 secs to keep connection alive.
  liftIO $ T.putStrLn  "msgHandlerApp, after acceptRequest and forkPingThread"
  {- let client = (usr,conn) -}
  {- clients <- liftIO $ readMVar mvwsss -}
  {- unless (clientExists client clients) $ do -}
  {-     liftIO $ modifyMVar_ mvwsss (return . addClient client) -}
  {-     return () -}
  {- liftIO $ T.putStrLn  "msgHandlerApp, after modifyMVar_, next msgReadLoop" -}
  msgReadLoop conn
  {- where  -}
  {-   mvwsss = view wsss sApp -}



------------------------------------------------------------------------------
-- This receives messages from the client.
-- We read file size, name and then contents.
-- When uploading has no problems, this can receive several files that can be
-- large. Files are stored in "downloads" subdirectory relative to the
-- directory the server is running (remember to make it).
-- How to do questions / TBD:
--    - Exception/error handling with regards to reveiveData, this is not ok
--      below.
--    - If we decide that file is too large, how to tell it to the client and
--      abort the whole uploading of the file?
--    - And in case of hangling large files, should we split/splice the file
--      to thunks of known size?
--    - Should we check the filename before using it? (probably yes -> how?)
msgReadLoop :: (MonadIO m) => WS.Connection -> m ()
msgReadLoop conn = forever $ do
  liftIO $ T.putStrLn  "msgReadLoop, first line in forever loop"
  -- res <-  runEitherT (message . connectionError . tryIO $ liftIO (WS.receiveData conn :: IO ByteString))
  res <-  runEitherT (message . connectionError . tryIO
    $ liftIO (WS.receiveData conn :: IO Text))
  t <- liftIO getCurrentTime
  liftIO $ T.putStrLn 
    $ "msgReadLoop, after receiveData, at " <> (T.pack . show) t
  let msg = case res of
              Left _ -> "connectionError (first receiveData)"
              Right r -> r
  liftIO $ T.putStrLn $ "msgReadLoop, the first message is " <> msg
  when (msg == "connectionError (first receiveData)")
    $ liftIO (die "msgReadLoop problem1")
  when (prefix `T.isPrefixOf` msg) $ do
    let efsz = (T.decimal . T.drop (T.length prefix)) msg
          :: Either String (Int, Text)
        (fsz,_rtxt) = case efsz of
                Left _ -> (0, T.empty)
                Right i -> i
    res2 <-  runEitherT (message . connectionError . tryIO
      $ liftIO (WS.receiveData conn :: IO Text))
    let fname = case res2 of
              Left _ -> "connectionError (second receiveData)"
              Right r -> r
    liftIO $ T.putStrLn $ "msgReadLoop, the file name is "
      <> fname <> " and its size is " <> (T.pack . show) fsz <> "."
    when (msg == "connectionError (second receiveData)")
      $ liftIO (die "msgReadLoop problem2")
    res3 <-  runEitherT (message . connectionError . tryIO
      $ liftIO (WS.receiveData conn :: IO ByteString))
    ok <-
      liftIO $ case res3 of
        Left _ -> return "connectionError (third receiveData)"
        Right fileb -> do
          liftIO $ T.putStrLn "msgReadLoop, got the file"
          fnames <- getDirectoryContents d
          liftIO $ T.putStrLn $ "Files in directory are: "
            <> (T.pack . show) fnames
          if T.unpack fname `elem` fnames
             then return $ "File " <> fname <> " already exists"
             else do -- Write the file with given name into the directory.
               let fn = T.pack d <> fname
               liftIO $ T.putStrLn $ "writing file " <> (T.pack . show ) fn
               BS.writeFile (T.unpack fn) fileb
               return $ "just wrote the file " <> fname
    liftIO $ T.putStrLn $ "msgReadLoop, ok = " <> ok
    when (msg == "connectionError (third receiveData)")
      $ liftIO (die "msgReadLoop problem3")
  where
    prefix = "File coming:"
    d = "./downloads/" :: FilePath

------------------------------------------------------------------------------


hmm :: AppHandler ()
hmm = liftIO $ T.putStrLn "hmm"
  -- writeBS "Hello "


------------------------------------------------------------------------------
-- | The application's routes.  From template, stripped down.
routes :: [(ByteString, AppHandler ())]
{- routes = [ ("/",   writeText "hello") -}
routes = [ -- ("login",    with auth handleLoginSubmit)
         -- , ("logout",   with auth handleLogout)
         -- , ("new_user", with auth handleNewUser)
         -- , ("loginInits", loginInits)
         ("/",   serveDirectory "static" >> hmm)
         , ("msgHandler", msgHandler)
         ]

------------------------------------------------------------------------------
-- | The application initializer. From template, stripped down.
app :: SnapletInit App App
app = makeSnaplet "app2" "An snaplet example application." Nothing $ do
  -- h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "" sess $
      initCookieSessionManager "site_key.txt" "_cookie" Nothing (Just 3600)
    -- a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    w <- liftIO $ newMVar newWSServerState
    --
    addRoutes routes
    return $ App s  w


------------------------------------------------------------------------------
-- |  Main with defaul values (e.g. logging).
--
main :: IO ()
main =
  serveSnapletNoArgParsing defaultConfig app
  -- httpServe defaultConfig app
  -- _ <- try $ httpServe defaultConfig app :: IO (Either SomeException ())



------------------------------------------------------------------------------
-- | Render login form
{- handleLogin :: Maybe T.Text -> Handler App (AuthManager App) () -}
{- handleLogin authError = heistLocal (I.bindSplices errs) $ render "login" -}
{-   where -}
{-     errs = maybe mempty splice authError -}
{-     splice err = "loginError" ## I.textSplice err -}


------------------------------------------------------------------------------
-- | Handle login submit
{- handleLoginSubmit :: Handler App (AuthManager App) () -}
{- handleLoginSubmit = -}
{-     loginUser "login" "password" Nothing -}
{-               (\_ -> handleLogin err)  -}
{-               (redirect "/loginInits") -}
{-               -- (redirect "/koe") -}
{-               -- (redirect "/dist") -}
{-   where -}
{-     err = Just "Unknown user or password" -}

------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
{- handleLogout :: Handler App (AuthManager App) () -}
{- handleLogout = logout >> redirect "/" -}

------------------------------------------------------------------------------
-- | Handle new user form submit
{- handleNewUser :: Handler App (AuthManager App) () -}
{- handleNewUser = method GET handleForm <|> method POST handleFormSubmit -}
{-   where -}
{-     handleForm = render "new_user" -}
{-     handleFormSubmit = registerUser "login" "password" >> redirect "/dist" -}

------------------------------------------------------------------------------

-- loginInits :: Handler App App ()
{- loginInits :: AppHandler () -}
{- loginInits = do -}
{-   withKatip $ logFM InfoS "loginInits, Calling msgHandler" -}
{-   redirect "/dist" -}
{-   msgHandler -}

