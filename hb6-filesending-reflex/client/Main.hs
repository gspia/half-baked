{-
  Idea shortly: select several files and send the files to server.
  This is based on fileinput- and websocket-examples at reflex-examples.
  https://github.com/reflex-frp/reflex-examples
  This works with reflex-platform
  https://github.com/reflex-frp/reflex-platform

  Question/problems: see the comments below.
  Shortly: below code doesn't use reflex websocket in a satisfactory way (ghcjs
  websockets worked) and the question remains, how to apply websockets.

  Note that Reflex.Dom.WebSocket.Foreign is hidden. There is a class
  IsWebSocketMessage. If it were not hidden, we might could derive that
  ArrayBuffer is a message. Now we get "Couldn't deduce IsWebSocketMessage
  ArrayBuffer". (There seems to be only Either, ByteString and Text instances
  at the moment.) Or is there a reason that it is hidden and that there is no
  more instances available?
-}
{-# LANGUAGE RecursiveDo, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Foreign.JavaScript.Utils (bsFromMutableArrayBuffer, bsToArrayBuffer)
import Reflex.Dom
-- import Reflex.Dom.WebSocket.Foreign -- hidden module
import Reflex.Host.Class
import Data.ByteString as DB
import Data.ByteString.Lazy as LB
import Data.Dependent.Sum
import Data.List as DL
import Data.Monoid
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad
import Control.Comonad (extract)
import GHCJS.Marshal
import GHCJS.Types
import qualified GHCJS.DOM.WebSocket as GDOM
import GHCJS.DOM.Blob
import GHCJS.DOM.FileReader
import GHCJS.DOM.File (getName)
import GHCJS.DOM.Types (File, UIEvent, liftJSM, MonadJSM, FromJSString, ArrayBuffer)
import GHCJS.DOM.EventM
import Language.Javascript.JSaddle.String (textFromJSString)
import GHC.Word (Word64)
import GHC.Generics


main :: IO ()
main = mainWidget $ do
  header
  filesD <- fileInput $ def & attributes .~ constDyn ("multiple" =: "multiple")
  let filesEv = updated $ _fileInput_value filesD
      url = "ws://localhost:8000/msgHandler" :: T.Text
  -- The following way to read and send files seems to work.
  ws <- GDOM.newWebSocket url ([]::[T.Text])
  el "div" . widgetHold blank . ffor filesEv $ \fls -> do
    forM_ fls $ \f -> do
      fn <- fileName f
      text $ "Try to send file: " <> fn
      el "br" blank
      textFR2 ws f

  -- The following seems to work with "text files" (e.g. index.html).
  -- But sending JPGs didn't work. Why? Is it that encoding?
  -- Or should the filereader read always binary? Note that file is read as
  -- text with fileReader and not as a ArrayBuffer. This is probably missing
  -- something very obvious.
  --
  -- Each message is ByteString: if we want to use this to different
  -- kinds of messages, we might should use json-data or something similar.
  -- How to make the arrayBuffer work?
  --
  -- This seems to open a new connection for each file (select several in
  -- fileInput). When would it be ok? Now the server has assumption that
  -- there is one connection (it forkPings) and that the connection could
  -- be used to send other messages, too. -> the below structure is probably
  -- not ok. How to correct? E.g. see the trial below (not ok one).
  {-
  el "div" . widgetHold blank . ffor filesEv $ \fls -> do
    forM_ fls $ \f -> do
      text "The file size is "
      fz <- fileSize f
      text $ T.pack . show $ fz
      el "br" blank
      text "The filename is  "
      fn <- fileName f
      text fn
      el "br" blank
      mfcEv <- textFR f
      -- mfcEv <- textFRB f
      -- mfcEv <- arrayBufferFR f
      widgetHold (text "no files were selected") $ ffor mfcEv $ \mfc -> do
        -- text (T.pack $ show mfc)
        let bsSz :: DB.ByteString = encodeUtf8 $ "File coming:" <> (T.pack . show) fz
            bsFn :: DB.ByteString = encodeUtf8 fn
            bsFc :: DB.ByteString = maybe "" encodeUtf8 mfc -- with textFR
            -- bsFc :: DB.ByteString = maybe "" (encodeUtf8 . T.pack . DL.concat . fromJSString) mfc
            -- with textFRB - not working (compiling). Can we turn JSString to ByteString?
            -- bsFc :: DB.ByteString = maybe "" (fromJSVal . toJSVal) mfc
            -- with arrayBufferFR - not quite working
            -- E.g. how to turn ArrayBuffer to ByteString?
        evStart <- delay 0 =<< getPostBuild
        bsDyn <- holdDyn [] $ [bsSz, bsFn, bsFc] <$ evStart
        let bsEv :: Event (SpiderTimeline Global) [DB.ByteString] = tagPromptlyDyn bsDyn evStart
        openWebSocket bsEv
        el "br" blank
      el "br" blank
  -}

  -- The following is not ok! Idea was to use the openWebsocket so that we
  -- could use it to send other data, too. And so that it wouldn't open a new
  -- connection for each file send.
  {-
  el "div" . widgetHold blank . ffor filesEv $ \fls -> do
    bss :: [(DB.ByteString, DB.ByteString, Event (SpiderTimeline Global) DB.ByteString)] <- forM fls $ \f -> do
      fz <- fileSize f
      fn <- fileName f
      fc <- textFR f
      return (encodeUtf8 fn, (encodeUtf8 . T.pack . show) fz, fmapMaybe mayT2mayBs fc)
    el "br" blank
    let (l1,l2,fc2) = unzip3 bss
        fzfnLst = DL.zip l1 l2
        evLst = fmap mapbbev bss
        bsEv = leftmost evLst -- compiles but does not work
    -- bsDyn <- holdDyn [] $ fzfnLst <$ evStart
    evStart <- delay 0 =<< getPostBuild
    -- bsDyn <- holdDyn [] $ () <$ evStart
    -- let bsEv2 = sequence evLst
    --     bsEv = fmap DL.concat bsEv2
    openWebSocket bsEv
    el "br" blank
  -}
  footer


-- (Note that this is modified from HSnippet code.)
gateDyn :: Reflex t => Dynamic t Bool -> Event t a -> Event t a
gateDyn d e = attachPromptlyDynWithMaybe (\b a -> if b then Just a else Nothing) d e

-- Compiles but does not work in every situation. The problem is probably in the
-- way the files are read (see comments above and readFR below).
-- (Note that this is modified from HSnippet code which contains more general
-- function.)
openWebSocket
    :: MonadWidget t m
    => Event t [DB.ByteString]
    -> m (Event t ())
openWebSocket bsMsg = do
    rec ws <- webSocket "ws://localhost:8000/msgHandler" $
          def & webSocketConfig_send .~ send
        websocketReady <- holdDyn False $ True <$ _webSocket_open ws
        websocketNotReady <- (return . fmap not) websocketReady
        buffer <- foldDyn (++) [] $ gateDyn websocketNotReady bsMsg
        let send = leftmost
                     [ gateDyn websocketReady bsMsg
                     , tag (current buffer) (_webSocket_open ws)
                     ]
    return $ _webSocket_open ws


{-
  -- Couldn't deduce IsWebSocketMessage ArrayBuffer.
  -- IsWebSocketMessage is in hidden module.
  -- Note that if this would compile, then we should try to transform the file
  -- name and size to ArrayBuffer (maybe with bsToArrayBuffer).
openWebSocketB
    :: MonadWidget t m
    => Event t [ArrayBuffer]
    -> m (Event t ())
openWebSocketB bsMsg = do
    rec ws <- webSocket "ws://localhost:8000/msgHandler" $
          def & webSocketConfig_send .~ send
        websocketReady <- holdDyn False $ True <$ _webSocket_open ws
        websocketNotReady <- (return . fmap not) websocketReady
        buffer <- foldDyn (++) [] $ gateDyn websocketNotReady bsMsg
        let send = leftmost
                     [ gateDyn websocketReady bsMsg
                     , tag (current buffer) (_webSocket_open ws)
                     ]
    return $ _webSocket_open ws
-}


--------------------------------------------------------------------------------

extFN :: MonadJSM m => [File] -> m [T.Text]
extFN r = sequence $ fmap fileName r
-- extFN r = performEvent $ fmap fileName r

mayT2mayBs :: Maybe T.Text -> Maybe DB.ByteString
mayT2mayBs = liftM encodeUtf8

mapbbev :: Functor f => (t,t, f t) -> f [t]
mapbbev (b1,b2,e) = fmap (\e' -> [b1,b2,e']) e


--------------------------------------------------------------------------------
-- From the hb5 - fileinput -example.

fileName :: MonadJSM m => File -> m T.Text
fileName request = fmap textFromJSString $ getName request

getFNames :: (PerformEvent t m, MonadJSM (Performable m))
          => Dynamic t [File] -> m (Event t [T.Text])
getFNames fDyn = performEvent $ fmap (sequence . fmap fileName) . updated $ fDyn

fileSize :: (MonadJSM m, IsBlob self) => self -> m GHC.Word.Word64
fileSize fl = getSize fl

getFSizes :: (PerformEvent t m, MonadJSM (Performable m))
          => Dynamic t [File] -> m (Event t [GHC.Word.Word64])
getFSizes fDyn = performEvent $ fmap (sequence . fmap fileSize) . updated $ fDyn
-- fNms :: Event (SpiderTimeline Global) [T.Text] <- getFNames $ value filesD
-- fSzs :: Event (SpiderTimeline Global) [GHC.Word.Word64] <- getFSizes $ value filesD

textFR :: MonadWidget t m => File -> m (Event t (Maybe T.Text))
textFR f = do
  fileReader <- liftJSM newFileReader
  -- readAsText fileReader (Just f) (Nothing :: Maybe T.Text)
  readAsText fileReader (Just f) (Just "utf-8":: Maybe T.Text)
  e <- wrapDomEvent fileReader (`on` loadEnd) . liftJSM $ do
     v <- getResult fileReader
     s <- (fromJSVal <=< toJSVal) v
     return s
  return e


--------------------------------------------------------------------------------
-- Note that if we try not to "widgetHold" result events, this does not work.
textFR2 :: MonadWidget t m => GDOM.WebSocket -> File -> m ()
textFR2 ws f = do
  fileReader <- liftJSM newFileReader
  fn <- fileName f
  fs <- fileSize f
  readAsArrayBuffer fileReader (Just f) -- Now we have to wait for loadEnd-event.
  e :: Event t () <- wrapDomEvent fileReader (`on` loadEnd) . liftJSM $ do
     v <- getResult fileReader
     s' :: Maybe ArrayBuffer <- (fromJSVal <=< toJSVal) v
     case s' of
       Nothing -> return ()
       Just s -> do
         GDOM.sendString ws $ "File coming:" <> (T.pack . show) fs
         GDOM.sendString ws fn
         GDOM.send ws s
     return ()
  -- Following line is required (or the files are not sent). Are there other
  -- ways to ensure that event will be performed? Or some other function to call
  -- than widgetHold or hold? Why we need one of them?
  -- Is this valid: "e may not fire on the frame t when the reading starts" and
  -- thus "we have to wait (hold) with something that refers to e"?
  -- If it is valid, why e is not happening without referring to it?
  -- widgetHold blank $ ffor e (\_ -> return ())
  hold () e
  -- Are the two above lines equal?
  return ()


-- textFRB :: MonadWidget t m => File -> m (Event t (Maybe ByteString))
textFRB :: MonadWidget t m => File -> m (Event t (Maybe JSString))
textFRB f = do
  fileReader <- liftJSM newFileReader
  readAsText fileReader (Just f) (Just "utf-8":: Maybe T.Text)
  e <- wrapDomEvent fileReader (`on` loadEnd) . liftJSM $ do
     v <- getResult fileReader
     s <- (fromJSVal <=< toJSVal) v
     return s
  return e


-- arrayBufferFR :: MonadWidget t m => File -> m (Event t [ArrayBuffer])
arrayBufferFR :: MonadWidget t m => File -> m (Event t (Maybe ArrayBuffer))
arrayBufferFR f = do
  fileReader <- liftJSM newFileReader
  readAsArrayBuffer fileReader (Just f)
  e <- wrapDomEvent fileReader (`on` loadEnd) . liftJSM $ do
     v <- getResult fileReader
     s <- (fromJSVal <=< toJSVal) v
     return s
  -- return $ fmap maybeToList e
  return e

--------------------------------------------------------------------------------
-- From the original fileinput-example (at reflex-examples).

linkNewTab :: MonadWidget t m => T.Text -> T.Text -> m ()
linkNewTab href s =
  elAttr "a" ("href" =: href <> "target" =: "_blank") $ text s

header :: MonadWidget t m => m ()
header = do
  el "strong" $ do
    linkNewTab "https://github.com/reflex-frp/reflex-dom" "Reflex.Dom"
    text " FileInput test page"
  el "p" $ do
    text "Select some files."

footer :: MonadWidget t m => m ()
footer = do
  el "hr" $ return ()
  el "p" $ do
    text "This is footer"

