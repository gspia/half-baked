{-
  Idea shortly: select several files and print their names, sizes and contents.
  This is based on fileinput-example at reflex-examples.
  https://github.com/reflex-frp/reflex-examples
  This works with reflex-platform https://github.com/reflex-frp/reflex-platform

  Questions: some of the type errors were quite difficult. -> why they occur &
  what they mean? They can be tried by making little changes below.
  How to structure the program?
-}
{-# LANGUAGE RecursiveDo, TypeFamilies, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}

import Reflex.Dom
import Reflex.Host.Class
import Data.Dependent.Sum
import Data.List
import Data.Monoid
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad
import GHCJS.Marshal
import GHCJS.Types
import GHCJS.DOM.Blob
import GHCJS.DOM.FileReader
import GHCJS.DOM.File (getName)
import GHCJS.DOM.Types (File, UIEvent, liftJSM, MonadJSM, FromJSString)
import GHCJS.DOM.EventM
import Language.Javascript.JSaddle.String (textFromJSString)
import GHC.Word (Word64)

fileName :: MonadJSM m => File -> m Text
fileName request = fmap textFromJSString $ getName request

-- getSize
fileSize :: (MonadJSM m, IsBlob self) => self -> m GHC.Word.Word64
fileSize fl = getSize fl


getFNames :: (PerformEvent t m, MonadJSM (Performable m))
          => Dynamic t [File] -> m (Event t [Text])
getFNames fDyn = performEvent $ fmap (sequence . fmap fileName) . updated $ fDyn

main :: IO ()
main = mainWidget $ do
  header
  filesDyn :: Dynamic (SpiderTimeline Global) [File]
    -- 3 following lines seem to do the same thing (ok)
    -- <- value <$> (fileInput $ def & attributes .~ constDyn ("multiple" =: "multiple"))
    -- <- value <$> fdyn1
    <- fdyn2
    -- <- join fdyn2'  -- does not compile
    -- <- fdyn2'       -- does not compile
  fnamesE :: Event (SpiderTimeline Global) [Text] <- getFNames filesDyn
    -- <- getFNames3 filesDyn
    -- does not compile when used in place of getFNames, why?
    -- ghcjs's and ghci's error msg is: No instance for (MonadJS ...
  el "div" . widgetHold blank . ffor fnamesE $ \fnms -> do
    text "The files are: "
    el "br" blank
    -- text $ (T.pack . show) fnms
    forM_ fnms $ \l -> do
      text l
      el "br" blank
  --
  let evfs :: Event (SpiderTimeline Global) [File] = updated filesDyn
  el "div" . widgetHold blank . ffor evfs $ \fls -> do
    forM_ fls $ \f -> do
      text "The filename is  "
      fn <- fileName f
      text fn
      el "br" blank
      --
      text "The file size is "
      fz <- fileSize f
      text $ T.pack . show $ fz
      el "br" blank
      --
      text "The file contents are: "
      el "br" blank
      e <- textFR f
      widgetHold (text "no files were selected") $ ffor e $ \e' -> text (T.pack $ show e')
      el "br" blank
  el "br" blank

  -- fnamesD :: Dynamic (SpiderTimeline Global) [Text] <- getFNames2 filesDyn
  -- The above line does not compile, why?
  -- ghci's error msg is: Couldn't match type
  -- ‘DomBuilderSpace (Dynamic (SpiderTimeline Global))’ with ‘GhcjsDomSpace’
  -- arising from a use of ‘getFNames2’. This is quite close to the one
  -- in reflex-frp-docs, see
  -- http://reflex-frp.readthedocs.io/en/latest/guide_to_dom_creation.html#troubleshooting-type-class-errors
  -- The following does not help:
  -- let fnamesE2 = updated fnamesD
  -- el "div" . widgetHold blank . ffor fnamesE2 $ \fnms -> do
  --   text $ (T.pack . show) fnms
  footer

textFR :: MonadWidget t m => File -> m (Event t (Maybe Text))
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
-- Trying out things to learn about the types.
fdyn1 :: forall t m. (MonadJSM m, MonadIO m, MonadFix m, MonadHold t m,
  TriggerEvent t m, DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace)
  => m (FileInput (DomBuilderSpace m) t)
fdyn1 = fileInput $ def & attributes .~ constDyn ("multiple" =: "multiple")


-- The following was found with the help of ghci (by using holes): it compiles
-- but doesn't work (at least not in the way as tried above, not with or without join):
fdyn2' :: (DomBuilderSpace (Dynamic t) ~ GhcjsDomSpace, Monad m,
     MonadFix (Dynamic t), MonadHold t1 (Dynamic t),
     DomBuilder t1 (Dynamic t), PostBuild t1 (Dynamic t),
     MonadJSM (Dynamic t), TriggerEvent t1 (Dynamic t)) =>
    m (Dynamic t (Dynamic t1 [File]))
fdyn2' = return $ value <$> fdyn1

-- This works:
fdyn2 :: (-- MonadIO m,
     MonadJSM m, MonadFix m, MonadHold t m,
     TriggerEvent t m, DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace
     ) =>
    m (Dynamic t [File])
fdyn2 = value <$> fdyn1
-- Is there a type synonym for some of the types in fdyn2?


--------------------------------------------------------------------------------
-- Instead of turning a file into a text in a context, we turn a file into a
-- text inside context.
fileName2 :: (MonadJSM m, MonadFix m, MonadHold t m,
      DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace
             ) => m File -> m Text
fileName2 evF = ((fmap textFromJSString) . join . (fmap getName)) evF

-- The following is hand made. In fdyn2, ghci suggested a type where instead of
-- m there was another layer of dynamics. But by just replacing each of them
-- with m it worked. Trying out the same here didn't succeed as getFNames2 just
-- wanted to have that Dynamic t inside.
-- We can get rid of it with join but that doesn't help when trying to use
-- getFNames2. (Or the useage pattern is not like above.) Why? How?
-- getFNames2 :: (MonadJSM m, MonadFix m, MonadHold t m,
--       DomBuilder t m, PostBuild t m, DomBuilderSpace m ~ GhcjsDomSpace,
--       DomBuilderSpace (Dynamic t) ~ GhcjsDomSpace, DomBuilder t (Dynamic t),
--       MonadHold t (Dynamic t), MonadFix (Dynamic t), PostBuild t (Dynamic t),
--       MonadJSM (Dynamic t))
-- Suggested by ghci:
getFNames2
  :: (DomBuilderSpace (Dynamic t) ~ GhcjsDomSpace,
      MonadFix (Dynamic t), Monad m, MonadHold t (Dynamic t),
      PostBuild t (Dynamic t), MonadJSM (Dynamic t), DomBuilder t (Dynamic t))
    => Dynamic t [File] -> m (Dynamic t [Text])
getFNames2 evF = return $ join $ simpleList evF fileName2


--------------------------------------------------------------------------------
-- hoogle tells that there is a function fromJSString at GHCJS.DOM.Types.
-- Let's try it, too.
fileName3 :: (MonadJSM m, FromJSString (Reflex.Dom.JSRef x), MonadJS x m)
          => File -> m Text
fileName3 request = do
  fn <- getName request
  fmap T.pack $ fromJSString fn

-- We would like to get something like getFNames:
-- getFNames :: (PerformEvent t m, MonadJSM (Performable m))
--      => Dynamic t [File] -> m (Event t [Text])
--
-- getFNames3 has 2 extra constraints (MonadJS and FromJSString) and the whole
-- program refuses to compile if we try to use this in the same place as
-- getFNames. Why? How to use?
getFNames3 :: (PerformEvent t1 m, MonadJSM (Performable m),
      FromJSString (Reflex.Dom.JSRef x), MonadJS x (Performable m))
  => Dynamic t1 [File] -> m (Event t1 [Text])
getFNames3 fDyn = performEvent $ fmap (sequence . fmap fileName3) . updated $ fDyn


--------------------------------------------------------------------------------
-- From the original fileinput-example (at reflex-examples).

linkNewTab :: MonadWidget t m => Text -> Text -> m ()
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
    text "The code for this example can be found in the "
    linkNewTab "https://github.com/reflex-frp/reflex-examples" "Reflex Examples"
    text " repo."

