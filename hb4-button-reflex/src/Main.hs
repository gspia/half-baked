{-
  This is based on examples about reflex, e.g.
    - https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md
      (e.g. src/dom04.hs and other in that tutorial)
    - https://github.com/reflex-frp/reflex-examples
  Other tutorials/docs
    - http://docs.reflex-frp.org/en/latest/architecture.html
    - https://github.com/reflex-frp/reflex/blob/develop/Quickref.md
    - https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md
    - https://github.com/mightybyte/real-world-reflex/blob/master/index.md

  Idea shortly:
  In the following we define a dom-element that is shown based on a state, with
  attributes that are based on a state. toggleButton shows an example, how to use
  traceEvent. And as a third thing, getElementById finds a div-element.

  Questions:
    - More information about types and type synonyms used / defined for reflex,
      reflex-dom etc related libs? Where to find sources / or is somewhere a
      tutorial about these?
    - By making little change shown below (toggleButton signature), we get type
      error. How to read it with common layman language & what is its meaning?
      Change the type of toggleButton to the other one to see the error message.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

import Reflex.Dom
import Data.Time
import Data.String
import Control.Monad.Trans (liftIO)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid
import qualified Data.Text as T
import GHCJS.DOM (currentDocument)
import qualified GHCJS.DOM.Types as G
import GHCJS.DOM.Element (getTagName)
import GHCJS.DOM.NonElementParentNode (getElementById)

--------------------------------------------------------------------------------

main = mainWidget $ do
  now <- liftIO getCurrentTime
  evTick <- tickLossy 1 now -- NominalDiffTime one second
  let evTime = (T.pack . show . _tickInfo_lastUTC) <$>  evTick
  el "div" $ do
    text "Hello, world!"
    dynText =<< holdDyn "No ticks yet" evTime
    el "div" $ do
      rec
        txtContent isVisible
        isVisible <- toggle False evToggle
        nums <- foldDyn (+) (0::Int) (1 <$ evToggle)
        evToggle <- textWithDynamicVisibility isVisible nums
      return ()
    -- geteEv <- geteWidget "txtContextId"
    evStart <- delay 0 =<< getPostBuild
    -- geteEv <- geteW "txtContextId" evTime
    geteEv <- getElemEv "txtContextId" $ evStart
    geteD <- holdDyn "no element found" geteEv
    dynText geteD

getElemEv :: MonadWidget t m => T.Text -> Event t b ->  m (Event t T.Text)
getElemEv id ev = do
  b <- holdDyn False $ True <$ ev
  e <- dyn $ ffor b $ \b' ->
    if b'
       then elTxt =<< (\d -> findEl d id) =<< currentDocument
       else return "empty"
  return e

geteWidget :: MonadWidget t m => T.Text -> m (Event t T.Text)
geteWidget id = do
  findEv <- button "Press this to find the tag"
  el "br" blank
  b <- holdDyn False $ True <$ findEv
  e <- dyn $ ffor b $ \b' ->
    if b'
       then elTxt =<< (\d -> findEl d id) =<< currentDocument
          -- mdoc :: Maybe G.Document <- currentDocument
          -- t <- elTxt =<< findEl mdoc id
       else return "empty"
  return e

findEl :: (G.MonadJSM m, G.IsNonElementParentNode self, G.ToJSString elementId) =>
     Maybe self -> elementId -> m (Maybe G.Element)
findEl Nothing _ = return Nothing
findEl (Just d) id = getElementById d id

docTxt Nothing = "Document not found."
docTxt (Just d) = "Document found."

elTxt :: (IsString a, G.MonadJSM m, G.IsElement self, G.FromJSString a) =>
     Maybe self -> m a
elTxt Nothing = return "Element not found."
elTxt (Just e) = getTagName e


-- txtContent :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> m ()
txtContent :: MonadWidget t m => Dynamic t Bool -> m ()
txtContent isV = do
  let txt = ffor isV $ \b ->
              if b
                then "Extra div is visible:::"
                else "Extra div is not shown:::"
  elAttr "div" ("id" =: "txtContextId") $ dynText txt
  dyn $ ffor isV
          (\case
            True -> el "div" $ text "is visible"
            False -> blank
            -- False -> el "div" $ text "not shown"
          )
  el "div" $ text ":::"


-- This widget takes the input value of visibility
-- and creates a view based on that
textWithDynamicVisibility :: (DomBuilder t m, PostBuild t m) =>
-- textWithDynamicVisibility :: MonadWidget t m =>
  Dynamic t Bool -> Dynamic t Int -> m (Event t ())
textWithDynamicVisibility isVisible nums = do
  -- View Widget to Generate Events
  -- button widget is defined in library, it creates a simple button
  let myAttrs = ffor isVisible
                     (\case
                       True -> ("style" =: "")
                       False -> ("style" =: "display: none;"))
                     -- Map Text Text is required by elDynAttr
      -- let n = tagPromptlyDyn (value nums) evToggle
      -- dynText $ t
  let nVal = numValue <$> nums -- nVal is now Dynamic t T.Text
      -- btxt = b2Txt <$> isVisible
      nbt = bnumTxt <$> isVisible <*> nums
  elDynAttr "div" myAttrs $ do
    text "ref-text "
    display nums
    el "br" blank
    dynText nVal -- ok
    -- display nVal -- this has extra "-chars.
    -- text nVal -- does not work

  -- evToggle <- button "Toggle"
  evToggle <- toggleButton isVisible nbt
  -- evToggle <- toButton2 isVisible "btn label" -- this doesn't compile
  -- with (DomBuilder and PostBuild) and the reason seems to be the wrong type.
  -- But it compiles with MonadWidget.
  return evToggle


numValue :: Int -> T.Text
numValue i = "nums val is " <> (T.pack . show) i

b2Txt :: Bool -> T.Text
b2Txt True = "Toggle on"
b2Txt False = "Toggle off"

bnumTxt :: Bool -> Int -> T.Text
bnumTxt True i = "Toggle on " <> (T.pack . show) i
bnumTxt False i = "Toggle off " <> (T.pack . show) i

--------------------------------------------------------------------------------

-- toggleButton :: MonadWidget t m
--             => Dynamic t Bool -- ^ enable or disable button
--             -> Dynamic t T.Text  -- ^ Label
--             -> m (Event t ())
-- If we use the above signature, we get "couldn't match type
-- ‘DomBuilderSpace m’ with ‘GhcjsDomSpace’..." above when there are DomBuilder
-- and PostBuild constraints. With MonadWidget, it compiles. Why it doens't work
-- with the above signature?
-- The following works:
toggleButton :: (Reflex t, PostBuild t1 m, DomBuilder t1 m,
      HasDomEvent t (Element EventResult (DomBuilderSpace m) t1) 'ClickTag)
  => Dynamic t1 a -> Dynamic t1 T.Text -> m (Event t ())
toggleButton b label = do
  (btn, _) <- el' "button" $ dynText label
  pure $ traceEvent "toggleButton event" $ domEvent Click btn
  -- pure $ domEvent Click btn

--------------------------------------------------------------------------------

toButton2 :: MonadWidget t m
            => Dynamic t Bool -- ^ enable or disable button
            -> T.Text         -- ^ Label
            -> m (Event t ())
toButton2 b label = do
  let attrs = ffor b $ \e -> monoidGuard (not e) $ "disabled" =: "disabled"
  (btn, _) <- elDynAttr' "button" attrs $ text label
  pure $ domEvent Click btn


--------------------------------------------------------------------------------

-- | A little helper function for data types in the *Monoid* type class:
-- If the boolean is True, return the first parameter, else return the null or
-- empty element of the monoid
monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty

