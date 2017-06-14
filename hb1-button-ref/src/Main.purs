
{-
This is based on 
https://github.com/slamdata/purescript-halogen/issues/423  

Questions / TBD:
- What the keyed elements are and how to use them?
- When to use reflabels? (Are they meant for this kind of situations?)
- (+ a lot of things about workflows )
- If there is alternative or better way to do effectively the same thing, 
  I'd love to add them here as an example.
- If some constructs are clear antipatterns, I'd love to improve/correct etc.


Motivation for this was to
- read properties of elements/nodes etc.
(- which can be used when doing things with files)


-}

module Main where

import Prelude
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.VDom.Driver (runUI)
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.InputF as HQI
import Halogen.Query.HalogenM as HQH
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except as CME
import Data.Array (catMaybes)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(..))
import Data.Foreign as DF
import Data.Either as DE

import DOM (DOM)
import DOM.HTML.Types as DHT
import DOM.HTML.HTMLElement as DHE
import DOM.Node.Element as DNE
import DOM.HTML.HTMLButtonElement as DHBE

--------------------------------------------------------------------------------

type AppEffects = HalogenEffects (console :: CONSOLE, dom :: DOM)
type App = Aff AppEffects

--------------------------------------------------------------------------------

data Query a
  = Toggle a

type State = Boolean

ref :: H.RefLabel
ref = H.RefLabel "myRef"

ref2 :: H.RefLabel
ref2 = H.RefLabel "myRef2"

comp :: H.Component HH.HTML Query Unit Void App
comp = H.component
  { initialState: const true
  , render
  , eval
  , receiver: const Nothing
  }

  where

  render :: State -> H.ComponentHTML Query
  render st = HK.div_ $ catMaybes $
    [ if st
      then Just $ Tuple "foo" $ HH.text "foo"
      else Nothing
    , Just $ Tuple "reftxt" $ HH.div
      [ HP.ref ref ]
      [ HH.text "Ref-text" ]
    , Just $ Tuple "button" $ HH.button
      [ HP.ref ref2
      , HP.name "button name"
      , HP.title "button title"
      , HE.onClick $ HE.input_ Toggle ]
      [ HH.text "Toggle" ]
    ] 
  
  eval :: Query ~> H.ComponentDSL State Query Void App
  eval = case _ of
    Toggle next -> do -- the toggle button (control) launches this action
      findRef2
      H.modify not
      findRef2
      pure next

  findRef :: H.ComponentDSL State Query Void App Unit
  findRef =
    H.getHTMLElementRef ref >>= case _ of
      Nothing -> H.liftEff $ log "could not find ref"
      Just el -> do
        H.liftEff $ log "found ref"
        let e = DHT.htmlElementToElement el 
        -- What functions there are for Element that is the type of e?
        -- Pursuit htlmElementToElement links to the 
        -- https://pursuit.purescript.org/packages/purescript-dom/4.5.0/docs/DOM.Node.Types#t:Element
        -- But it contains only the types.
        -- Sometimes just cloning the repo and doing a grep or ag to find the
        -- right modules/functions saves time.
        H.liftEff $ log $ "e.tagName=" <> DNE.tagName e

  findRef2 :: H.ComponentDSL State Query Void App Unit
  findRef2 =
    getHTMLButtonRef ref2 >>= case _ of
      Nothing -> H.liftEff $ log "could not find ref2"
      Just el -> do
        H.liftEff $ log "found ref2 (button)"
        n <- H.liftEff $ DHBE.name el
        H.liftEff $ log $ "el.name=" <> n
        let e = DHT.htmlButtonElementToHTMLElement el 
        str <- H.liftEff $ DHE.title e
        H.liftEff $ log $ "e.title=" <> str


-- See https://pursuit.purescript.org/packages/purescript-halogen/2.1.0/docs/Halogen.Query#v:getHTMLElementRef
-- And its source, getHTMLButtonRef is similar:
getHTMLButtonRef :: forall s f g p o m. HQI.RefLabel 
  -> HQH.HalogenM s f g p o m (Maybe DHT.HTMLButtonElement)
getHTMLButtonRef = map (go =<< _) <<< HQH.getRef
  where
  go :: DF.Foreign -> Maybe DHT.HTMLButtonElement
  go = DE.either (const Nothing) Just <<< CME.runExcept <<< DHT.readHTMLButtonElement 
  -- readHTMLElement

--------------------------------------------------------------------------------
    
main :: Eff AppEffects Unit
main = runHalogenAff $ do
  body <- awaitBody
  runUI comp unit body

