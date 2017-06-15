module Main where

{-
This is based on 
https://github.com/slamdata/purescript-halogen/issues/423  
and 
https://github.com/gspia/half-baked/tree/master/hb1-button-ref

Idea shortly: tie file input onChange event to FileCf-action, which in turn,
launches the file reading (in to the client) together with a listener waiting
for loadend event.  Listener launches UpLoadReady-action that uses the files.
User can select several files at the fileinput-dialog.

Questions / TBD:
- Is this the way to go? 
- If yes, a better api and packing as re-useable component?
- E.g., it would be convenient if we could just replace the onChange-line with 
  a call to a function that takes care of the most of the things done here.
  That call could take three/four actions as input, one for read and ok files,
  one for ready and not ok files, one for progress handling, and one for all
  done. Is this possible so that subscribing and unsubscribing would be done
  behing the scenes (=so that user is not required to return Done)?
- Some of the loops probably could be done as maps (more clearly)
- Is it possible that one reader launches the UpLoadReady-action, a second too,
  so that on the first UpLoadReady-action will handle both files? In this case
  the second UpLoadReady-action would only unsubscribe the listener. How to test
  this?
- At some point, we have a FileList and use its items one-by-one (see fileInfo
  and fileRead). Maybe turn this to a traversable and then use maps?
- This does not work in the case of halts (interrupting the reading), TBD.
- Try readAsArrayBuffer TBD
- Showing reading progress to user in case of large files TBD

Motivation for this was/is to
- load files to client 
- and from client to server (TBD)
   - with websockets (TBD)
   - with xhr (TBD)

-}

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
import Halogen.Query.EventSource  as ES
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except as CME
import Data.Array (catMaybes)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foreign as DF
import Data.Either as DE
import Data.List as DL
import Data.Traversable as DT

import DOM (DOM)
import DOM.File.FileList as DFFL
import DOM.File.File as DFF
import DOM.File.FileReader as DFFR
import DOM.File.FileReader.ReadyState as DFFRRS
import DOM.File.Types as DFT
import DOM.HTML.Types as DHT
import DOM.HTML.HTMLInputElement as DHIE
import DOM.HTML.Event.EventTypes as EventTypes
import DOM.Event.Types as DET
import DOM.Event.EventTarget as DEET

import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

type AppEffects = HalogenEffects (console :: CONSOLE, dom :: DOM)
type App = Aff AppEffects

--------------------------------------------------------------------------------

data Query a
  = FileCh a 
  | UpLoadReady DET.Event (H.SubscribeStatus -> a)

type State = { fileR :: DL.List DFT.FileReader
             , strs :: DL.List String}


ref :: H.RefLabel
ref = H.RefLabel "myRef"

comp :: H.Component HH.HTML Query Unit Void App
comp = H.component
  { initialState: const {fileR: DL.Nil, strs: DL.Nil}
  , render
  , eval
  , receiver: const Nothing
  }

  where

  render :: State -> H.ComponentHTML Query
  render st = HK.div_ $ catMaybes $
    [ Just $ Tuple "fileinput" $ HH.input
      [ HP.ref ref
      , HP.type_ HP.InputFile
      , HP.name "fileinput element"
      , HP.multiple true -- user can select several files at the same time
      , HP.id_ "fileinputId"
      , HE.onChange $ HE.input_ FileCh
      ]
    ] <> (map strShow $ DL.toUnfoldable (DL.zip st.strs $ DL.(..) 1 (DL.length st.strs)))
      where
        strShow (Tuple t i) = Just $ Tuple ("some str, item=" <> show i) $ 
                HH.div_ [ HH.hr_ , HH.text t]
  
  eval :: Query ~> H.ComponentDSL State Query Void App
  eval = case _ of
    FileCh next -> do  -- the input element (control) launches this action
      H.liftEff $ log "FileCh, 1. line, next ref"
      findRef -- this calls fileInfo and fileRead, defined below
      -- Is there other ways to find out the HTML element that originated this
      -- event / is this doable in a simpler way?
      H.liftEff $ log "FileCh, last line, after ref"
      pure next
    UpLoadReady e reply -> do 
      -- The event handler listening the loadend events calls this action.
      -- We use the subscribe-method attached to the file reader, so we 
      -- have to tell to it to stop listening (by H.Done at the end).
      H.liftEff $ log "UpLoadReady, 1. line"
      st <- H.get
      Tuple ready notready <- H.liftEff $ 
        splitFRs (DL.length st.fileR) st.fileR (Tuple DL.Nil DL.Nil)
      {-- let len = DL.length ready --}
      {-- H.liftEff $ log $ "UpLoadReady, ready len = " <> show len --}
      -- Do something for each file that is in ready-list.
      _ <- DT.for ready \i -> do
        fres <- H.liftEff $ DFFR.result i     -- to get the contents, get the foreign result
        -- here we should check if the file is ok 
        -- see: https://developer.mozilla.org/en-US/docs/Web/API/FileReader
        -- and https://developer.mozilla.org/en-US/docs/Web/API/FileReader/readAsText
        -- and https://developer.mozilla.org/en-US/docs/Web/Events/loadend
        let txt = unsafeCoerce fres :: String -- which we have to coerce
        -- The result type of coerce depends on the reading function, we use
        -- readAsText below -> this is a String.
        H.liftEff $ log $ "\n\nand\n\n txt=" <> txt
        H.modify (_ { strs = DL.Cons txt st.strs}) -- store the file contents
      H.modify (_ { fileR = notready}) -- files still in the reading process
      pure (reply H.Done)
   

  fileRead :: DHT.HTMLInputElement -> Maybe DFT.FileList -> H.ComponentDSL State Query Void App Unit
  fileRead _ Nothing = H.liftEff $ log "fileRead, no files"
  fileRead el (Just flst) = do
    let len = DFFL.length flst
    H.liftEff $ log $ "fileRead, found " <> show len <> " files."
    pFiles (len - 1)  -- hmm, can we turn this to a map?
      where
        pFiles 0 = fRead (DFFL.item 0 flst)
        pFiles i = 
          if i>0
            then fRead (DFFL.item i flst) <* pFiles (i-1)
            else pure unit
        fRead Nothing = H.liftEff $ log $ "File not found (fRead)"
        fRead (Just f) = do
          H.liftEff $ log $ "fRead, processing a file " <> DFF.name f
          let b = DFT.fileToBlob f
          fr <- H.liftEff DFFR.fileReader
          st <- H.get
          H.modify (_ { fileR = DL.snoc st.fileR fr})
          H.subscribe $ ES.eventSource' 
            (onFR $ DFT.fileReaderToEventTarget fr) -- Actual event listener
            -- with eventTarget. 
            (Just <<< H.request <<< UpLoadReady)    -- The action to take on event.
          -- H.liftEff $ DFFR.readAsArrayBuffer b fr
          H.liftEff $ DFFR.readAsText b fr
          H.liftEff $ log $ "fRead, readAs.. just called."

  fileInfo :: Maybe DFT.FileList -> H.ComponentDSL State Query Void App Unit
  fileInfo Nothing = H.liftEff $ log "fileInfo, no files"
  fileInfo (Just flst) = do
    let len = DFFL.length flst
    H.liftEff $ log $ "fileInfo, found " <> show len <> " files."
    pFiles (len - 1)  -- hmm, can we turn this to a map?
      where
        pFiles 0 = fInfo (DFFL.item 0 flst)
        pFiles i = 
          if i>0
            then fInfo (DFFL.item i flst) <* pFiles (i-1)
            else pure unit
        fInfo Nothing = H.liftEff $ log $ "File not found"
        fInfo (Just f) = H.liftEff $ log $ "File " 
          <> DFF.name f <> " has " 
          <> show (DFF.size f) <> " bytes."

  findRef :: H.ComponentDSL State Query Void App Unit
  findRef = getHTMLInputRef ref >>= case _ of
      Nothing -> H.liftEff $ log "could not find ref"
      Just el -> do
        H.liftEff $ log "found ref (input)"
        let e = DHT.htmlInputElementToHTMLElement el 
        n <- H.liftEff $ DHIE.name el
        H.liftEff $ log $ "el.name=" <> n
        mfl <- H.liftEff $ DHIE.files el -- Maybe FileList
        fileInfo mfl     -- output some information of the files
        fileRead el mfl  
        -- read the files, set the event listener for loaded files etc.


-- to make the splitFRs's type shorter/more clear
type FRList = DL.List DFT.FileReader

-- This splits the filereader list to those readers that are ready (done) and to
-- those that are still loading or empty. Is this doable with a foldable?
-- Note that "readers done" may not have been able to read the files.
-- See https://developer.mozilla.org/en-US/docs/Web/API/FileReader/readyState
splitFRs :: forall m. 
  Int 
  -> FRList
  -> Tuple FRList FRList
  -> Eff (dom :: DOM | m) (Tuple FRList FRList)
splitFRs 0 frs res = pure res
splitFRs i frs rnr = do
  case DL.uncons frs of
    Nothing -> pure rnr
    Just {head: h, tail: t} -> do
         let r = fst rnr
             nr = snd rnr
         rs <- H.liftEff $ DFFR.readyState h
         if rs == DFFRRS.DONE
            then splitFRs (i-1) t (Tuple (DL.Cons h r) nr) 
            else splitFRs (i-1) t (Tuple r (DL.Cons h nr)) 
   

-- See https://pursuit.purescript.org/packages/purescript-halogen/2.1.0/docs/Halogen.Query#v:getHTMLElementRef
-- And its source, getHTMLInputRef is similar:
getHTMLInputRef :: forall s f g p o m. HQI.RefLabel 
  -> HQH.HalogenM s f g p o m (Maybe DHT.HTMLInputElement)
getHTMLInputRef = map (go =<< _) <<< HQH.getRef
  where
  go :: DF.Foreign -> Maybe DHT.HTMLInputElement
  go = DE.either (const Nothing) Just <<< CME.runExcept <<< DHT.readHTMLInputElement 


-- on FileReader event, used by subscribe and eventSource above
onFR :: DET.EventTarget
        -> (DET.Event -> Eff AppEffects Unit)
        -> Eff AppEffects (Eff AppEffects Unit)
onFR et callback = do
  -- Create an EventListener that will log a message and pass the event to the callback
  let listener =
        DEET.eventListener (\event -> do
          H.liftEff $ log "onFR"
          callback event)
  -- Add the EventListener to the element so it will fire when file is read
  DEET.addEventListener
    EventTypes.loadend -- load operation is completed (either in success or failure)
    -- EventTypes.load -- does load-event work?
    listener true et
  -- Return the function that will be launched when the event listener is removed?
  pure $ DEET.removeEventListener
    EventTypes.loadend
    -- EventTypes.load -- does load-event work?
    listener true et


--------------------------------------------------------------------------------
    
main :: Eff AppEffects Unit
main = runHalogenAff $ do
  body <- awaitBody
  runUI comp unit body

