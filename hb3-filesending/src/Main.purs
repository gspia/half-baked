module Main where

{-
This is based on 
https://github.com/gspia/half-baked/tree/master/hb2-fileinputs

Idea shortly: 
Get a file list, and process each file (send it to the server). Our server wants
three things: a start message with file size, then name and last is the file.

Questions / TBD:
- how cancel loading?
- progress reporting?
- large files? 
- large number of files?
- how to test different network conditions?

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

import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE, log) as MAC
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log) as MEC
import Control.Monad.Eff.Class (liftEff) as MEC
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (nowDateTime, NOW)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Except as CME
import Control.Monad.Free.Trans (FreeT)

import Data.Array (catMaybes)
import Data.ArrayBuffer.Types as DABT
-- import Data.ArrayBuffer.Show as DABS
import Data.Int (floor)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (for_)
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

-- import DOM.Websocket.BinaryType as DWBT
import DOM.Websocket.Event.CloseEvent as CE
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket as WS

import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

type AppEffects = HalogenEffects 
  ( console :: MEC.CONSOLE
  , dom :: DOM
  , exception :: EXCEPTION
  , ref :: REF
  , avar :: AVAR
  , now :: NOW
  )
type App = Aff AppEffects

--------------------------------------------------------------------------------

data Query a
  = FileCh a 
  | UpLoadReady DET.Event (H.SubscribeStatus -> a)

-- FileDesc contains filereader, one for each file that is read, and the files
-- name and size. The filereader has onload-event, which fires when the file has
-- been loaded and then we start sending the file to the server.
type FileDesc = {freader:: DFT.FileReader, fname :: String, fsize :: Int}
type FRList = DL.List FileDesc

type State = { fileRs :: FRList 
             , bstrs :: DL.List DABT.ArrayBuffer }

data Message = 
  ABMsg DABT.ArrayBuffer
  | Fname String
  | AMsg String

ref :: H.RefLabel
ref = H.RefLabel "myRef"

comp :: H.Component HH.HTML Query Unit Message App
comp = H.component
  { initialState: const {fileRs: DL.Nil, bstrs: DL.Nil}
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
    ] <> (map strShow $ DL.toUnfoldable (DL.zip st.bstrs $ DL.(..) 1 (DL.length st.bstrs)))
      where
        strShow (Tuple t i) = Just $ Tuple ("some str, item=" <> show i) $ 
                HH.div_ [ HH.hr_ , HH.text "a file..."
                        -- $ DABS.showImpl $ unsafeCoerce t :: DABT.ArrayBuffer
                        -- hmm, how to show contents of ArrayBuffer or how
                        -- to coerce it to something that can be shown?
                        ]
  
  eval :: Query ~> H.ComponentDSL State Query Message App
  eval = case _ of
    FileCh next -> do  -- the input element (control) launches this action
      H.liftEff $ MEC.log "FileCh, 1. line, next ref"
      findRef -- this calls fileInfo and fileRead, defined below
      -- Is there other ways to find out the HTML element that originated this
      -- event / is this doable in a simpler way?
      H.liftEff $ MEC.log "FileCh, last line, after ref"
      pure next
    UpLoadReady e reply -> do 
      -- The event handler listening the loadend events calls this action.
      -- We use the subscribe-method attached to the file reader, so we 
      -- have to tell to it to stop listening (by H.Done at the end).
      H.liftEff $ MEC.log "UpLoadReady, 1. line"
      st <- H.get
      Tuple ready notready <- H.liftEff $ 
        splitFRs (DL.length st.fileRs) st.fileRs (Tuple DL.Nil DL.Nil)
      let len = DL.length ready
      H.liftEff $ MEC.log $ "UpLoadReady, ready len = " <> show len
      -- Do something for each file that is in ready-list:
      _ <- DT.for ready \{freader:fr, fname:fnm, fsize:fsz} -> do
        fres <- H.liftEff $ DFFR.result fr -- this returns a foreign result
        H.liftEff $ MEC.log $ "UpLoadReady, after result"
        -- here we should check if the file is ok 
        -- see: https://developer.mozilla.org/en-US/docs/Web/API/FileReader
        -- and https://developer.mozilla.org/en-US/docs/Web/API/FileReader/readAsText
        -- and https://developer.mozilla.org/en-US/docs/Web/Events/loadend
        let btxt = unsafeCoerce fres :: DABT.ArrayBuffer 
        -- We have to coerce the foreign value.
        -- The result type of coerce depends on the reading function below.
        H.modify (_ { bstrs = DL.Cons btxt st.bstrs}) -- store the file contents
        H.liftEff $ MEC.log $ "UpLoadReady, next sending files"
        H.raise $ AMsg $ "File coming:" <> show fsz
        H.raise $ Fname fnm
        H.raise $ ABMsg btxt
      H.modify (_ { fileRs = notready}) -- files still in the reading process
      pure (reply H.Done)

  fileRead :: Maybe DFT.FileList -> H.ComponentDSL State Query Message App Unit
  fileRead Nothing = H.liftEff $ MEC.log "fileRead, no files"
  fileRead (Just flst) = do
    let len = DFFL.length flst
    H.liftEff $ MEC.log $ "fileRead, found " <> show len <> " files."
    pFiles (len - 1)  -- hmm, can we turn this to a map?
      where
        pFiles 0 = fRead (DFFL.item 0 flst)
        pFiles i = 
          if i>0
            then fRead (DFFL.item i flst) <* pFiles (i-1)
            else pure unit
        fRead Nothing = H.liftEff $ MEC.log $ "File not found (fRead)"
        fRead (Just f) = do
          H.liftEff $ MEC.log $ "fRead, processing a file " <> DFF.name f
          let b = DFT.fileToBlob f
              fnm = DFF.name f
              fsz = floor $ DFF.size f
          fr <- H.liftEff DFFR.fileReader
          st <- H.get
          H.modify (_ { fileRs = DL.Cons {freader:fr, fname:fnm, fsize: fsz} st.fileRs})
          H.subscribe $ ES.eventSource' 
            -- Actual event listener with eventTarget, one for each file.
            (onFR $ DFT.fileReaderToEventTarget fr) 
            -- The action to take on event.
            (Just <<< H.request <<< UpLoadReady)    
          -- Next we give the blob (file) to the filereader and ask it to start
          -- loading. Note that blob has size property but not name.
          H.liftEff $ DFFR.readAsArrayBuffer b fr
          -- H.liftEff $ DFFR.readAsText b fr
          H.liftEff $ MEC.log $ "fRead, readAs.. just called."

  fileInfo :: Maybe DFT.FileList -> H.ComponentDSL State Query Message App Unit
  fileInfo Nothing = H.liftEff $ MEC.log "fileInfo, no files"
  fileInfo (Just flst) = do
    let len = DFFL.length flst
    H.liftEff $ MEC.log $ "fileInfo, found " <> show len <> " files."
    pFiles (len - 1)  -- hmm, can we turn this to a map?
      where
        pFiles 0 = fInfo (DFFL.item 0 flst)
        pFiles i = 
          if i>0
            then fInfo (DFFL.item i flst) <* pFiles (i-1)
            else pure unit
        fInfo Nothing = H.liftEff $ MEC.log $ "File not found"
        fInfo (Just f) = H.liftEff $ MEC.log $ "File " 
          <> DFF.name f <> " has " 
          <> show (DFF.size f) <> " bytes."

  findRef :: H.ComponentDSL State Query Message App Unit
  findRef = getHTMLInputRef ref >>= case _ of
      Nothing -> H.liftEff $ MEC.log "could not find ref"
      Just el -> do
        H.liftEff $ MEC.log "found ref (input)"
        let e = DHT.htmlInputElementToHTMLElement el 
        n <- H.liftEff $ DHIE.name el
        H.liftEff $ MEC.log $ "el.name=" <> n
        mfl <- H.liftEff $ DHIE.files el -- Maybe FileList
        fileInfo mfl     -- output some information of the files
        fileRead mfl  
        -- read the files, set the event listener for loaded files etc.


--------------------------------------------------------------------------------
-- 

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
    -- Just {head: (Tuple h fnm), tail: t} -> do
    Just {head: h@{freader:hr, fname:fnm, fsize:fsz}, tail: t} -> do
         let r = fst rnr
             nr = snd rnr
         rs <- H.liftEff $ DFFR.readyState hr
         if rs == DFFRRS.DONE
            then splitFRs (i-1) t (Tuple (DL.Cons h r) nr) 
            else splitFRs (i-1) t (Tuple r (DL.Cons h nr)) 

-- See 
-- https://pursuit.purescript.org/packages/purescript-halogen/2.1.0/docs/Halogen.Query#v:getHTMLElementRef
-- And its source, getHTMLInputRef is similar:
getHTMLInputRef :: forall s f g p o m. HQI.RefLabel 
  -> HQH.HalogenM s f g p o m (Maybe DHT.HTMLInputElement)
getHTMLInputRef = map (go =<< _) <<< HQH.getRef
  where
  go :: DF.Foreign -> Maybe DHT.HTMLInputElement
  go = DE.either (const Nothing) Just <<< CME.runExcept <<< DHT.readHTMLInputElement 


-- on FileReader event, used by subscribe and eventSource above
onFR :: DET.EventTarget -> (DET.Event -> Eff AppEffects Unit)
        -> Eff AppEffects (Eff AppEffects Unit)
onFR et callback = do
  -- Create an EventListener that will log a message and pass the event to the callback
  let listener =
        DEET.eventListener (\event -> do
          H.liftEff $ MEC.log "onFR"
          callback event)
  -- Add the EventListener to the element so it will fire when file is read
  DEET.addEventListener
    EventTypes.loadend 
    -- load operation is completed (either in success or failure)
    -- EventTypes.load -- does load-event work?
    -- See https://developer.mozilla.org/en-US/docs/Web/Events/loadend
    -- and https://developer.mozilla.org/en-US/docs/Web/API/FileReader
    listener true et
  -- Return the function that will be launched when the event listener is removed?
  pure $ DEET.removeEventListener
    EventTypes.loadend
    -- EventTypes.load -- does load-event work?
    listener true et

--------------------------------------------------------------------------------

-- This sends the messages to the server. This is based on Halogen-websockets
-- example. 
-- https://github.com/slamdata/purescript-halogen/tree/master/examples/driver-websockets
-- Our component comp send messages (raises them) and this will deliver them to
-- the server.
wsSender
  :: forall m t eff.
  Monad m => MonadAff
    ( console :: MAC.CONSOLE
    , dom :: DOM
    | eff) m
      => WS.WebSocket -> FreeT (CR.Await Message) m t
wsSender socket = CR.consumer \msg -> do
  H.liftAff $ MAC.log ("wsSender, sending a message")
  case msg of
    AMsg str -> MEC.liftEff $ WS.sendString socket str
    Fname str -> MEC.liftEff $ WS.sendString socket str
    ABMsg (abfr) -> MEC.liftEff $ WS.sendArrayBuffer socket (abfr)
  pure Nothing

--------------------------------------------------------------------------------
    
main :: Eff AppEffects Unit
main = do 
  connection <- WS.create (WS.URL "ws://localhost:8000/msgHandler") []
  u <- WS.url connection
  H.liftEff $ MEC.log ("main, connection url=" <> u)
  runHalogenAff $ do
    body <- awaitBody
    io <- runUI comp unit body
    io.subscribe $ wsSender connection -- from this client to the server
    -- Connecting the consumer to the producer initializes both, feeding
    -- queries back to our component as messages are received from server.
    CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query)


--------------------------------------------------------------------------------

-- This reads the messages from the server. This is based on Halogen-websockets
-- example and one other source (which link?)
-- https://github.com/slamdata/purescript-halogen/tree/master/examples/driver-websockets
wsProducer
  :: forall eff
   . WS.WebSocket
  -> CR.Producer String
    (Aff
      ( avar :: AVAR
      , exception :: EXCEPTION
      , dom :: DOM
      , console :: MEC.CONSOLE
      , now :: NOW
      , ref :: REF
      | eff)) Unit
wsProducer socket = CRA.produce \emit -> do
  DEET.addEventListener WSET.onMessage (listenerM emit) false (WS.socketToEventTarget socket)
  DEET.addEventListener WSET.onClose (listenerC emit) false (WS.socketToEventTarget socket)
  DEET.addEventListener WSET.onOpen (listenerO emit) false (WS.socketToEventTarget socket)
  DEET.addEventListener WSET.onError (listenerE emit) false (WS.socketToEventTarget socket)
  where
  -- onMessage
  listenerM emit = DEET.eventListener \ev -> do
    for_ (readHelper WS.readMessageEvent ev) \msgEvent ->
      for_ (readHelper DF.readString (ME.data_ msgEvent)) \msg ->
        emit (DE.Left msg)
  -- onClose
  listenerC emit = DEET.eventListener \ev ->
    for_ (readHelper WS.readCloseEvent ev) \clsEvent -> do
      let rstr = readHelper DF.readString (CE.reason clsEvent)
          rint = readHelper DF.readInt (CE.code clsEvent)
          rbo  = readHelper DF.readBoolean (CE.wasClean clsEvent)
          s = case rstr of
                Nothing -> "empty str"
                Just st -> ":" <> st <> ":"
          r = case rint of
                Nothing -> " and no int"
                Just i  -> " int=" <> show i
          b = case rbo of
                Nothing -> " and no bool"
                Just i  -> " bool=" <> show i
      MEC.liftEff $ MEC.log ("onClose-ev, listener2, msg=" <> s <> r <> b)
      n <- H.liftEff nowDateTime
      emit (DE.Left $ ("onClose-ev" <> s <> r <> b <> show n))
  -- onOpen
  listenerO emit = DEET.eventListener \ev -> do
    MEC.liftEff $ MEC.log ("onOpen-ev, listener3")
    n <- H.liftEff nowDateTime
    emit (DE.Left $ "onOpen-ev at " <> show n)
  -- onError
  listenerE emit = DEET.eventListener \ev -> do
    MEC.liftEff $ MEC.log ("onError-ev, listener4")
    emit (DE.Left ("onError-ev, listener4"))
  readHelper :: forall a b. (DF.Foreign -> DF.F a) -> b -> Maybe a
  readHelper read = do
    DE.either (const Nothing) Just <<< CME.runExcept <<< read <<< DF.toForeign


-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends queries in when it receives inputs from the producer.
wsConsumer
  :: forall eff
   . (Query ~> Aff (HalogenEffects (console :: MEC.CONSOLE | eff)))
  -> CR.Consumer String (Aff (HalogenEffects (console :: MEC.CONSOLE | eff))) Unit
wsConsumer query = CR.consumer \msg -> do
  H.liftEff $ MEC.log ("Consumer (for messages from server), msg=" <> show msg)
  pure Nothing

