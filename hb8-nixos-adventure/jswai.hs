module Main where

...

import Language.Javascript.JSaddle (JSM, liftJSM)
import GHCJS.Types
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Run        (syncPoint)

#ifdef ghcjs_HOST_OS
#else
-- import Language.Javascript.JSaddle.WebKitGTK (run)
import Language.Javascript.JSaddle.Warp as JSW (run)
import Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleWithAppOr)
import Network.Wai                            (Application)
import Network.Wai.Handler.Warp               (defaultSettings, run, runSettings
                                              , setPort, setTimeout)
import Network.WebSockets (defaultConnectionOptions)
import Network.Wai.Application.Static
import WaiAppStatic.Types
#endif

------------------------------------------------------------------------------

main :: IO ()
#ifdef ghcjs_HOST_OS
main = liftJSM mainW
#else
main = JSW.run 8000 $ mainW

-- | A @main@ for doing development.
devMain :: Application -> JSM () -> Int -> IO ()
devMain backend frontend port = do
  putStrLn $ "Running dev server on localhost:" Mon.<> show port
  app <- jsaddleWithAppOr
    defaultConnectionOptions
    (frontend >> syncPoint)
    backend
  runSettings (defaultSettings & setTimeout 3600 & setPort port) app

-- | A version of @devMain@ that can be used with @ghcid --test@ to get an auto-reloading server.
devMainAutoReload :: Application -> JSM () -> Int -> IO ()
devMainAutoReload backend frontend port =
  debugWrapper $ \refreshMiddleware registerContext ->
    devMain (refreshMiddleware backend) (registerContext >> frontend) port

staticServer :: Application
staticServer = staticApp ((defaultFileServerSettings "./static") & noCache)
  where noCache s = s { ssMaxAge = MaxAgeSeconds 0 }

#endif

------------------------------------------------------------------------------
------------------------------------------------------------------------------

mainW :: JSM ()
mainW = mainWidgetWithHead headEl bodyEl

...
