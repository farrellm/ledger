{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.JS where

import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Types (JSM, liftJSM)
import Relude

#ifdef ghcjs_HOST_OS
#else
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
    setTimeout,
  )
import Network.Wai.Middleware.Static
import Language.Javascript.JSaddle.WebSockets
import Network.WebSockets (defaultConnectionOptions)
#endif

#ifdef ghcjs_HOST_OS
runLedger :: Int -> IO () -> IO ()
runLedger _port = id
#else
runLedger :: Int -> JSM () -> IO ()
runLedger port f =
  runSettings (defaultSettings & setPort port & setTimeout 3600)
    =<< jsaddleOr defaultConnectionOptions (f >> syncPoint) ledgerApp
  where
    ledgerApp = staticPolicy (addBase "static") jsaddleApp
#endif
