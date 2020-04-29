{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.JS where

import Language.Javascript.JSaddle.Types
import Ledger.Prelude

#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle
#else
import Control.Concurrent.MVar (withMVar)
import Language.Javascript.JSaddle.WebSockets
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setPort,
    setTimeout,
  )
import Network.WebSockets (defaultConnectionOptions)
import System.IO.Unsafe (unsafePerformIO)
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

#ifdef ghcjs_HOST_OS
putStrLn' :: (MonadIO m) => String -> m ()
putStrLn' x = void . liftJSM $ global ! "console" # "log" $ [x]
#else
baton :: MVar ()
baton = unsafePerformIO $ newMVar ()
{-# NOINLINE baton #-}

putStrLn' :: (MonadIO m) => String -> m ()
putStrLn' s = liftIO . withMVar baton $ \() -> putStrLn s
#endif

print' :: (Show a, MonadIO m) => a -> m ()
print'  = putStrLn' . show
