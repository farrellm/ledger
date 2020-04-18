{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.JS where

import Language.Javascript.JSaddle.Types
import Relude

#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle
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

#ifdef ghcjs_HOST_OS
putStrLn' :: (MonadIO m) => String -> m ()
putStrLn' x = void . liftIO $ eval ("console.log('" <> x <> "')")
#else
putStrLn' :: (MonadIO m) => String -> m ()
putStrLn' = putStrLn
#endif

print' :: (Show a, MonadIO m) => a -> m ()
print'  = putStrLn' . show
