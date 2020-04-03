{-# LANGUAGE OverloadedStrings #-}

module Prelude
  ( module Relude,
    module Common,
    firstJust,
    nextJust,
    print',
    putStrLn',
  )
where

import Common
import Control.Concurrent.MVar.Lifted (withMVar)
import Control.Monad.Trans.Control (MonadBaseControl)
import Relude
import System.IO.Unsafe (unsafePerformIO)

firstJust :: (Monad m) => [m (Maybe a)] -> m a
firstJust [] = error "no Just found!"
firstJust (x : xs) = maybe (firstJust xs) pure =<< x

nextJust :: (Monad m) => m (Maybe a) -> m a
nextJust x = maybe (nextJust x) pure =<< x

baton :: MVar ()
baton = unsafePerformIO $ newMVar ()
{-# NOINLINE baton #-}

print' :: (MonadBaseControl IO m, MonadIO m, Show a) => a -> m ()
print' a = withMVar baton $ \() -> print a

putStrLn' :: (MonadBaseControl IO m, MonadIO m) => String -> m ()
putStrLn' a = withMVar baton $ \() -> putStrLn a
