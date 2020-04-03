{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.Chan.Lifted (newChan, readChan, writeChan)
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Exception (AsyncException, handle)
import qualified Data.Map as M
import qualified Data.UUID as U
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Jupyter
import Jupyter.Types.Common
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import Web.Scotty
import Prelude hiding (get)

waitKernels :: Map UUID KernelControl -> IO ()
waitKernels kernels =
  for_ kernels $ \kCtrl ->
    readMVar (_done kCtrl)

destruct :: Map UUID KernelControl -> IO ()
destruct kernels = do
  putStrLn' "destructing..."
  for_ kernels $ \kCtrl ->
    putMVar (_destruct kCtrl) ()
  waitKernels kernels
  putStrLn' "destructed"

test :: IO ()
test = do
  kernels <- newMVar mempty :: IO (MVar (Map UUID KernelControl))
  handle @AsyncException (\_ -> destruct =<< readMVar kernels) $ do
    u <- nextJust (liftIO nextUUID)
    k <- KernelControl <$> newChan <*> newChan <*> newEmptyMVar <*> newEmptyMVar
    modifyMVar_ kernels (pure . M.insert u k)
    void $ fork (runKernelName "python3" k)
    writeChan (_in k) (KernelExecute $ unlines ["print('yolo')", "2 + 2"])
    -- writeChan (_in k) (KernelExecute $ unlines ["print 'yolo'", "2 + 2"])
    print' =<< readChan (_out k)
    print' =<< readChan (_out k)
    print' =<< readChan (_out k)
    writeChan (_in k) KernelShutdown
    waitKernels =<< readMVar kernels

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  kernels <- newMVar mempty :: IO (MVar (Map UUID KernelControl))
  handle @AsyncException (\_ -> destruct =<< readMVar kernels)
    . scotty 8000
    $ do
      middleware
        ( simpleCors
            . staticPolicy (addBase "static")
            . staticPolicy (addBase "frontend")
        )
      get "/" $ file "./frontend/index.html"
      get "/uuid" $ text . fromStrict . U.toText =<< nextJust (liftIO nextUUID)
      get "/new_kernel" $ do
        u <- nextJust (liftIO nextUUID)
        k <- KernelControl <$> newChan <*> newChan <*> newEmptyMVar <*> newEmptyMVar
        modifyMVar_ kernels (pure . M.insert u k)
        void . fork $ runKernelName "python3" k
        text . fromStrict $ U.toText u
