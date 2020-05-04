{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.Chan.Lifted (newChan, readChan, writeChan)
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Exception (AsyncException, handle)
import Control.Lens
import Data.Aeson hiding (json)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Map as M
import Data.Traversable (for)
import qualified Data.UUID as U
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Jupyter
import Jupyter.Types.Common
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import Relude.Extra.Map ((!?))
import System.Directory
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

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  kernels <- newMVar mempty :: IO (MVar (Map UUID KernelControl))
  ledgers <- newMVar mempty :: IO (MVar (Map FilePath UUID))
  handle @AsyncException (\_ -> destruct =<< readMVar kernels)
    . scotty 8000
    $ do
      middleware
        ( simpleCors
            . staticPolicy (addBase "static")
            . staticPolicy (addBase "frontend")
        )
      --
      get "/" $ file "./frontend/index.html"
      --
      get "/new_kernel" $
        newKernel kernels >>= json
      --
      post "/get_kernel" $ do
        putStrLn' "get_kernel:"
        l <- jsonData
        putStrLn' l
        (!? l) <$> readMVar ledgers >>= \case
          Just u -> (!? u) <$> readMVar kernels >>= \case
            Just k -> writeChan (_in k) KernelShutdown
            Nothing -> pass
          Nothing -> pass
        u <- newKernel kernels
        modifyMVar_ ledgers (pure . M.insert l u)
        json u
      --
      post "/execute" $ do
        putStrLn' "execute:"
        d <- jsonData
        print' (d :: ExecuteRequest)
        M.lookup (d ^. executeRequest_kernelUUID) <$> readMVar kernels >>= \case
          Nothing -> pass
          Just k ->
            writeChan (_in k) $
              KernelExecute
                (d ^. executeRequest_cellUUID)
                (d ^. executeRequest_cellCode)
        json (d ^. executeRequest_cellUUID)
      --
      post "/result" $ do
        putStrLn' "result:"
        d <- jsonData
        print' (d :: ResultRequest)
        M.lookup (d ^. resultRequest_kernelUUID) <$> readMVar kernels >>= \case
          Nothing -> do
            putStrLn' "kernel missing"
            json (U.nil, KernelMissing $ d ^. resultRequest_kernelUUID)
          Just k -> do
            putStrLn' "waiting for result"
            r <- readChan (_out k)
            print' r
            json r
      --
      get "/home" $ liftIO getHomeDirectory >>= json
      --
      post "/save" $ do
        putStrLn' "save:"
        (p, l) <- jsonData
        putStrLn' p
        writeFileLBS p $ encodePretty @LedgerSave l
      --
      post "/load" $ do
        putStrLn' "load:"
        p <- jsonData
        putStrLn p
        x <- liftIO $ do
          d <- doesPathExist p
          if d
            then eitherDecodeFileStrict' p
            else
              pure $
                Right
                  LedgerSave
                    { _ledgerSave_label = mempty,
                      _ledgerSave_badLabel = mempty,
                      _ledgerSave_code = []
                    }
        json x
      --
      post "/list" $ do
        putStrLn' "list:"
        r <- jsonData
        putStrLn' r
        xs <- liftIO $ do
          ps <- listDirectory r
          for ps $ \p -> do
            d <- doesDirectoryExist p
            if d
              then pure $ LedgerDirectory p
              else pure $ LedgerFile p
        json xs
  where
    newKernel kernels = do
      u <- nextJust (liftIO nextUUID)
      k <- KernelControl <$> newChan <*> newChan <*> newEmptyMVar <*> newEmptyMVar
      modifyMVar_ kernels (pure . M.insert u k)
      void . fork $ runKernelName "python3" k
      pure u
