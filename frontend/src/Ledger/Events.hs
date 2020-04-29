{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Events where

import Common
import Control.Lens hiding ((#))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Traversable (for)
import qualified Data.UUID.Types as U
import Data.UUID.Types (UUID)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Types (liftJSM)
import Ledger.JS
import Ledger.Prelude
import Ledger.Types
import Ledger.Utility
import Reflex.Dom.Core

pollExecute :: (MonadWidget t m) => LedgerState -> m (Event t ResultsUpdate)
pollExecute ls = do
  (evExecute, triggerExecute) <- newTriggerEvent
  fork . forever $ do
    putStrLn' "#### waiting for ready"
    takeMVar (ls ^. ready)
    putStrLn' "#### ready!"
    q <- readChan (ls ^. queue)
    print' q
    case q of
      Nothing -> void $ swapMVar (ls ^. stop) False
      Just (u, c) -> triggerExecute (u, c)
  evExecute' <- performEventAsync $
    evExecute <&> \(u, c) go ->
      fork $ do
        k <- readMVar (ls ^. kernelUUID)
        go $ ExecuteRequest k u c
  evX <-
    performRequestAsync $
      evExecute' <&> postJson' ((ls ^. url) <> ":8000/execute")
  filterDuplicates (evX $> RunningCell)

pollOutput :: (MonadWidget t m) => LedgerState -> m (Event t ResultsUpdate)
pollOutput ls = do
  evPostBuild <- getPostBuild
  (evNewRequest, triggerNewRequest) <- newTriggerEvent
  evNewRequest' <- performEventAsync $
    (evNewRequest <> evPostBuild) <&> \() go ->
      fork $ do
        putStrLn' "read kernel UUID"
        k <- readMVar (ls ^. kernelUUID)
        print' k
        go $ ResultRequest k
  evOutput' <-
    performRequestAsync $
      evNewRequest' <&> postJson' ((ls ^. url) <> ":8000/result")
  evOutput'' <- filterDuplicates $ fmap decodeXhrResponse evOutput'
  evOutput''' <- performEvent $
    evOutput'' <&> \case
      Nothing -> do
        liftIO $ do
          putStrLn' "request fail 2"
          (ls ^. triggerKernelUpdate) DeadKernel
          triggerNewRequest ()
        pure Nothing
      Just (c :: UUID, r :: KernelOutput) -> do
        liftIO $ triggerNewRequest ()
        pure . Just $ Output c r
  pure $ catMaybes evOutput'''

refreshState :: MonadJSM m => LedgerState -> b -> m b
refreshState ls u = do
  withMVar (ls ^. uuids) $ \xs -> do
    cs <- for xs $ \x ->
      liftJSM $ do
        cms <- global ^. js ("cms" :: Text)
        cm <- cms ^. js (show x :: Text)
        c <- fromJSVal =<< cm ^. js0 ("getValue" :: Text)
        pure $ fromMaybe "" c
    writeIORef (ls ^. code) $ M.fromList (zip xs cs)
    for_ xs $ updateParameters ls
  pure u

kernelUpdate :: (MonadIO m) => LedgerState -> (() -> IO ()) -> KernelUpdate -> m ()
kernelUpdate ls triggerNewKernel u =
  case u of
    NewKernel k -> do
      putStrLn "new kernel"
      putMVar (ls ^. kernelUUID) k
      putMVar (ls ^. ready) ()
      writeChan (ls ^. queue) $ Just (U.nil, "__ledger = dict()")
    ShutdownKernel -> pass
    DeadKernel -> do
      putStrLn' "kernel died"
      void $ tryTakeMVar (ls ^. kernelUUID)
    StartKernel -> do
      putStrLn' "start new kernel"
      liftIO $ triggerNewKernel ()

resultsUpdate :: (MonadIO m) => LedgerState -> ResultsUpdate -> m ()
resultsUpdate ls u =
  case u of
    ExecuteCell c -> executeCell ls c
    Output c (KernelResult _ r) ->
      modifyIORef (ls ^. result) (M.insert c r)
    Output c (KernelStdout _ r) ->
      modifyIORef (ls ^. stdout) (M.alter (Just . (<> r) . fromMaybe "") c)
    Output c k@(KernelError _ rs _ _) -> do
      let rs' = filterEsc <$> rs
      print' k
      print' rs'
      modifyIORef (ls ^. error) (M.alter (Just . (<> unlines rs') . fromMaybe "") c)
    Output _ (KernelMissing _) -> do
      putStrLn' "KernelMissing"
      liftIO $ (ls ^. triggerKernelUpdate) DeadKernel
    Output _ (KernelDone _) -> do
      putStrLn' "KernelDone"
      putMVar (ls ^. ready) ()
    UpdateLabel c l -> do
      markDepsDirty ls c
      lbl <- readIORef (ls ^. label)
      let bs = S.fromList . fmap snd . filter ((/= c) . fst) $ M.toList lbl
      modifyIORef (ls ^. badLabel) (S.delete c)
      case l of
        "" -> modifyIORef (ls ^. label) (M.delete c)
        _ -> do
          modifyIORef (ls ^. label) (M.insert c l)
          when (S.member l bs) $ modifyIORef (ls ^. badLabel) (S.insert c)
      updateParameters ls c
    UpdateCode c -> markDirty ls c
    r -> print' r
  where
    filterEsc :: Text -> Text
    filterEsc = toStrict . go . toLazy
      where
        go :: LText -> LText
        go "" = ""
        go t =
          let (a, t') = TL.break (== '\ESC') t
              b = TL.drop 1 $ TL.dropWhile (/= 'm') t'
           in a <> go b

ledgerUpdate :: (MonadIO m) => LedgerState -> LedgerUpdate -> m ()
ledgerUpdate ls u =
  case u of
    AddCellEnd x -> modifyMVar (ls ^. uuids) (<> [x])
    RemoveCell x -> modifyMVar (ls ^. uuids) $ filter (/= x)
    RaiseCell x -> modifyMVar (ls ^. uuids) $ \xs ->
      case break (== x) xs of
        (f, _ : b) -> insertPenultimate x f ++ b
        _ -> xs
    LowerCell x -> modifyMVar (ls ^. uuids) $ \xs ->
      case break (== x) xs of
        (f, _ : y : b) -> f ++ y : x : b
        _ -> xs
    r -> print' r
  where
    insertPenultimate z [] = [z]
    insertPenultimate z [w] = [z, w]
    insertPenultimate z (w : ws) = w : insertPenultimate z ws

executeCell :: (MonadIO m) => LedgerState -> UUID -> m ()
executeCell ls x =
  withMVar (ls ^. stop) $ \s ->
    unless s $
      (!? x) <$> readIORef (ls ^. code) >>= \case
        Just c ->
          (!? x) <$> readIORef (ls ^. parameters) >>= \case
            Just ps -> do
              lbl <- readIORef (ls ^. label)
              bad <- readIORef (ls ^. badLabel)
              let lbl' =
                    M.fromList $
                      swap <$> filter (flip S.notMember bad . fst) (M.toList lbl)
                  n = T.replace "-" "_" (show x)
                  n' = "_" <> n
                  cs' = addReturn $ lines c
                  c' =
                    unlines
                      ( "def " <> n' <> "(" <> T.intercalate ", " (S.toList ps) <> "):"
                          : (("  " <>) <$> cs')
                      )
                  args =
                    T.intercalate ", " $
                      fmap (\p -> p <> "=__ledger['" <> U.toText (lbl' M.! p) <> "']") (S.toList ps)
                  e = case lbl !? x of
                    Just _ -> T.concat ["__ledger['", show x, "'] = ", n', "(", args, ")"]
                    Nothing -> T.concat [n', "(", args, ")"]
              putStrLn' ("Execute: " <> show x)
              print' c'
              print' e
              modifyIORef (ls ^. result) $ M.delete x
              modifyIORef (ls ^. stdout) $ M.delete x
              modifyIORef (ls ^. error) $ M.delete x
              modifyIORef (ls ^. dirty) $ S.delete x
              writeChan (ls ^. queue) $ Just (x, unlines [c', e])
              writeChan (ls ^. queue) $ Just (U.nil, "%reset in out")
            Nothing -> bug LedgerBug
        Nothing -> bug LedgerBug

updateParameters :: (MonadIO m) => LedgerState -> UUID -> m ()
updateParameters ls x =
  (!? x) <$> readIORef (ls ^. code) >>= \case
    Just c -> do
      lbl <- readIORef (ls ^. label)
      bad <- readIORef (ls ^. badLabel)
      let bs =
            S.fromList
              . fmap snd
              . filter (flip S.notMember bad . fst)
              $ M.toList lbl
          ts = tokenize c
          ps = S.intersection bs ts
      modifyIORef (ls ^. parameters) (M.insert x ps)
    Nothing -> modifyIORef (ls ^. parameters) (M.delete x)

markDirty :: (MonadIO m) => LedgerState -> UUID -> m ()
markDirty ls u = do
  d <- S.member u <$> readIORef (ls ^. dirty)
  unless d $ do
    modifyIORef (ls ^. dirty) (S.insert u)
    markDepsDirty ls u

markDepsDirty :: forall m. (MonadIO m) => LedgerState -> UUID -> m ()
markDepsDirty ls x = do
  pss <- M.toList <$> readIORef (ls ^. parameters)
  bad <- readIORef (ls ^. badLabel)
  (!? x) <$> readIORef (ls ^. label) >>= \case
    Just l | S.notMember x bad -> do
      let ys = fst <$> filter (S.member l . snd) pss
      for_ ys $ \y -> do
        d <- S.member y <$> readIORef (ls ^. dirty)
        unless d $ markDirty ls y
    _ -> pass
