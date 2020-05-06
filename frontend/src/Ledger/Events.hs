{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Events where

import Common
import Control.Lens hiding ((#))
import Data.Char (isSpace)
import Data.Graph
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
import System.FilePath ((</>))

initializeLedger :: (MonadWidget t m, MonadReader LedgerState m) => m (Event t FilePath)
initializeLedger = do
  ls <- ask
  evPostBuild <- getPostBuild
  evCwd <-
    filterDuplicates =<< catMaybes
      <$> getAndDecode (evPostBuild $> ((ls ^. url) <> ":8000/home"))
  performEvent_ $ evCwd <&> writeIORef (ls ^. cwd)
  evList <-
    filterDuplicates
      =<< mapMaybe decodeXhrResponse
      <$> performRequestAsync (evCwd <&> postJson' ((ls ^. url) <> ":8000/list"))
  performEvent $
    evList <&> \ps -> do
      let ns = S.fromList $
            ps <&> \case
              LedgerDirectory n -> n
              LedgerFile n -> n
          go i =
            let n = "new-" <> show i <> ".ldgr"
             in if S.notMember n ns
                  then n
                  else go (i + 1)
          f =
            if S.notMember "new.ldgr" ns
              then "new.ldgr"
              else go (1 :: Int)
      d <- readIORef (ls ^. cwd)
      pure (d </> f)

pollExecute :: (MonadWidget t m, MonadReader LedgerState m) => m (Event t ResultsUpdate)
pollExecute = do
  ls <- ask
  (evExecute, triggerExecute) <- newTriggerEvent
  fork . forever $ do
    putStrLn' "#### waiting for ready"
    takeMVar (ls ^. ready)
    putStrLn' "#### ready!"
    q <- readChan (ls ^. queue)
    print' q
    case q of
      Nothing -> do
        void $ swapMVar (ls ^. stop) False
        void $ tryPutMVar (ls ^. ready) ()
      Just (u, c) -> do
        s <- readMVar (ls ^. stop)
        if s
          then do
            -- stopping - set cell dirty and reset ready
            (ls ^. triggerResultsUpdate) (UpdateCode u)
            void $ tryPutMVar (ls ^. ready) ()
          else triggerExecute (u, c)
  evExecute' <- performEventAsync $
    evExecute <&> \(u, c) go ->
      fork $ do
        k <- readMVar (ls ^. kernelUUID)
        go $ ExecuteRequest k u c
  evX <-
    performRequestAsync $
      evExecute' <&> postJson' ((ls ^. url) <> ":8000/execute")
  filterDuplicates (evX $> RunningCell)

pollOutput :: (MonadWidget t m, MonadReader LedgerState m) => m (Event t ResultsUpdate)
pollOutput = do
  ls <- ask
  evPostBuild <- getPostBuild
  (evNewRequest, triggerNewRequest) <- newTriggerEvent
  evNewRequest' <- performEventAsync $
    (evNewRequest <> evPostBuild) <&> \() go ->
      fork $ do
        putStrLn' "|||| read kernel UUID"
        k <- readMVar (ls ^. kernelUUID)
        putStrLn' ("|||| " <> show k)
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

refreshState :: (MonadJSM m, MonadReader LedgerState m) => b -> m b
refreshState u = do
  ls <- ask
  withMVar (ls ^. uuids) $ \xs -> do
    mcs <- liftJSM $ do
      cms <- jsg ("cms" :: Text)
      for xs $ \x -> do
        cm <- cms ^. js (show x :: Text)
        isUndef <- ghcjsPure $ isUndefined cm
        if isUndef
          then pure Nothing
          else fromJSVal =<< cm ^. js0 ("getValue" :: Text)
    case traverse (\(k, v) -> (k,) <$> v) (zip xs mcs) of
      Nothing -> putStrLn' "cannot refresh state!"
      Just cs -> writeIORef (ls ^. code) $ M.fromList cs
    for_ xs updateParameters
  pure u

kernelUpdate ::
  (MonadIO m, MonadReader LedgerState m) =>
  (FilePath -> IO ()) ->
  KernelUpdate ->
  m ()
kernelUpdate triggerNewKernel u = do
  ls <- ask
  case u of
    NewKernel k -> do
      putStrLn' ("new kernel UUID: " <> show k)
      putMVar (ls ^. kernelUUID) k
      putMVar (ls ^. ready) ()
      writeChan (ls ^. queue) $ Just (U.nil, "__ledger = dict()")
    ShutdownKernel -> pass
    DeadKernel -> do
      putStrLn' "kernel died"
      void $ tryTakeMVar (ls ^. kernelUUID)
    StartKernel -> do
      putStrLn' "start new kernel"
      f <- readIORef (ls ^. file)
      putStrLn' f
      liftIO $ triggerNewKernel f

resultsUpdate :: (MonadIO m, MonadReader LedgerState m) => ResultsUpdate -> m ()
resultsUpdate u = do
  ls <- ask
  case u of
    ExecuteCell c -> executeCell c
    Output c (KernelResult _ r) ->
      modifyIORef (ls ^. result) (M.insert c r)
    Output c (KernelStdout _ r) ->
      modifyIORef (ls ^. stdout) (M.alter (Just . (<> r) . fromMaybe "") c)
    Output c k@(KernelError _ rs _ _) -> do
      let rs' = filterEsc <$> rs
      print' k
      print' rs'
      modifyIORef (ls ^. error) (M.alter (Just . (<> unlines rs') . fromMaybe "") c)
      void $ swapMVar (ls ^. stop) True
      writeChan (ls ^. queue) Nothing
      modifyIORef (ls ^. dirty) (S.insert c)
    Output _ (KernelMissing _) -> do
      putStrLn' "KernelMissing"
      liftIO $ (ls ^. triggerKernelUpdate) DeadKernel
    Output _ (KernelDone _) -> do
      putStrLn' "KernelDone"
      putMVar (ls ^. ready) ()
    UpdateLabel c l -> do
      markDepsDirty c
      lbl <- readIORef (ls ^. label)
      let bs = S.fromList . fmap snd . filter ((/= c) . fst) $ M.toList lbl
      modifyIORef (ls ^. badLabel) (S.delete c)
      case T.strip l of
        "" -> modifyIORef (ls ^. label) (M.delete c)
        l' -> do
          modifyIORef (ls ^. label) (M.insert c l')
          when (S.member l' bs) $
            modifyIORef (ls ^. badLabel) (S.insert c)
      updateParameters c
    UpdateCode c -> markDirty c
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

ledgerUpdate :: (MonadIO m, MonadReader LedgerState m) => LedgerUpdate -> m ()
ledgerUpdate u = do
  ls <- ask
  case u of
    LedgerUpdateError err -> putStrLn' (toString err)
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
    LoadLedger s -> do
      putStrLn' "load"
      let us = fst <$> s ^. ledgerSave_code
      void $ swapMVar (ls ^. uuids) us
      writeIORef (ls ^. label) (s ^. ledgerSave_label)
      writeIORef (ls ^. badLabel) (s ^. ledgerSave_badLabel)
      writeIORef (ls ^. code) (M.fromList $ s ^. ledgerSave_code)
      writeIORef (ls ^. dirty)
        . S.fromList
        . fmap fst
        . filter ((/= "") . snd)
        $ s ^. ledgerSave_code
      for_ us updateParameters
      liftIO $ (ls ^. triggerKernelUpdate) StartKernel
    SaveLedger -> fork $ do
      f <- readIORef (ls ^. file)
      us <- readMVar (ls ^. uuids)
      cs <- readIORef (ls ^. code)
      _ledgerSave_label <- readIORef (ls ^. label)
      _ledgerSave_badLabel <- readIORef (ls ^. badLabel)
      let s =
            LedgerSave
              { _ledgerSave_code = mapMaybe (\x -> (x,) <$> (cs !? x)) us,
                ..
              }
      (ls ^. triggerSave) (f, s)
  where
    insertPenultimate z [] = [z]
    insertPenultimate z [w] = [z, w]
    insertPenultimate z (w : ws) = w : insertPenultimate z ws

executeCell :: forall m. (MonadIO m, MonadReader LedgerState m) => UUID -> m ()
executeCell x = do
  ls <- ask
  withMVar (ls ^. stop) $ \s ->
    unless s $ do
      f <- updatePython x
      ds <- solveDeps
      ds' <- filterM isDirty ds
      putStrLn' ("Execute: " <> show x)
      print' ds
      print' ds'
      if length ds' <= 1
        then do
          putStrLn' (toString f)
          modifyIORef (ls ^. result) $ M.delete x
          modifyIORef (ls ^. stdout) $ M.delete x
          modifyIORef (ls ^. error) $ M.delete x
          modifyIORef (ls ^. dirty) $ S.delete x
          writeChan (ls ^. queue) $ Just (x, f)
          writeChan (ls ^. queue) $ Just (U.nil, "%reset in out")
        else for_ ds' $ \d ->
          liftIO . (ls ^. triggerResultsUpdate) $ ExecuteCell d
  where
    isDirty :: UUID -> m Bool
    isDirty u = (u `S.member`) <$> (readIORef =<< view dirty)
    --
    solveDeps :: m [UUID]
    solveDeps = do
      putStrLn' "buildGraph"
      us <- viewMVar uuids
      ps <- viewIORef parameters
      lbl <- goodLabel
      let foo u = (u,,) <$> (lbl !? u) <*> (S.toList <$> ps !? u)
          es = mapMaybe foo us
          es' =
            if M.member x lbl
              then es
              else (x, "", maybe [] S.toList $ ps !? x) : es
          (g, nfv, vfk) = graphFromEdges es'
          r =
            S.fromList . fromMaybe [] $
              reachable g <$> vfk (fromMaybe "" $ lbl !? x)
          ts = reverse $ topSort g
      pure [nfv t ^. _1 | t <- ts, S.member t r]

updatePython :: (MonadIO m, MonadReader LedgerState m) => UUID -> m Text
updatePython x = do
  c <- fromMaybe "" . (!? x) <$> viewIORef code
  ps <- fromMaybe mempty . (!? x) <$> viewIORef parameters
  lbl <- viewIORef label
  lbl' <- transposeMap <$> goodLabel
  let n = T.replace "-" "_" (show x)
      n' = "_" <> n
      cs = lines c
      cs' =
        if  | isImports cs -> cs
            | isDef cs ->
              (wrapDef n' ps cs <> returnDef)
                ++ [saveResult n' lbl' ps]
            | otherwise ->
              (wrapDef n' ps $ addReturn cs)
                ++ saveResult n' lbl' ps
                : returnResult lbl
      f = unlines cs'
  pure f
  where
    isImports :: [Text] -> Bool
    isImports = all isImport
    --
    isDef :: [Text] -> Bool
    isDef = any (T.isPrefixOf "def _(")
    --
    isImport :: Text -> Bool
    isImport l =
      (T.length (T.strip l) == 0)
        || T.isPrefixOf "import " l
        || (T.isPrefixOf "from " l && T.isInfixOf " import " l)
    --
    wrapDef :: Text -> Set Text -> [Text] -> [Text]
    wrapDef n ps cs =
      "def " <> n <> "(" <> T.intercalate ", " (S.toList ps) <> "):"
        : (("  " <>) <$> cs)
    --
    saveResult n' lbl' ps =
      let args =
            T.intercalate ", " $
              fmap (\p -> p <> "=__ledger['" <> U.toText (lbl' M.! p) <> "']") (S.toList ps)
       in T.concat ["__ledger['", show x, "'] = ", n', "(", args, ")"]
    --
    returnResult lbl =
      case lbl !? x of
        Just _ -> []
        Nothing -> [T.concat ["__ledger['", show x, "']"]]
    --
    addReturn :: [Text] -> [Text]
    addReturn cs =
      let rs = dropWhile isSpaces $ reverse cs
       in case nonEmpty rs of
            Just (l :| ls) ->
              let l' =
                    if isSpace (T.head l)
                      || T.isPrefixOf "return" l
                      || T.isInfixOf ";" l
                      || T.isPrefixOf "%" l
                      then l
                      else "return " <> l
               in reverse (l' : ls)
            Nothing -> []
      where
        isSpaces :: Text -> Bool
        isSpaces = all isSpace . toString
    --
    returnDef :: [Text]
    returnDef = ["  return _"]

updateParameters :: (MonadIO m, MonadReader LedgerState m) => UUID -> m ()
updateParameters x = do
  ls <- ask
  (!? x) <$> readIORef (ls ^. code) >>= \case
    Just c -> do
      lbl <- readIORef (ls ^. label)
      bad <- readIORef (ls ^. badLabel)
      let bs =
            S.fromList
              . fmap snd
              . filter ((/= x) . fst)
              . filter (flip S.notMember bad . fst)
              $ M.toList lbl
          ts = tokenize c
          ps = S.intersection bs ts
      modifyIORef (ls ^. parameters) (M.insert x ps)
    Nothing -> modifyIORef (ls ^. parameters) (M.delete x)

markDirty :: (MonadIO m, MonadReader LedgerState m) => UUID -> m ()
markDirty u = do
  ls <- ask
  d <- S.member u <$> readIORef (ls ^. dirty)
  unless d $ do
    modifyIORef (ls ^. dirty) (S.insert u)
    markDepsDirty u

markDepsDirty :: forall m. (MonadIO m, MonadReader LedgerState m) => UUID -> m ()
markDepsDirty x = do
  ls <- ask
  pss <- M.toList <$> readIORef (ls ^. parameters)
  bad <- readIORef (ls ^. badLabel)
  (!? x) <$> readIORef (ls ^. label) >>= \case
    Just l | S.notMember x bad -> do
      let ys = fst <$> filter (S.member l . snd) pss
      for_ ys $ \y -> do
        d <- S.member y <$> readIORef (ls ^. dirty)
        unless d $ markDirty y
    _ -> pass
