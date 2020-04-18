{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Common
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Lens hiding ((#))
import Control.Monad.Fix (MonadFix)
import Data.Aeson (ToJSON, encode)
import qualified Data.Map as M
import Data.Traversable (for)
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as U
import Data.Witherable (Filterable, catMaybes, mapMaybe)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Types (JSM, liftJSM)
import Ledger.JS
import Ledger.Types
import Reflex.Dom.Core
import Relude hiding (catMaybes, mapMaybe, stdout)
import System.Random

filterDuplicates ::
  (Reflex t, Eq a, MonadHold t m, MonadFix m) =>
  Event t a ->
  m (Event t a)
filterDuplicates e = do
  d <- holdDyn Nothing $ Just <$> e
  d' <- holdUniqDyn d
  let e' = updated d'
  pure (catMaybes e')

newChan :: (MonadIO m) => m (Chan a)
newChan = liftIO Chan.newChan

readChan :: (MonadIO m) => Chan a -> m a
readChan = liftIO . Chan.readChan

writeChan :: (MonadIO m) => Chan a -> a -> m ()
writeChan c a = liftIO $ Chan.writeChan c a

withMVar :: (MonadIO m) => MVar a -> (a -> m b) -> m b
withMVar m f = do
  x <- takeMVar m
  r <- f x
  putMVar m x
  pure r

modifyMVar :: MonadIO m => MVar a -> (a -> a) -> m ()
modifyMVar m f = f <$> takeMVar m >>= putMVar m

fork :: MonadIO m => IO () -> m ()
fork = void . liftIO . forkIO

postJson' :: (ToJSON a) => Text -> a -> XhrRequest ByteString
postJson' url a =
  XhrRequest "POST" url $
    def
      { _xhrRequestConfig_headers = headerUrlEnc,
        _xhrRequestConfig_sendData = body
      }
  where
    headerUrlEnc = "Content-type" =: "text/plain"
    body = toStrict $ encode a

main :: IO ()
main =
  runLedger 8001 $
    mainWidgetWithHead htmlHead htmlBody

takeLeft :: Filterable f => f (Either a b) -> f a
takeLeft = mapMaybe leftToMaybe

takeRight :: Filterable f => f (Either a b) -> f b
takeRight = mapMaybe rightToMaybe

nextUUID :: (Reflex t, MonadIO (PushM t)) => Event t a -> Event t UUID
nextUUID = push (const $ Just <$> liftIO randomIO)

htmlHead :: MonadWidget t m => m ()
htmlHead = do
  elAttr
    "meta"
    ( "name" =: "viewport"
        <> "content" =: "width=device-width, initial-scale=1, maximum-scale=1.0, user-scalable=no"
    )
    blank
  el "title" $ text "Ledger"
  elAttr "script" ("defer" =: "" <> "src" =: "fontawesome/js/all.js") blank
  elAttr
    "script"
    ("src" =: "codemirror/lib/codemirror.js" <> "async" =: "false")
    blank
  elAttr
    "link"
    ( "rel" =: "stylesheet"
        <> "type" =: "text/css"
        <> "href" =: "codemirror/lib/codemirror.css"
    )
    blank
  elAttr
    "script"
    ("src" =: "codemirror/mode/python/python.js" <> "async" =: "false" <> "defer" =: "")
    blank
  elAttr
    "link"
    ( "rel" =: "stylesheet"
        <> "type" =: "text/css"
        <> "href" =: "bulma/css/bulma.min.css"
    )
    blank
  elAttr
    "link"
    ( "href" =: "main.css"
        <> "type" =: "text/css"
        <> "rel" =: "stylesheet"
    )
    blank

htmlBody ::
  forall t m.
  ( MonadWidget t m,
    MonadIO (PushM t),
    DomRenderHook t m
  ) =>
  m ()
htmlBody = do
  ls <-
    LedgerState <$> newIORef Nothing
      <*> newMVar []
      <*> newIORef mempty
      <*> newIORef mempty
      <*> newIORef mempty
      <*> newIORef mempty
      <*> newChan
      <*> newMVar False
  -- manage new kernel requests
  (evNewKernel', triggerNewKernel) <- newTriggerEvent
  evNewKernel <- NewKernel <<$>> getAndDecode (evNewKernel' $> "http://localhost:8000/new_kernel")
  liftIO $ triggerNewKernel ()
  (evDeadKernel', triggerDeadKernel) <- newTriggerEvent
  let evDeadKernel = evDeadKernel' $> DeadKernel
  -- manage execute requests
  evExRes <- do
    (evExecute, triggerExecute) <- newTriggerEvent
    fork . forever $ do
      q <- readChan (ls ^. queue)
      case q of
        Nothing -> void $ swapMVar (ls ^. stop) False
        Just (u, c) -> triggerExecute (u, c)
    evExecute' <- performEvent $
      evExecute <&> \(u, c) ->
        readIORef (ls ^. kernelUUID) >>= \case
          Just k -> pure $ Just $ ExecuteRequest k u c
          Nothing -> pure Nothing
    evX <-
      performRequestAsync $
        catMaybes evExecute' <&> postJson' "http://localhost:8000/execute"
    filterDuplicates (evX $> RunningCell)
  -- manage kernel output requests
  (evNewRequest, triggerNewRequest) <- newTriggerEvent
  evNewRequest' <- performEvent $
    evNewRequest <&> \() ->
      readIORef (ls ^. kernelUUID) >>= \case
        Nothing -> do
          liftIO $ putStrLn' "request fail 1"
          pure Nothing
        Just k -> pure . Just $ ResultRequest k
  evOutput' <-
    performRequestAsync $
      catMaybes evNewRequest' <&> postJson' "http://localhost:8000/result"
  evOutput'' <- filterDuplicates $ fmap decodeXhrResponse evOutput'
  evOutput''' <- performEvent $
    evOutput'' <&> \case
      Nothing -> do
        liftIO $ do
          putStrLn' "request fail 2"
          triggerDeadKernel ()
        pure Nothing
      Just (c :: UUID, r :: KernelOutput) -> do
        liftIO $ triggerNewRequest ()
        pure . Just $ Output c r
  let evOutput = catMaybes evOutput'''
  -- manage code changes
  (evCodeChange, triggerCodeChange) <- newTriggerEvent
  liftJSM
    . (global <# ("onCodeChange" :: Text))
    . fun
    $ \_ _ [u] -> valToText u >>= liftIO . triggerCodeChange
  evCodeChange' <- debounce 0.2 evCodeChange
  let evCode = UpdateCode <$> catMaybes (U.fromText <$> evCodeChange')
  --
  rec evStartKernel <- navbar dynKernel
      (evCells, evAddEnd) <- divClass "section" . divClass "container" $ do
        dynEvCells <- simpleList dynSnapshotCells $ \dynCode -> do
          dynCell <- holdUniqDyn (extractCell <$> dynCode <*> dynResults)
          evEvCell <- dyn (cell dynCell <$> dynCode)
          switchHold never evEvCell
        evCells' <- switchHold never $ leftmost <$> updated dynEvCells
        (elAdd, _) <-
          elAttr' "button" ("class" =: "button" <> "type" =: "button")
            . elAttr "span" ("class" =: "icon")
            $ elAttr "i" ("class" =: "fas fa-plus") blank
        let evAdd = domEvent Click elAdd
            evUUID' = AddCellEnd <$> nextUUID evAdd
        pure (evCells', evUUID')
      --
      evKernel <-
        performEvent $
          ( refreshState ls
              >=> kernelUpdate ls triggerNewKernel triggerNewRequest
              >=> \_ -> readIORef (ls ^. kernelUUID)
          )
            <$> leftmost [evNewKernel, evDeadKernel, evStartKernel]
      dynKernel <- holdDyn Nothing evKernel
      --
      evSnapshotCells <-
        performEvent $
          (refreshState ls >=> ledgerUpdate ls >=> snapshotCells ls)
            <$> leftmost [evAddEnd, takeRight evCells]
      dynSnapshotCells <- holdUniqDyn =<< holdDyn [] evSnapshotCells
      --
      evResults <-
        performEvent $
          (refreshState ls >=> resultsUpdate ls triggerNewKernel >=> snapshotResults ls)
            <$> leftmost [takeLeft evCells, evExRes, evOutput, evCode]
      dynResults <- holdUniqDyn =<< holdDyn (ResultsSnapshot mempty mempty mempty) evResults
  el "script" . text $
    unlines
      [ "cms = new Map()",
        "cs = new Map()"
      ]
  where
    refreshState :: LedgerState -> a -> Performable m a
    refreshState ls u = do
      withMVar (ls ^. uuids) $ \xs -> do
        cs <- for xs $ \x ->
          liftJSM $ do
            cms <- global ^. js ("cms" :: Text)
            cm <- cms ^. js (show x :: Text)
            c <- fromJSVal =<< cm ^. js0 ("getValue" :: Text)
            pure $ fromMaybe "" c
        writeIORef (ls ^. code) $ M.fromList (zip xs cs)
      pure u
    --
    kernelUpdate ::
      LedgerState ->
      (() -> IO ()) ->
      (() -> IO ()) ->
      KernelUpdate ->
      Performable m ()
    kernelUpdate ls triggerNewKernel triggerNewRequest u =
      case u of
        NewKernel k -> do
          writeIORef (ls ^. kernelUUID) k
          liftIO $ triggerNewRequest ()
        ShutdownKernel -> pass
        DeadKernel -> do
          putStrLn' "kernel died"
          writeIORef (ls ^. kernelUUID) Nothing
        StartKernel -> do
          putStrLn' "start new kernel"
          liftIO $ triggerNewKernel ()
    --
    resultsUpdate :: LedgerState -> (() -> IO ()) -> ResultsUpdate -> Performable m ()
    resultsUpdate ls triggerNewKernel u =
      case u of
        ExecuteCell x ->
          withMVar (ls ^. stop) $ \s ->
            unless s $
              M.lookup x <$> readIORef (ls ^. code) >>= \case
                Just c -> do
                  putStrLn' ("Execute: " <> show (x, c))
                  writeChan (ls ^. queue) $ Just (x, c)
                  modifyIORef (ls ^. result) $ M.delete x
                  modifyIORef (ls ^. stdout) $ M.delete x
                Nothing -> pass
        Output c (KernelResult _ r) ->
          modifyIORef (ls ^. result) (M.insert c r)
        Output c (KernelStdout _ r) ->
          modifyIORef (ls ^. stdout) (M.alter (Just . (<> r) . fromMaybe "") c)
        Output _ (KernelMissing _) -> do
          putStrLn' "KernelMissing"
          writeIORef (ls ^. kernelUUID) Nothing
          liftIO $ triggerNewKernel ()
        UpdateLabel c l ->
          modifyIORef (ls ^. label) (M.insert c l)
        r -> print' r
    --
    ledgerUpdate :: LedgerState -> LedgerUpdate -> Performable m ()
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
    --
    snapshotCells :: LedgerState -> () -> Performable m [CodeSnapshot]
    snapshotCells ls () = do
      l <- readIORef (ls ^. label)
      c <- readIORef (ls ^. code)
      fmap (\u -> CodeSnapshot u (M.lookup u l) (M.lookup u c)) <$> readMVar (ls ^. uuids)
    --
    snapshotResults :: LedgerState -> () -> Performable m ResultsSnapshot
    snapshotResults ls () =
      ResultsSnapshot
        <$> readIORef (ls ^. code)
        <*> readIORef (ls ^. result)
        <*> readIORef (ls ^. stdout)

navbar :: forall t m. (MonadWidget t m) => Dynamic t (Maybe UUID) -> m (Event t KernelUpdate)
navbar dynKernel =
  elAttr
    "nav"
    ( "class" =: "navbar has-background-light"
        <> "role" =: "navigation"
        <> ("aria-label" =: "main navigation")
    )
    . divClass "navbar-brand"
    $ do
      elAttr "span" ("class" =: "navbar-item") $ do
        elAttr "img" ("src" =: "notebook-icon.png" <> "width" =: "28" <> "height" =: "28") blank
        divClass "title" $ text "Ledger"
      elAttr "span" ("class" =: "navbar-item") $ do
        let c =
              unwords . (["button", "is-small"] <>) . maybe ["is-loading"] (const [])
                <$> dynKernel
            a = ("type" =: "button" <>) . ("class" =:) <$> c
        (elNewKernel, _) <-
          elDynAttr' "button" a
            . elAttr "span" ("class" =: "icon")
            $ elAttr "i" ("class" =: "fas fa-check") blank
        pure $ domEvent Click elNewKernel $> StartKernel

cell ::
  forall t m.
  (MonadWidget t m) =>
  Dynamic t CellSnapshot ->
  CodeSnapshot ->
  m (Event t (Either ResultsUpdate LedgerUpdate))
cell dynSnapshot c =
  divClass "card" $ do
    (evRes, evLeg) <- divClass "card-header" $ do
      evLhs <- elAttr "p" ("class" =: "card-header-title") $ do
        (elEx, _) <-
          elAttr'
            "button"
            ("class" =: "button is-primary is-outlined is-small" <> "type" =: "button")
            . elAttr "span" ("class" =: "icon is-small")
            $ elAttr "i" ("class" =: "fas fa-play") blank
        elLabel <-
          inputElement
            (def :: InputElementConfig EventResult t GhcjsDomSpace)
              { _inputElementConfig_initialValue = fromMaybe "" (c ^. label),
                _inputElementConfig_elementConfig =
                  def
                    { _elementConfig_initialAttributes =
                        "type" =: "text" <> "class" =: "input"
                    }
              }
        text "= λ()"
        let evEx = domEvent @t Click elEx $> ExecuteCell (c ^. uuid)
            evLabel = UpdateLabel (c ^. uuid) <$> _inputElement_input elLabel
        pure (leftmost [evEx, evLabel])
      evRhs <- elAttr "p" ("class" =: "card-header-icon")
        . divClass "buttons are-small has-addons"
        $ do
          (elU, _) <-
            elAttr'
              "button"
              ("class" =: "button" <> "type" =: "button")
              . elAttr "span" ("class" =: "icon is-small")
              $ elAttr "i" ("class" =: "fas fa-arrow-up") blank
          (elD, _) <-
            elAttr'
              "button"
              ("class" =: "button" <> "type" =: "button")
              . elAttr "span" ("class" =: "icon is-small")
              $ elAttr "i" ("class" =: "fas fa-arrow-down") blank
          (elX, _) <-
            elAttr'
              "button"
              ("class" =: "button is-danger is-outlined" <> "type" =: "button")
              . elAttr "span" ("class" =: "icon is-small")
              $ elAttr "i" ("class" =: "fas fa-times") blank
          let evX = domEvent @t Click elX $> RemoveCell (c ^. uuid)
              evU = domEvent @t Click elU $> RaiseCell (c ^. uuid)
              evD = domEvent @t Click elD $> LowerCell (c ^. uuid)
          pure $ leftmost [evX, evU, evD]
      pure (evLhs, evRhs)
    divClass "card-content" $ do
      elAttr "textarea" ("id" =: show (c ^. uuid)) $
        text (fromMaybe "" (c ^. code))
      dyn_ $
        dynSnapshot <&> \snapshot -> do
          case snapshot ^. stdout of
            Nothing -> blank
            Just t -> elClass "pre" "stdout" $ text t
          case snapshot ^. result of
            Nothing -> blank
            Just t -> elClass "pre" "result" $ text t
    el "script" $ text (mkScript $ c ^. uuid)
    pure (leftmost [Left <$> evRes, Right <$> evLeg])
  where
    mkScript :: UUID -> Text
    mkScript u =
      unlines
        [ "var ta = document.getElementById('" <> show u <> "')",
          "var cm = CodeMirror.fromTextArea(ta,",
          "   {mode: 'python', viewportMargin: Infinity})",
          "cms['" <> show u <> "']=cm",
          "cm.on('changes', function(x) { onCodeChange('" <> show u <> "') })"
        ]

extractCell :: CodeSnapshot -> ResultsSnapshot -> CellSnapshot
extractCell c r =
  CellSnapshot (r ^. result . at (c ^. uuid)) (r ^. stdout . at (c ^. uuid))

consoleLog :: (Show a) => a -> JSM JSVal
consoleLog x = eval ("console.log('" <> show x <> "')" :: Text)
