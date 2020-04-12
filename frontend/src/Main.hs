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
import Control.Concurrent
  ( forkIO,
    threadDelay,
  )
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Lens hiding ((#))
import Control.Monad.Fix (MonadFix)
import Data.Aeson (ToJSON, encode)
import qualified Data.Map as M
import Data.Traversable (for)
import Data.UUID.Types (UUID)
import Data.Witherable (catMaybes)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Types (JSM, liftJSM)
import Ledger.JS
import Ledger.Types
import Reflex.Dom.Core
import Relude hiding (catMaybes, stdout)
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
      <*> newChan
      <*> newMVar False
  -- events for launching a new kernel
  (evNewKernel', triggerNewKernel) <- newTriggerEvent
  evNewKernel'' <- debounce 0.1 evNewKernel'
  evNewKernel <- NewKernel <<$>> getAndDecode (evNewKernel'' $> "http://localhost:8000/new_kernel")
  liftIO $ triggerNewKernel ()
  -- manage requests for kernel output
  (evNewRequest, triggerNewRequest) <- newTriggerEvent
  evNewRequest' <- debounce 0.1 evNewRequest
  evNewRequest'' <- performEvent $
    evNewRequest' <&> \() ->
      readIORef (ls ^. kernelUUID) >>= \case
        Nothing -> do
          void . liftIO $ forkIO $ do
            threadDelay 100000
            triggerNewRequest ()
          pure Nothing
        Just k -> pure . Just $ ResultRequest k
  evOutput' <-
    performRequestAsync $
      catMaybes evNewRequest'' <&> postJson' "http://localhost:8000/result"
  evOutput'' <- performEvent $
    evOutput' <&> \res ->
      case decodeXhrResponse res of
        Nothing -> do
          void . liftIO $ forkIO $ do
            threadDelay 1000000
            triggerNewRequest ()
          pure Nothing
        Just (c :: UUID, r :: KernelOutput) -> do
          liftIO $ triggerNewRequest ()
          pure . Just $ Output c r
  evOutput <- filterDuplicates $ catMaybes evOutput''
  liftIO $ triggerNewRequest ()
  --
  rec elAttr
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
          elAttr "span" ("class" =: "navbar-item") . void . dyn $
            dynSnapshot <&> \snapshot ->
              let c =
                    maybe "is-loading " (const "") (snapshot ^. kernelReady)
                      <> "button is-small is-static"
               in elAttr "div" ("class" =: c <> "type" =: "button")
                    . elAttr "span" ("class" =: "icon")
                    $ elAttr "i" ("class" =: "fas fa-check") blank
      (evCells, evUUID) <- divClass "section" . divClass "container" $ do
        dynEvCells <- simpleList ((^. uuids) <$> dynSnapshot) $ \dynU -> do
          evEvCell <- dyn $ liftA2 mkCell dynU dynSnapshot
          switchHold never evEvCell
        evCells' <- switchHold never $ leftmost <$> updated dynEvCells
        (elAdd, _) <-
          elAttr' "button" ("class" =: "button" <> "type" =: "button")
            . elAttr "span" ("class" =: "icon")
            $ elAttr "i" ("class" =: "fas fa-plus") blank
        let evAdd = domEvent Click elAdd
            evUUID' = AddCellEnd <$> nextUUID evAdd
        pure (evCells', evUUID')
      evExRes <- do
        (evExecute, triggerExecute) <- newTriggerEvent
        void . liftIO . forkIO . forever $ do
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
      evSnapshot <-
        performEvent $
          (refreshState ls >=> updateState ls triggerNewKernel >=> snapshotState ls)
            <$> leftmost [evNewKernel, evUUID, evCells, evExRes, evOutput]
      dynSnapshot <-
        holdUniqDyn
          =<< holdDyn (LedgerSnapshot Nothing [] mempty mempty mempty) evSnapshot
  el "script" . text $
    unlines
      [ "cms = new Map()",
        "cs = new Map()"
      ]
  where
    mkScript i =
      unlines
        [ "var ta = document.getElementById('" <> i <> "')",
          "var cm = CodeMirror.fromTextArea(ta,",
          "   {mode: 'python', viewportMargin: Infinity})",
          "cms['" <> i <> "']=cm"
        ]
    --
    nextUUID = push (const $ Just <$> liftIO randomIO)
    --
    refreshState :: LedgerState -> LedgerUpdate -> Performable m LedgerUpdate
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
    updateState :: LedgerState -> (() -> IO ()) -> LedgerUpdate -> Performable m ()
    updateState ls triggerNewKernel u =
      case u of
        NewKernel k -> writeIORef (ls ^. kernelUUID) k
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
        ExecuteCell x ->
          withMVar (ls ^. stop) $ \s ->
            unless s $
              M.lookup x <$> readIORef (ls ^. code) >>= \case
                Just c -> do
                  putStrLn ("Execute: " <> show (x, c))
                  writeChan (ls ^. queue) $ Just (x, c)
                  modifyIORef (ls ^. result) $ M.delete x
                  modifyIORef (ls ^. stdout) $ M.delete x
                Nothing -> pass
        Output c (KernelResult _ r) ->
          modifyIORef (ls ^. result) (M.insert c r)
        Output c (KernelStdout _ r) ->
          modifyIORef (ls ^. stdout) (M.alter (Just . (<> r) . fromMaybe "") c)
        Output _ (KernelMissing _) -> do
          writeIORef (ls ^. kernelUUID) Nothing
          liftIO $ triggerNewKernel ()
        r -> print r
      where
        insertPenultimate z [] = [z]
        insertPenultimate z [w] = [z, w]
        insertPenultimate z (w : ws) = w : insertPenultimate z ws
    --
    snapshotState :: LedgerState -> () -> Performable m LedgerSnapshot
    snapshotState ls () =
      LedgerSnapshot
        <$> readIORef (ls ^. kernelUUID)
        <*> readMVar (ls ^. uuids)
        <*> readIORef (ls ^. code)
        <*> readIORef (ls ^. result)
        <*> readIORef (ls ^. stdout)
    --
    --
    mkCell u snapshot =
      divClass "card" $ do
        evX <- divClass "card-header" $ do
          evLhs <- elAttr "p" ("class" =: "card-header-title") $ do
            (elEx, _) <-
              elAttr'
                "button"
                ("class" =: "button is-primary is-outlined is-small" <> "type" =: "button")
                . elAttr "span" ("class" =: "icon is-small")
                $ elAttr "i" ("class" =: "fas fa-play") blank
            text "λ()"
            let evEx = domEvent @t Click elEx $> ExecuteCell u
            pure evEx
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
              let evX = domEvent @t Click elX $> RemoveCell u
                  evU = domEvent @t Click elU $> RaiseCell u
                  evD = domEvent @t Click elD $> LowerCell u
              pure $ leftmost [evX, evU, evD]
          pure $ leftmost [evLhs, evRhs]
        divClass "card-content" $ do
          elAttr "textarea" ("id" =: show u) $
            text (fromMaybe "" $ snapshot ^. code . at u)
          case snapshot ^. stdout . at u of
            Nothing -> blank
            Just t -> elClass "pre" "stdout" $ text t
          case snapshot ^. result . at u of
            Nothing -> blank
            Just t -> elClass "pre" "result" $ text t
        el "script" $ text (mkScript $ show u)
        pure evX

consoleLog :: (Show a) => a -> JSM JSVal
consoleLog x = eval ("console.log('" <> show x <> "')" :: Text)
