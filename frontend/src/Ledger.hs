{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger
  ( htmlHead,
    htmlBody,
  )
where

import Control.Lens
import qualified Data.Text as T
import qualified Data.UUID.Types as U
import GHCJS.DOM.Types (SerializedScriptValue (..))
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Types (liftJSM)
import Ledger.Events
import Ledger.JS
import Ledger.Prelude
import Ledger.Static
import Ledger.Types
import Ledger.Utility
import Ledger.Widgets
import Network.URI (URI (..), URIAuth (..))
import Reflex.Dom.Core
import System.FilePath (takeDirectory, takeFileName)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

htmlBody ::
  forall t m.
  ( MonadWidget t m,
    MonadIO (PushM t),
    DomRenderHook t m
  ) =>
  m ()
htmlBody = do
  (evKernelUpdate, triggerKernelUpdate') <- newTriggerEvent
  (evLedgerUpdate, triggerLedgerUpdate') <- newTriggerEvent
  (evResultsUpdate, triggerResultsUpdate') <- newTriggerEvent
  (evSave, triggerSave') <- newTriggerEvent
  (evCodeChange, triggerCodeChange) <- newTriggerEvent
  ls <- do
    url' <- getLocationUrl
    let _ledgerState_url = url' =~ ("https?://[^:]+" :: Text)
    print' _ledgerState_url
    _ledgerState_cwd <- newIORef "/"
    _ledgerState_file <- newIORef "x"
    _ledgerState_kernelUUID <- newEmptyMVar
    _ledgerState_uuids <- newMVar []
    _ledgerState_label <- newIORef mempty
    _ledgerState_badLabel <- newIORef mempty
    _ledgerState_parameters <- newIORef mempty
    _ledgerState_code <- newIORef mempty
    _ledgerState_result <- newIORef mempty
    _ledgerState_stdout <- newIORef mempty
    _ledgerState_error <- newIORef mempty
    _ledgerState_dirty <- newIORef mempty
    _ledgerState_queue <- newChan
    _ledgerState_stop <- newMVar False
    _ledgerState_ready <- newEmptyMVar
    pure
      LedgerState
        { _ledgerState_triggerKernelUpdate = triggerKernelUpdate',
          _ledgerState_triggerLedgerUpdate = triggerLedgerUpdate',
          _ledgerState_triggerResultsUpdate = triggerResultsUpdate',
          _ledgerState_triggerSave = triggerSave',
          ..
        }
  usingReaderT ls $ do
    liftJSM $ do
      -- javascript initialization
      (global <# ("cms" :: Text)) =<< obj
      (global <# ("onCodeChange" :: Text)) . fun $
        \_ _ [u] -> valToText u >>= liftIO . triggerCodeChange
    --
    evPostBuild <- getPostBuild
    protocol <- getLocationProtocol
    host <- getLocationHost
    hashbang <- toString . T.drop 2 <$> getLocationFragment
    putStrLn' ("hashbang: " <> hashbang)
    -- manage new kernel requests
    (evNewKernel', triggerNewKernel) <- newTriggerEvent
    evNewKernel'' <-
      filterDuplicates
        =<< mapMaybe decodeXhrResponse
        <$> performRequestAsync (evNewKernel' <&> postJson' ((ls ^. url) <> ":8000/get_kernel"))
    evNewKernel <- NewKernel <<$>> catMaybes <$> filterDuplicates evNewKernel''
    -- manage execute requests
    evExRes <- pollExecute
    -- manage kernel output requests
    evOutput <- pollOutput
    evCodeChange' <- debounce 0.2 evCodeChange
    let evCode = UpdateCode <$> catMaybes (U.fromText <$> evCodeChange')
    -- initialization
    evInitialization <-
      if hashbang == ""
        then initializeLedger
        else do
          writeIORef (ls ^. cwd) (takeDirectory hashbang)
          pure (evPostBuild $> hashbang)
    let evLoadPath = leftmost [evInitialization]
    performEvent_ $ evLoadPath <&> writeIORef (ls ^. file)
    void $ manageHistory $
      evLoadPath <&> \p ->
        HistoryCommand_PushState
          ( HistoryStateUpdate
              (SerializedScriptValue jsNull)
              ("Ledger: " <> toText (takeFileName p))
              ( Just
                  ( URI
                      (toString protocol)
                      (Just $ URIAuth "" (toString host) "")
                      ""
                      ""
                      ("#!" <> p)
                  )
              )
          )
    evLoad' <-
      filterDuplicates
        =<< mapMaybe decodeXhrResponse
          <$> performRequestAsync (evLoadPath <&> postJson' ((ls ^. url) <> ":8000/load"))
    let evLoad = evLoad' <&> \case
          Right r -> LoadLedger r
          Left e -> LedgerUpdateError ("error loading: " <> e)
    --
    void $ performRequestAsync (evSave <&> postJson' ((ls ^. url) <> ":8000/save"))
    --
    rec (evStartKernel, evSaveLoad) <- navbar dynKernel
        ((evCellsRes, evCellsLedger), evAddEnd) <-
          divClass "section" . divClass "container" $ do
            dynEvCells <- simpleList dynCodes $ \dynCode -> do
              dynCell <- holdUniqDyn (zipDynWith extractCell dynCode dynResults)
              cell dynCell dynCode
            let evCells = switchDyn $ leftmost <$> dynEvCells
            (elAdd, _) <-
              elAttr' "button" ("class" =: "button" <> "type" =: "button")
                . elAttr "span" ("class" =: "icon")
                $ elAttr "i" ("class" =: "fas fa-plus") blank
            let evAdd = domEvent Click elAdd
                evUUID = AddCellEnd <$> nextUUID evAdd
            pure (fanEither evCells, evUUID)
        void . dyn $
          dynKernel <&> \case
            Nothing -> blank
            Just _ ->
              elAttr
                "script"
                ( "type" =: "text/javascript"
                    <> "src" =: "codemirror/mode/python/python.js"
                    <> "defer" =: ""
                    <> "async" =: "false"
                )
                blank
        --
        evKernel <-
          performEvent $
            ( refreshState
                >=> traverse_ (kernelUpdate triggerNewKernel)
                >=> \_ -> tryReadMVar (ls ^. kernelUUID)
            )
              <$> mergeList
                [ evNewKernel,
                  evStartKernel,
                  evKernelUpdate
                ]
        dynKernel <- holdDyn Nothing evKernel
        --
        evCodes <-
          performEvent $
            ( refreshState
                >=> traverse_ ledgerUpdate
                >=> snapshotCells
            )
              <$> mergeList
                [ evLoad,
                  evAddEnd,
                  evCellsLedger,
                  evSaveLoad,
                  evLedgerUpdate
                ]
        dynCodes <- holdUniqDyn =<< holdDyn [] evCodes
        --
        evResults <-
          performEvent $
            ( refreshState
                >=> traverse_ resultsUpdate
                >=> snapshotResults
            )
              <$> mergeList [evCellsRes, evExRes, evOutput, evCode, evResultsUpdate]
        dynResults <- holdUniqDyn =<< holdDyn emptyResultsSnapshot evResults
    blank
