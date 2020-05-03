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
import qualified Data.Set as S
import qualified Data.UUID.Types as U
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Types (liftJSM)
import Ledger.Events
import Ledger.JS
import Ledger.Prelude
import Ledger.Types
import Ledger.Utility
import Ledger.Widgets
import Reflex.Dom.Core
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
  evPostBuild <- getPostBuild
  (evKernelUpdate, triggerKernelUpdate') <- newTriggerEvent
  (evLedgerUpdate, triggerLedgerUpdate') <- newTriggerEvent
  (evResultsUpdate, triggerResultsUpdate') <- newTriggerEvent
  ls <- do
    url' <-
      liftJSM $
        valToText =<< jsg ("window" :: Text) ! ("location" :: Text) ! ("href" :: Text)
    let _ledgerState_url = url' =~ ("https?://[^:]+" :: Text)
    print' _ledgerState_url
    _ledgerState_file <- newIORef "new.ldgr"
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
          ..
        }
  usingReaderT ls $ do
    -- manage new kernel requests
    (evNewKernel', triggerNewKernel) <- newTriggerEvent
    evNewKernel'' <- getAndDecode (evNewKernel' $> ((ls ^. url) <> ":8000/new_kernel"))
    evNewKernel <- NewKernel <<$>> catMaybes <$> filterDuplicates evNewKernel''
    -- manage execute requests
    evExRes <- pollExecute
    -- manage kernel output requests
    evOutput <- pollOutput
    -- manage code changes
    (evCodeChange, triggerCodeChange) <- newTriggerEvent
    liftJSM
      . (global <# ("onCodeChange" :: Text))
      . fun
      $ \_ _ [u] -> valToText u >>= liftIO . triggerCodeChange
    evCodeChange' <- debounce 0.2 evCodeChange
    let evCode = UpdateCode <$> catMaybes (U.fromText <$> evCodeChange')
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
                [ evPostBuild $> StartKernel,
                  evNewKernel,
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
              <$> mergeList [evAddEnd, evCellsLedger, evSaveLoad, evLedgerUpdate]
        dynCodes <- holdUniqDyn =<< holdDyn [] evCodes
        --
        evResults <-
          performEvent $
            ( refreshState
                >=> traverse_ resultsUpdate
                >=> snapshotResults
            )
              <$> mergeList [evCellsRes, evExRes, evOutput, evCode, evResultsUpdate]
        dynResults <-
          holdUniqDyn
            =<< holdDyn emptyResultsSnapshot evResults
    el "script" . text $ "cms = new Map()"

