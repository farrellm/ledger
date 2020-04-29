{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Types where

import Common
import Control.Concurrent.Chan (Chan)
import Control.Lens (makeLensesWith, underscoreFields)
import Data.UUID.Types (UUID)
import Ledger.Prelude

data KernelUpdate
  = NewKernel UUID
  | ShutdownKernel
  | DeadKernel
  | StartKernel
  deriving (Show, Eq)

data LedgerUpdate
  = AddCellEnd UUID
  | RemoveCell UUID
  | RaiseCell UUID
  | LowerCell UUID
  | LoadLedger FilePath
  | SaveLedger
  deriving (Show, Eq)

data ResultsUpdate
  = ExecuteCell UUID
  | RunningCell -- UUID
  | Output UUID KernelOutput
  | UpdateLabel UUID Text
  | UpdateCode UUID
  deriving (Show, Eq)

data LedgerState
  = LedgerState
      { _ledgerState_url :: Text,
        _ledgerState_file :: IORef FilePath,
        _ledgerState_triggerKernelUpdate :: KernelUpdate -> IO (),
        _ledgerState_triggerLedgerUpdate :: LedgerUpdate -> IO (),
        _ledgerState_triggerResultsUpdate :: ResultsUpdate -> IO (),
        _ledgerState_kernelUUID :: MVar UUID,
        _ledgerState_uuids :: MVar [UUID],
        _ledgerState_label :: IORef (Map UUID Text),
        _ledgerState_badLabel :: IORef (Set UUID),
        _ledgerState_parameters :: IORef (Map UUID (Set Text)),
        _ledgerState_code :: IORef (Map UUID Text),
        _ledgerState_result :: IORef (Map UUID Text),
        _ledgerState_stdout :: IORef (Map UUID Text),
        _ledgerState_error :: IORef (Map UUID Text),
        _ledgerState_dirty :: IORef (Set UUID),
        _ledgerState_queue :: Chan (Maybe (UUID, Text)),
        _ledgerState_stop :: MVar Bool,
        _ledgerState_ready :: MVar ()
      }

makeLensesWith underscoreFields ''LedgerState

data KernelLanguage
  = KernelJulia
  | KernelPython
  | KernelR
  deriving (Show, Eq)

data ResultsSnapshot
  = ResultsSnapshot
      { _resultsSnapshot_label :: Map UUID Text,
        _resultsSnapshot_badLabel :: Set UUID,
        _resultsSnapshot_parameters :: Map UUID (Set Text),
        _resultsSnapshot_code :: Map UUID Text,
        _resultsSnapshot_result :: Map UUID Text,
        _resultsSnapshot_stdout :: Map UUID Text,
        _resultsSnapshot_error :: Map UUID Text,
        _resultsSnapshot_dirty :: Set UUID
      }
  deriving (Show, Eq)

makeLensesWith underscoreFields ''ResultsSnapshot

emptyResultsSnapshot :: ResultsSnapshot
emptyResultsSnapshot =
  ResultsSnapshot mempty mempty mempty mempty mempty mempty mempty mempty

data CodeSnapshot
  = CodeSnapshot
      { _codeSnapshot_uuid :: UUID,
        _codeSnapshot_label :: Text,
        _codeSnapshot_badLabel :: Bool,
        _codeSnapshot_code :: Text
      }
  deriving (Show, Eq)

makeLensesWith underscoreFields ''CodeSnapshot

data CellSnapshot
  = CellSnapshot
      { _cellSnapshot_label :: Maybe Text,
        _cellSnapshot_badLabel :: Bool,
        _cellSnapshot_parameters :: Maybe (Set Text),
        _cellSnapshot_result :: Maybe Text,
        _cellSnapshot_stdout :: Maybe Text,
        _cellSnapshot_error :: Maybe Text,
        _cellSnapshot_dirty :: Bool
      }
  deriving (Show, Eq)

makeLensesWith underscoreFields ''CellSnapshot
