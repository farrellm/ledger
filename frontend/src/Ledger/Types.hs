{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Types where

import Common
import Control.Concurrent.Chan (Chan)
import Control.Lens (makeFieldsNoPrefix)
import Data.UUID.Types (UUID)
import Relude

data LedgerState
  = LedgerState
      { _kernelUUID :: IORef (Maybe UUID),
        _uuids :: MVar [UUID],
        _label :: IORef (Map UUID (Either Text Text)),
        _parameters :: IORef (Map UUID (Set Text)),
        _code :: IORef (Map UUID Text),
        _result :: IORef (Map UUID Text),
        _stdout :: IORef (Map UUID Text),
        _error :: IORef (Map UUID Text),
        _dirty :: IORef (Set UUID),
        _queue :: Chan (Maybe (UUID, Text)),
        _stop :: MVar Bool,
        _ready :: MVar ()
      }

makeFieldsNoPrefix ''LedgerState

data KernelLanguage
  = KernelJulia
  | KernelPython
  | KernelR
  deriving (Show, Eq)

data ResultsSnapshot
  = ResultsSnapshot
      { _label :: Map UUID (Either Text Text),
        _parameters :: Map UUID (Set Text),
        _code :: Map UUID Text,
        _result :: Map UUID Text,
        _stdout :: Map UUID Text,
        _error :: Map UUID Text,
        _dirty :: Set UUID
      }
  deriving (Show, Eq)

makeFieldsNoPrefix ''ResultsSnapshot

data CodeSnapshot
  = CodeSnapshot
      { _uuid :: UUID,
        _label :: Maybe (Either Text Text),
        _code :: Maybe Text
      }
  deriving (Show, Eq)

makeFieldsNoPrefix ''CodeSnapshot

data CellSnapshot
  = CellSnapshot
      { _label :: Maybe (Either Text Text),
        _parameters :: Maybe (Set Text),
        _result :: Maybe Text,
        _stdout :: Maybe Text,
        _error :: Maybe Text,
        _dirty :: Bool
      }
  deriving (Show, Eq)

makeFieldsNoPrefix ''CellSnapshot

data KernelUpdate
  = NewKernel (Maybe UUID)
  | ShutdownKernel
  | DeadKernel
  | StartKernel
  deriving (Show, Eq)

data LedgerUpdate
  = NullUpdate Text
  | AddCellEnd UUID
  | RemoveCell UUID
  | RaiseCell UUID
  | LowerCell UUID
  | LoadLedger FilePath
  deriving (Show, Eq)

data ResultsUpdate
  = ExecuteCell UUID
  | RunningCell -- UUID
  | Output UUID KernelOutput
  | UpdateLabel UUID Text
  | UpdateCode UUID
  deriving (Show, Eq)
