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
        _code :: IORef (Map UUID Text),
        _result :: IORef (Map UUID Text),
        _stdout :: IORef (Map UUID Text),
        _queue :: Chan (Maybe (UUID, Text)),
        _stop :: MVar Bool
      }

makeFieldsNoPrefix ''LedgerState

data KernelLanguage
  = KernelJulia
  | KernelPython
  | KernelR
  deriving (Show, Eq)

data LedgerSnapshot
  = LedgerSnapshot
      { _kernelReady :: Maybe UUID,
        _uuids :: [UUID],
        _code :: Map UUID Text,
        _result :: Map UUID Text,
        _stdout :: Map UUID Text
      }
  deriving (Show, Eq)

makeFieldsNoPrefix ''LedgerSnapshot

data LedgerUpdate
  = NullUpdate Text
  | NewKernel (Maybe UUID)
  | AddCellEnd UUID
  | RemoveCell UUID
  | RaiseCell UUID
  | LowerCell UUID
  | ExecuteCell UUID
  | RunningCell -- UUID
  | Output UUID KernelOutput
  | LoadLedger FilePath
  deriving (Show, Eq)
