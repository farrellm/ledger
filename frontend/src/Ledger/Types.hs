{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Types where

import Control.Concurrent.Chan (Chan)
import Control.Lens (makeFieldsNoPrefix)
import Data.UUID.Types (UUID)
import Relude

data LedgerState
  = LedgerState
      { _uuids :: MVar [UUID],
        _code :: IORef (Map UUID Text),
        _queue :: Chan (Maybe (UUID, Text)),
        _stop :: MVar Bool
      }

makeFieldsNoPrefix ''LedgerState

data LedgerSnapshot
  = LedgerSnapshot
      { _uuids :: [UUID],
        _code :: Map UUID Text
      }
  deriving (Show, Eq)

makeFieldsNoPrefix ''LedgerSnapshot

data LedgerUpdate
  = AddCellEnd UUID
  | RemoveCell UUID
  | RaiseCell UUID
  | LowerCell UUID
  | ExecuteCell UUID
  | LoadLedger FilePath
  deriving (Show)
