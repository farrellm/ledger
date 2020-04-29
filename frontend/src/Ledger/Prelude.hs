{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Prelude
  ( module X,
    LedgerBug (..),
  )
where

import Data.Witherable as X (catMaybes, mapMaybe)
import Relude as X hiding (catMaybes, error, mapMaybe, stdout)
import Relude.Extra.Map as X

data LedgerBug = LedgerError String | LedgerBug
  deriving (Show)

instance Exception LedgerBug
