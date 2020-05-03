{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Common where

import Control.Lens (makeLenses)
import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    defaultOptions,
  )
import Data.UUID.Types (UUID)
import Relude
import Text.Casing (fromHumps, toQuietSnake)

enumOptions :: Options
enumOptions =
  defaultOptions
    { constructorTagModifier = toQuietSnake . fromHumps
    }

fieldsOptions :: Int -> Options
fieldsOptions n =
  defaultOptions
    { fieldLabelModifier =
        toQuietSnake . fromHumps . takeWhile (/= '_') . drop n,
      omitNothingFields = True
    }

data KernelInput
  = KernelExecute UUID Text
  | KernelShutdown
  deriving (Generic, Show, Eq, Ord)

data KernelOutput
  = KernelStdout Text Text
  | KernelResult Text Text
  | KernelError Text [Text] Text Text
  | KernelDone Text
  | KernelMissing UUID
  deriving (Generic, Show, Eq, Ord)

instance ToJSON KernelInput

instance FromJSON KernelInput

instance ToJSON KernelOutput

instance FromJSON KernelOutput

data ExecuteRequest
  = ExecuteRequest
      { _executeRequest_kernelUUID :: UUID,
        _executeRequest_cellUUID :: UUID,
        _executeRequest_cellCode :: Text
      }
  deriving (Generic, Show)

instance ToJSON ExecuteRequest

instance FromJSON ExecuteRequest

makeLenses ''ExecuteRequest

newtype ResultRequest
  = ResultRequest
      { _resultRequest_kernelUUID :: UUID
      }
  deriving (Generic, Show)

instance ToJSON ResultRequest

instance FromJSON ResultRequest

makeLenses ''ResultRequest

data LedgerSave
  = LedgerSave
      { _ledgerSave_label :: Map UUID Text,
        _ledgerSave_badLabel :: Set UUID,
        _ledgerSave_code :: Map UUID Text
      }
  deriving (Generic, Show)

instance ToJSON LedgerSave

instance FromJSON LedgerSave

makeLenses ''LedgerSave
