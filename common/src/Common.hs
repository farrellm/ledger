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

customOptions :: Options
customOptions =
  defaultOptions
    { fieldLabelModifier =
        toQuietSnake . fromHumps . takeWhile (/= '_') . drop 1,
      constructorTagModifier = toQuietSnake . fromHumps,
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
