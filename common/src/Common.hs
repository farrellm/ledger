{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Common where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
  )
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
  = KernelExecute Text
  | KernelShutdown
  deriving (Show, Eq, Ord)

data KernelOutput
  = KernelStdout Text
  | KernelResult Text
  | KernelError [Text] Text Text
  | KernelDone
  deriving (Show, Eq, Ord)
