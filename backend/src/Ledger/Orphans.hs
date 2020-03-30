{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ledger.Orphans
  (
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
  )
import Network.Socket (PortNumber)

instance FromJSON PortNumber where
  parseJSON p = fromInteger <$> parseJSON p

instance ToJSON PortNumber where

  toJSON p = toJSON (fromIntegral p :: Integer)

  toEncoding p = toEncoding (fromIntegral p :: Integer)
