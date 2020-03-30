{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Jupyter.Types.Common where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
  )
import qualified Data.Aeson.Types as A
import Data.Singletons
  ( SingI,
    SomeSing (..),
    fromSing,
    toSing,
    withSing,
  )
import Data.Singletons.Prelude.Show
import Data.Singletons.TH (singletons)
import Text.Casing (fromHumps, toQuietSnake)

customOptions :: Options
customOptions =
  defaultOptions
    { fieldLabelModifier =
        toQuietSnake . fromHumps . takeWhile (/= '_') . drop 1,
      constructorTagModifier = toQuietSnake . fromHumps,
      omitNothingFields = True
    }

newtype Username = Username String
  deriving (Generic, Show)

instance FromJSON Username where
  parseJSON = genericParseJSON customOptions

instance ToJSON Username where

  toJSON = genericToJSON customOptions

  toEncoding = genericToEncoding customOptions

$( singletons
     [d|
       data ReqRep = Request | Reply
         deriving (Show)

       data MessageType = Shutdown | Execute | Heartbeat
         deriving (Show)

       data IOPubType = Stream | Status | ExecuteInput | ExecuteResult
         deriving (Generic, Show)

       data KernelSocket = Shell | IOPub | Stdin | Control | Hb
         deriving (Show)
       |]
 )

instance FromJSON IOPubType where
  parseJSON = genericParseJSON customOptions

instance FromJSON (SomeSing IOPubType) where
  parseJSON v = toSing <$> parseJSON v

instance (SingI e) => FromJSON (SIOPubType e) where
  parseJSON o = withSing @e $ \s ->
    let p = (toText . toQuietSnake . fromHumps . show $ fromSing s)
        err = "expected \"" <> toString p <> "\", but encountedered " <> show o
     in case o of
          A.String v | p == v -> pure s
          _ -> fail err

data ExecutionState = Starting | Busy | Idle
  deriving (Generic, Show, Eq)

instance FromJSON ExecutionState where
  parseJSON = genericParseJSON customOptions

type family MessageSocket m where
  MessageSocket 'Shutdown = 'Control
  MessageSocket 'Execute = 'Shell
  MessageSocket 'Heartbeat = 'Hb

class Deserialize a where
  deserialize :: [ByteString] -> a

class Serialize a where
  serialize :: a -> NonEmpty ByteString

newtype DeserializeBug = DeserializeBug String
  deriving (Show)

instance Exception DeserializeBug
