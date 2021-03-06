{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Jupyter.Types.Common where

import Control.Concurrent.Chan.Lifted (Chan)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    genericParseJSON,
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
import Data.UUID (UUID)
import Text.Casing (fromHumps, toQuietSnake)

newtype Username = Username String
  deriving (Generic, Show)

instance FromJSON Username

instance ToJSON Username

$( singletons
     [d|
       data ReqRep = Request | Reply
         deriving (Show)

       data MessageType = Shutdown | Execute | Heartbeat | KernelInfo
         deriving (Show)

       data IOPubType = Stream | Status | ExecuteInput | ExecuteResult | Error
         deriving (Generic, Show)

       data KernelSocket = Shell | IOPub | Stdin | Control | Hb
         deriving (Show)
       |]
 )

instance FromJSON IOPubType where
  parseJSON = genericParseJSON enumOptions

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
  parseJSON = genericParseJSON enumOptions

type family MessageSocket m where
  MessageSocket 'Shutdown = 'Control
  MessageSocket 'Execute = 'Shell
  MessageSocket 'KernelInfo = 'Shell
  MessageSocket 'Heartbeat = 'Hb

class Deserialize a where
  deserialize :: [ByteString] -> a

class Serialize a where
  serialize :: a -> NonEmpty ByteString

newtype DeserializeBug = DeserializeBug String
  deriving (Show)

instance Exception DeserializeBug

data KernelControl
  = KernelControl
      { _in :: Chan KernelInput,
        _out :: Chan (UUID, KernelOutput),
        _done :: MVar (),
        _destruct :: MVar ()
      }
