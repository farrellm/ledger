{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Jupyter.Types.Content where

import Control.Lens.TH (makeFieldsNoPrefix)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
  )
import Data.Aeson.Types ((.:), withObject)
import qualified Data.Aeson.Types as A
import Jupyter.Types.Common

data ExecuteRequest
  = ExecuteRequest
      { _code :: Text,
        _silent :: Bool,
        _storeHistory :: Bool,
        _userExpressions :: Map Text Text,
        _allowStdin :: Bool,
        _stopOnError :: Bool
      }
  deriving (Generic, Show)

data ExecuteReply
  = ExecuteReply
      { _executionCount :: Int,
        _payload :: [Map Text Text],
        _userExpressions :: Map Text Text
      }
  deriving (Generic, Show)

newtype ShutdownContent
  = ShutdownContent
      { _restart :: Bool
      }
  deriving (Generic, Show)

data HeartbeatContent = HeartbeatContent
  deriving (Generic, Show)

type family MsgContent m r = res | res -> m r where
  MsgContent 'Execute 'Request = ExecuteRequest
  MsgContent 'Execute 'Reply = Reply ExecuteReply
  MsgContent 'Shutdown 'Request = ShutdownContent
  MsgContent 'Shutdown 'Reply = Reply ShutdownContent
  MsgContent 'Heartbeat 'Request = HeartbeatContent
  MsgContent 'Heartbeat 'Reply = Void

data ErrorReply
  = ErrorReply
      { _ename :: Text,
        _evalue :: Text,
        _traceback :: [Text]
      }
  deriving (Generic, Show)

data Reply a
  = Ok a
  | Error ErrorReply
  | Abort (Map Text Text)
  deriving (Generic, Show)

instance ToJSON ExecuteRequest where

  toJSON = genericToJSON customOptions

  toEncoding = genericToEncoding customOptions

instance FromJSON ExecuteReply where
  parseJSON = genericParseJSON customOptions

instance ToJSON ShutdownContent where
  toJSON = genericToJSON customOptions

instance FromJSON ShutdownContent where
  parseJSON = genericParseJSON customOptions

instance ToJSON HeartbeatContent where
  toJSON = genericToJSON customOptions

instance FromJSON HeartbeatContent where
  parseJSON = genericParseJSON customOptions

instance FromJSON ErrorReply where
  parseJSON = genericParseJSON customOptions

instance (FromJSON a) => FromJSON (Reply a) where
  parseJSON j = flip (withObject "Reply") j $ \o -> do
    s <- o .: "status"
    if  | s == A.String "ok" -> Ok <$> parseJSON (A.Object o)
        | s == A.String "error" -> Error <$> parseJSON (A.Object o)
        | otherwise -> Abort <$> parseJSON (A.Object o)

makeFieldsNoPrefix ''ExecuteRequest

makeFieldsNoPrefix ''ExecuteReply

makeFieldsNoPrefix ''ShutdownContent

makeFieldsNoPrefix ''HeartbeatContent

makeFieldsNoPrefix ''ErrorReply

data StreamContent
  = StreamContent
      { _name :: Text,
        _text :: Text
      }
  deriving (Generic, Show)

newtype StatusContent
  = StatusContent
      { _executionState :: ExecutionState
      }
  deriving (Generic, Show)

data InputContent
  = InputContent
      { _executionCount :: Int,
        _code :: Text
      }
  deriving (Generic, Show)

data ResultContent
  = ResultContent
      { _data_ :: Map Text Text,
        _executionCount :: Int,
        _metadata :: Map Text Text
      }
  deriving (Generic, Show)

type family IOPubContent m = res | res -> m where
  IOPubContent 'Stream = StreamContent
  IOPubContent 'Status = StatusContent
  IOPubContent 'ExecuteInput = InputContent
  IOPubContent 'ExecuteResult = ResultContent

instance FromJSON StreamContent where
  parseJSON = genericParseJSON customOptions

instance FromJSON StatusContent where
  parseJSON = genericParseJSON customOptions

instance FromJSON InputContent where
  parseJSON = genericParseJSON customOptions

instance FromJSON ResultContent where
  parseJSON = genericParseJSON customOptions

makeFieldsNoPrefix ''StreamContent

makeFieldsNoPrefix ''StatusContent

makeFieldsNoPrefix ''InputContent

makeFieldsNoPrefix ''ResultContent
