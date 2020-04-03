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

data KernelInfoRequest = KernelInfoRequest
  deriving (Generic, Show)

data KernelInfoReply
  = KernelInfoReply
      { _protocolVersion :: Text,
        _implementation :: Text,
        _implementationVersion :: Text,
        _languageInfo :: LanguageInfo,
        _banner :: Text,
        _helpLinks :: Maybe [HelpLink]
      }
  deriving (Generic, Show)

data LanguageInfo
  = LanguageInfo
      { _name :: Text,
        _version :: Text
      }
  deriving (Generic, Show)

data HelpLink
  = HelpLink
      { _text :: Text,
        _url :: Text
      }
  deriving (Generic, Show)

type family MsgContent m r = res | res -> m r where
  MsgContent 'Execute 'Request = ExecuteRequest
  MsgContent 'Execute 'Reply = Reply ExecuteReply
  MsgContent 'Shutdown 'Request = ShutdownContent
  MsgContent 'Shutdown 'Reply = Reply ShutdownContent
  MsgContent 'Heartbeat 'Request = HeartbeatContent
  MsgContent 'Heartbeat 'Reply = Void
  MsgContent 'KernelInfo 'Request = KernelInfoRequest
  MsgContent 'KernelInfo 'Reply = Reply KernelInfoReply

data ErrorReply
  = ErrorReply
      { _ename :: Text,
        _evalue :: Text,
        _traceback :: [Text]
      }
  deriving (Generic, Show)

data Reply a
  = ReplyOk a
  | ReplyError ErrorReply
  | ReplyAbort (Map Text Text)
  deriving (Generic, Show)

instance ToJSON ExecuteRequest where
  toJSON = genericToJSON customOptions

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

instance ToJSON KernelInfoRequest where
  toJSON = genericToJSON customOptions

instance FromJSON KernelInfoReply where
  parseJSON = genericParseJSON customOptions

instance FromJSON LanguageInfo where
  parseJSON = genericParseJSON customOptions

instance FromJSON HelpLink where
  parseJSON = genericParseJSON customOptions

instance (FromJSON a) => FromJSON (Reply a) where
  parseJSON j = flip (withObject "Reply") j $ \o -> do
    s <- o .: "status"
    if  | s == A.String "ok" -> ReplyOk <$> parseJSON (A.Object o)
        | s == A.String "error" -> ReplyError <$> parseJSON (A.Object o)
        | otherwise -> ReplyAbort <$> parseJSON (A.Object o)

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

data ErrorContent
  = ErrorContent
      { _traceback :: [Text],
        _ename :: Text,
        _evalue :: Text
      }
  deriving (Generic, Show)

type family IOPubContent m = res | res -> m where
  IOPubContent 'Stream = StreamContent
  IOPubContent 'Status = StatusContent
  IOPubContent 'ExecuteInput = InputContent
  IOPubContent 'ExecuteResult = ResultContent
  IOPubContent 'Error = ErrorContent

instance FromJSON StreamContent where
  parseJSON = genericParseJSON customOptions

instance FromJSON StatusContent where
  parseJSON = genericParseJSON customOptions

instance FromJSON InputContent where
  parseJSON = genericParseJSON customOptions

instance FromJSON ResultContent where
  parseJSON = genericParseJSON customOptions

instance FromJSON ErrorContent where
  parseJSON = genericParseJSON customOptions

makeFieldsNoPrefix ''StreamContent

makeFieldsNoPrefix ''StatusContent

makeFieldsNoPrefix ''InputContent

makeFieldsNoPrefix ''ResultContent

makeFieldsNoPrefix ''ErrorContent
