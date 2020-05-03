{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Jupyter.Types.Content where

import Control.Lens (makeFields)
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
import Prelude hiding (ExecuteRequest)

data ExecuteRequest
  = ExecuteRequest
      { executeRequestCode :: Text,
        executeRequestSilent :: Bool,
        executeRequestStoreHistory :: Bool,
        executeRequestUserExpressions :: Map Text Text,
        executeRequestAllowStdin :: Bool,
        executeRequestStopOnError :: Bool
      }
  deriving (Generic, Show)

data ExecuteReply
  = ExecuteReply
      { executeReplyExecutionCount :: Int,
        executeReplyPayload :: [Map Text Text],
        executeReplyUserExpressions :: Map Text Text
      }
  deriving (Generic, Show)

newtype ShutdownContent
  = ShutdownContent
      { shutdownContentRestart :: Bool
      }
  deriving (Generic, Show)

data HeartbeatContent = HeartbeatContent
  deriving (Generic, Show)

data KernelInfoRequest = KernelInfoRequest
  deriving (Generic, Show)

data KernelInfoReply
  = KernelInfoReply
      { kernelInfoReplyProtocolVersion :: Text,
        kernelInfoReplyImplementation :: Text,
        kernelInfoReplyImplementationVersion :: Text,
        kernelInfoReplyLanguageInfo :: LanguageInfo,
        kernelInfoReplyBanner :: Text,
        kernelInfoReplyHelpLinks :: Maybe [HelpLink]
      }
  deriving (Generic, Show)

data LanguageInfo
  = LanguageInfo
      { languageInfoName :: Text,
        languageInfoVersion :: Text
      }
  deriving (Generic, Show)

data HelpLink
  = HelpLink
      { helpLinkText :: Text,
        helpLinkUrl :: Text
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
      { errorReplyEname :: Text,
        errorReplyEvalue :: Text,
        errorReplyTraceback :: [Text]
      }
  deriving (Generic, Show)

data Reply a
  = ReplyOk a
  | ReplyError ErrorReply
  | ReplyAbort (Map Text Text)
  deriving (Generic, Show)

instance ToJSON ExecuteRequest where

  toJSON = genericToJSON (fieldsOptions 14)

  toEncoding = genericToEncoding (fieldsOptions 14)

instance FromJSON ExecuteReply where
  parseJSON = genericParseJSON (fieldsOptions 12)

instance ToJSON ShutdownContent where

  toJSON = genericToJSON (fieldsOptions 15)

  toEncoding = genericToEncoding (fieldsOptions 15)

instance FromJSON ShutdownContent where
  parseJSON = genericParseJSON (fieldsOptions 15)

instance ToJSON HeartbeatContent where

  toJSON = genericToJSON (fieldsOptions 16)

  toEncoding = genericToEncoding (fieldsOptions 16)

instance FromJSON HeartbeatContent where
  parseJSON = genericParseJSON (fieldsOptions 16)

instance FromJSON ErrorReply where
  parseJSON = genericParseJSON (fieldsOptions 10)

instance ToJSON KernelInfoRequest where

  toJSON = genericToJSON (fieldsOptions 17)

  toEncoding = genericToEncoding (fieldsOptions 17)

instance FromJSON KernelInfoReply where
  parseJSON = genericParseJSON (fieldsOptions 15)

instance FromJSON LanguageInfo where
  parseJSON = genericParseJSON (fieldsOptions 12)

instance FromJSON HelpLink where
  parseJSON = genericParseJSON (fieldsOptions 8)

instance (FromJSON a) => FromJSON (Reply a) where
  parseJSON j = flip (withObject "Reply") j $ \o -> do
    s <- o .: "status"
    if  | s == A.String "ok" -> ReplyOk <$> parseJSON (A.Object o)
        | s == A.String "error" -> ReplyError <$> parseJSON (A.Object o)
        | otherwise -> ReplyAbort <$> parseJSON (A.Object o)

makeFields ''ExecuteRequest

makeFields ''ExecuteReply

makeFields ''ShutdownContent

makeFields ''HeartbeatContent

makeFields ''ErrorReply

data StreamContent
  = StreamContent
      { streamContentName :: Text,
        streamContentText :: Text
      }
  deriving (Generic, Show)

newtype StatusContent
  = StatusContent
      { statusContentExecutionState :: ExecutionState
      }
  deriving (Generic, Show)

data InputContent
  = InputContent
      { inputContentExecutionCount :: Int,
        inputContentCode :: Text
      }
  deriving (Generic, Show)

data ResultContent
  = ResultContent
      { resultContentData_ :: Map Text Text,
        resultContentExecutionCount :: Int,
        resultContentMetadata :: Map Text Text
      }
  deriving (Generic, Show)

data ErrorContent
  = ErrorContent
      { errorContentTraceback :: [Text],
        errorContentEname :: Text,
        errorContentEvalue :: Text
      }
  deriving (Generic, Show)

type family IOPubContent m = res | res -> m where
  IOPubContent 'Stream = StreamContent
  IOPubContent 'Status = StatusContent
  IOPubContent 'ExecuteInput = InputContent
  IOPubContent 'ExecuteResult = ResultContent
  IOPubContent 'Error = ErrorContent

instance FromJSON StreamContent where
  parseJSON = genericParseJSON (fieldsOptions 13)

instance FromJSON StatusContent where
  parseJSON = genericParseJSON (fieldsOptions 13)

instance FromJSON InputContent where
  parseJSON = genericParseJSON (fieldsOptions 12)

instance FromJSON ResultContent where
  parseJSON = genericParseJSON (fieldsOptions 13)

instance FromJSON ErrorContent where
  parseJSON = genericParseJSON (fieldsOptions 12)

makeFields ''StreamContent

makeFields ''StatusContent

makeFields ''InputContent

makeFields ''ResultContent

makeFields ''ErrorContent
