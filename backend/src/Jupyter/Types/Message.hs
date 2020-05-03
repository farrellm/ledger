{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Jupyter.Types.Message
  ( Header (..),
    Message (..),
    IOPubMessage (..),
    MsgInsts (..),
    IOPubInsts (..),
    msgInsts,
    iopubInsts,
    messageSocket,
    newHeader,
    newRequest,
    newHeartbeat,
    -- header lenses
    msgId,
    username,
    session,
    date,
    msgType,
    version,
    -- message lenses
    zmqIdent,
    header,
    parentHeader,
    metadata,
    content,
    buffers,
  )
where

import Control.Lens
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    eitherDecode,
    encode,
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
  )
import qualified Data.Aeson.Types as A
import Data.GADT.Show (GShow (..))
import Data.HashMap.Strict (lookup)
import Data.Singletons
  ( SingI,
    SingInstance (..),
    fromSing,
    singInstance,
    withSing,
    withSomeSing,
  )
import Data.Some (Some (..))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V1 (nextUUID)
import Jupyter.Types.Common
import Jupyter.Types.Content
import Jupyter.Types.Kernel
import System.ZMQ4.Monadic (Receiver, Sender, Socket)
import Text.Casing (fromHumps, toQuietSnake)
import Text.Show (ShowS, showsPrec)

toSnake :: Text -> Text
toSnake = toText . toQuietSnake . fromHumps . toString

data MsgType m r = MsgType (SMessageType m) (SReqRep r)
  deriving (Generic, Show)

instance ToJSON (MsgType m r) where
  toJSON (MsgType m r) = A.String (toSnake (show (fromSing m) <> show (fromSing r)))

instance (SingI m, SingI r) => FromJSON (MsgType m r) where
  parseJSON o =
    withSing @m $ \m -> withSing @r $ \r ->
      let p = toSnake (show (fromSing m) <> show (fromSing r))
          err = "expected \"" <> toString p <> "\", but encountedered " <> show o
       in case o of
            A.String v | p == v -> pure $ MsgType m r
            _ -> fail err

type family MsgUUID (r :: ReqRep) where
  MsgUUID 'Request = UUID
  MsgUUID 'Reply = Text

data Header (m :: MessageType) (r :: ReqRep)
  = Header
      { headerMsgId :: MsgUUID r,
        headerUsername :: Username,
        headerSession :: MsgUUID r,
        headerDate :: UTCTime,
        headerMsgType :: MsgType m r,
        headerVersion :: Text
      }
  deriving (Generic)

makeFields ''Header

deriving instance Show (Header m 'Request)

deriving instance Show (Header m 'Reply)

instance ToJSON (Header m 'Request) where

  toJSON = genericToJSON (fieldsOptions 6)

  toEncoding = genericToEncoding (fieldsOptions 6)

instance (SingI m) => FromJSON (Header m 'Reply) where
  parseJSON = genericParseJSON (fieldsOptions 6)

instance (SingI m) => FromJSON (Header m 'Request) where
  parseJSON = genericParseJSON (fieldsOptions 6)

type family ParentHeader (m :: MessageType) (r :: ReqRep) where
  ParentHeader m 'Request = ()
  ParentHeader m 'Reply = Header m 'Request

data Message (m :: MessageType) (r :: ReqRep)
  = Message
      { messageZmqIdent :: Text,
        messageHeader :: Header m r,
        messageParentHeader :: ParentHeader m r,
        messageMetadata :: HashMap Text A.Value,
        messageContent :: MsgContent m r,
        messageBuffers :: [Text]
      }
  deriving (Generic)

makeFields ''Message

deriving instance
  ( Show (Header m r),
    Show (ParentHeader m r),
    Show (MsgContent m r)
  ) =>
  Show (Message m r)

instance (ToJSON (MsgContent m 'Request)) => ToJSON (Message m 'Request) where

  toJSON = genericToJSON (fieldsOptions 7)

  toEncoding = genericToEncoding (fieldsOptions 7)

instance (SingI m, FromJSON (MsgContent m 'Reply)) => FromJSON (Message m 'Reply) where
  parseJSON = genericParseJSON (fieldsOptions 7)

messageSocket ::
  forall s m z.
  (s ~ MessageSocket m, SingI s, KernelSocket' s) =>
  SMessageType m ->
  Kernel z ->
  Socket z (SocketType s)
messageSocket _ k = withSing @s $ \s -> kernelSocket s k

instance
  (FromJSON (MsgContent e 'Reply), SingI e) =>
  Deserialize (Message e 'Reply)
  where
  deserialize (ids : "<IDS|MSG>" : _s : h : ph : md : c : bs) =
    let res = do
          messageHeader <- eitherDecode $ toLazy h
          messageParentHeader <- eitherDecode $ toLazy ph
          messageMetadata <- eitherDecode $ toLazy md
          messageContent <- eitherDecode $ toLazy c
          messageBuffers <- traverse (eitherDecode . toLazy) bs
          pure Message {messageZmqIdent = decodeUtf8 ids, ..}
     in case res of
          Right msg -> msg
          Left err -> bug $ DeserializeBug err
  deserialize msg = bug $ DeserializeBug ("invalid message: " <> show msg)

instance
  (ToJSON (MsgContent m 'Request)) =>
  Serialize (Message m 'Request)
  where
  serialize msg =
    -- zmq identity(ies)
    ""
      :| [
           -- delimiter
           "<IDS|MSG>",
           -- HMAC signature
           "",
           -- serialized header dict
           toStrict . encode $ msg ^. header,
           -- serialized parent header dict
           toStrict . encode $ msg ^. parentHeader,
           -- serialized metadata dict
           toStrict . encode $ msg ^. metadata,
           -- serialized content dict
           toStrict . encode $ msg ^. content
           -- extra raw data buffer(s)
         ]

data IOPubHeader e
  = IOPubHeader
      { iOPubHeaderMsgId :: Text,
        iOPubHeaderUsername :: Username,
        iOPubHeaderSession :: Text,
        iOPubHeaderDate :: UTCTime,
        iOPubHeaderMsgType :: SIOPubType e,
        iOPubHeaderVersion :: Text
      }
  deriving (Generic, Show)

makeFields ''IOPubHeader

instance (SingI e) => FromJSON (IOPubHeader e) where
  parseJSON = genericParseJSON (fieldsOptions 11)

instance GShow IOPubHeader where gshowsPrec = showsPrec

data StatusParentHeader
  = StatusParentHeader
      { statusParentHeaderMsgId :: Maybe UUID,
        statusParentHeaderUsername :: Maybe Username,
        statusParentHeaderSession :: Maybe Text,
        statusParentHeaderDate :: Maybe UTCTime,
        statusParentHeaderMsgType :: Maybe (MsgType 'Execute 'Request),
        statusParentHeaderVersion :: Maybe Text
      }
  deriving (Generic, Show)

makeFields ''StatusParentHeader

instance FromJSON StatusParentHeader where
  parseJSON = genericParseJSON (fieldsOptions 18)

data IOPubParentHeader
  = IOPubParentHeader
      { iOPubParentHeaderMsgId :: UUID,
        iOPubParentHeaderUsername :: Username,
        iOPubParentHeaderSession :: UUID,
        iOPubParentHeaderDate :: UTCTime,
        iOPubParentHeaderMsgType :: Text,
        iOPubParentHeaderVersion :: Text
      }
  deriving (Generic, Show)

makeFields ''IOPubParentHeader

instance ToJSON IOPubParentHeader where

  toJSON = genericToJSON (fieldsOptions 17)

  toEncoding = genericToEncoding (fieldsOptions 17)

instance FromJSON IOPubParentHeader where
  parseJSON = genericParseJSON (fieldsOptions 17)

data IOPubMessage e
  = IOPubMessage
      { iOPubMessageZmqIdent :: Text,
        iOPubMessageHeader :: IOPubHeader e,
        iOPubMessageParentHeader :: IOPubParentHeader,
        iOPubMessageMetadata :: HashMap Text A.Value,
        iOPubMessageContent :: IOPubContent e,
        iOPubMessageBuffers :: [Text]
      }
  deriving (Generic)

makeFields ''IOPubMessage

deriving instance (Show (IOPubContent e)) => Show (IOPubMessage e)

showsPrec' :: Int -> IOPubMessage e -> ShowS
showsPrec' i m =
  case iopubInsts (m ^. header . msgType) of
    IOPubInsts -> showsPrec i m

instance GShow IOPubMessage where
  gshowsPrec = showsPrec'

instance
  (SingI m, FromJSON (IOPubContent m)) =>
  FromJSON (IOPubMessage m)
  where
  parseJSON = genericParseJSON (fieldsOptions 12)

data IOPubInsts e where
  IOPubInsts ::
    ( c ~ IOPubContent e,
      Show c,
      FromJSON c
    ) =>
    IOPubInsts
      e

iopubInsts :: SIOPubType e -> IOPubInsts e
iopubInsts SStream = IOPubInsts
iopubInsts SStatus = IOPubInsts
iopubInsts SExecuteInput = IOPubInsts
iopubInsts SExecuteResult = IOPubInsts
iopubInsts SError = IOPubInsts

instance Deserialize (Some IOPubMessage) where
  deserialize (ids : "<IDS|MSG>" : _s : h : ph : md : c : bs) =
    let res = do
          h' <- eitherDecode $ toLazy h
          case lookup "msg_type" (h' :: A.Object) of
            Just (A.String t) -> do
              t' <- eitherDecode $ show t
              withSomeSing (t' :: IOPubType) go
            Just v -> Left ("expected string for \"msg_type\" found: " <> show v)
            Nothing -> Left "missing key \"msg_type\" in header"
     in case res of
          Right msg -> msg
          Left err -> bug $ DeserializeBug err
    where
      go :: forall e. SIOPubType e -> Either String (Some IOPubMessage)
      go e = case iopubInsts e of
        IOPubInsts -> do
          iOPubMessageHeader <- case singInstance e of
            SingInstance -> eitherDecode @(IOPubHeader e) $ toLazy h
          iOPubMessageParentHeader <- eitherDecode $ toLazy ph
          iOPubMessageMetadata <- eitherDecode $ toLazy md
          iOPubMessageContent <- eitherDecode $ toLazy c
          iOPubMessageBuffers <- traverse (eitherDecode . toLazy) bs
          pure $ Some IOPubMessage {iOPubMessageZmqIdent = decodeUtf8 ids, ..}
  deserialize msg = bug $ DeserializeBug ("invalid message: " <> show msg)

data MsgInsts e where
  MsgInsts ::
    ( cq ~ MsgContent e 'Request,
      cp ~ MsgContent e 'Reply,
      s ~ MessageSocket e,
      t ~ SocketType s,
      Show cq,
      Show cp,
      ToJSON cq,
      FromJSON cp,
      KernelSocket' s,
      SingI s,
      Sender t,
      Receiver t
    ) =>
    MsgInsts
      e

msgInsts :: SMessageType e -> MsgInsts e
msgInsts SShutdown = MsgInsts
msgInsts SExecute = MsgInsts
msgInsts SHeartbeat = MsgInsts
msgInsts SKernelInfo = MsgInsts

newHeader :: (MonadIO m) => Kernel z -> SMessageType e -> m (Header e 'Request)
newHeader kernel mt = do
  headerDate <- liftIO getCurrentTime
  headerMsgId <- nextJust $ liftIO nextUUID
  pure
    Header
      { headerUsername = kernel ^. username,
        headerSession = kernel ^. session,
        headerMsgType = MsgType mt SRequest,
        headerVersion = "5.0",
        ..
      }

newRequest ::
  (SingI e, MonadIO m) =>
  Kernel z ->
  MsgContent e 'Request ->
  m (Message e 'Request)
newRequest kernel c =
  withSing $ \m -> do
    messageHeader <- newHeader kernel m
    pure $
      Message
        { messageZmqIdent = "",
          messageParentHeader = (),
          messageMetadata = mempty,
          messageContent = c,
          messageBuffers = mempty,
          ..
        }

newHeartbeat ::
  MonadIO m => Kernel z -> m (Message 'Heartbeat 'Request)
newHeartbeat kernel = do
  messageHeader <- newHeader kernel SHeartbeat
  pure
    Message
      { messageZmqIdent = "",
        messageParentHeader = (),
        messageMetadata = mempty,
        messageContent = HeartbeatContent,
        messageBuffers = mempty,
        ..
      }
