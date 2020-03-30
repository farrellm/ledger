{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import System.ZMQ4.Monadic (Receiver, Sender)
import System.ZMQ4.Monadic (Socket)
import Text.Casing (fromHumps, toQuietSnake)
import Text.Show (ShowS, showsPrec)

toSnake :: Text -> Text
toSnake = toText . toQuietSnake . fromHumps . toString

data MsgType m r = MsgType (SMessageType m) (SReqRep r)
  deriving (Generic, Show)

instance ToJSON (MsgType m 'Request) where
  toJSON (MsgType m _) = A.String (toSnake (show $ fromSing m) <> "_request")

instance (SingI m, SingI r) => FromJSON (MsgType m r) where
  parseJSON o =
    withSing @m $ \m -> withSing @r $ \r ->
      let p = toSnake (show (fromSing m) <> show (fromSing r))
          err = "expected \"" <> toString p <> "\", but encountedered " <> show o
       in case o of
            A.String v | p == v -> pure $ MsgType m r
            _ -> fail err

instance Field1 (MsgType m r) (MsgType m' r) (SMessageType m) (SMessageType m')

instance Field2 (MsgType m r) (MsgType m r') (SReqRep r) (SReqRep r')

type family MsgUUID (r :: ReqRep) where
  MsgUUID 'Request = UUID
  MsgUUID 'Reply = Text

data Header (m :: MessageType) (r :: ReqRep)
  = Header
      { _msgId :: MsgUUID r,
        _username :: Username,
        _session :: MsgUUID r,
        _date :: UTCTime,
        _msgType :: MsgType m r,
        _version :: Text
      }
  deriving (Generic)

makeFieldsNoPrefix ''Header

deriving instance Show (Header m 'Request)

deriving instance Show (Header m 'Reply)

instance ToJSON (Header m 'Request) where

  toJSON = genericToJSON customOptions

  toEncoding = genericToEncoding customOptions

instance (SingI m) => FromJSON (Header m 'Reply) where
  parseJSON = genericParseJSON customOptions

instance (SingI m) => FromJSON (Header m 'Request) where
  parseJSON = genericParseJSON customOptions

type family ParentHeader (m :: MessageType) (r :: ReqRep) where
  ParentHeader m 'Request = ()
  ParentHeader m 'Reply = Header m 'Request

data Message (m :: MessageType) (r :: ReqRep)
  = Message
      { _zmqIdent :: Text,
        _header :: Header m r,
        _parentHeader :: ParentHeader m r,
        _metadata :: HashMap Text A.Value,
        _content :: MsgContent m r,
        _buffers :: [Text]
      }
  deriving (Generic)

makeFieldsNoPrefix ''Message

deriving instance
  ( Show (Header m r),
    Show (ParentHeader m r),
    Show (MsgContent m r)
  ) =>
  Show (Message m r)

instance (ToJSON (MsgContent m 'Request)) => ToJSON (Message m 'Request) where

  toJSON = genericToJSON customOptions

  toEncoding = genericToEncoding customOptions

instance (SingI m, FromJSON (MsgContent m 'Reply)) => FromJSON (Message m 'Reply) where
  parseJSON = genericParseJSON customOptions

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
          _header <- eitherDecode $ toLazy h
          _parentHeader <- eitherDecode $ toLazy ph
          _metadata <- eitherDecode $ toLazy md
          _content <- eitherDecode $ toLazy c
          _buffers <- traverse (eitherDecode . toLazy) bs
          pure Message {_zmqIdent = decodeUtf8 ids, ..}
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
      { _msgId :: Text,
        _username :: Username,
        _session :: Text,
        _date :: UTCTime,
        _msgType :: SIOPubType e,
        _version :: Text
      }
  deriving (Generic, Show)

makeFieldsNoPrefix ''IOPubHeader

instance (SingI e) => FromJSON (IOPubHeader e) where
  parseJSON = genericParseJSON customOptions

instance GShow IOPubHeader where gshowsPrec = showsPrec

data StatusParentHeader
  = StatusParentHeader
      { _msgId :: Maybe UUID,
        _username :: Maybe Username,
        _session :: Maybe Text,
        _date :: Maybe UTCTime,
        _msgType :: Maybe (MsgType 'Execute 'Request),
        _version :: Maybe Text
      }
  deriving (Generic, Show)

makeFieldsNoPrefix ''StatusParentHeader

instance FromJSON StatusParentHeader where
  parseJSON = genericParseJSON customOptions

type family IOPubParentHeader (m :: IOPubType) where
  IOPubParentHeader 'Status = StatusParentHeader
  IOPubParentHeader m = Header 'Execute 'Request

data IOPubMessage e
  = IOPubMessage
      { _zmqIdent :: Text,
        _header :: IOPubHeader e,
        _parentHeader :: IOPubParentHeader e,
        _metadata :: HashMap Text A.Value,
        _content :: IOPubContent e,
        _buffers :: [Text]
      }
  deriving (Generic)

makeFieldsNoPrefix ''IOPubMessage

deriving instance
  (Show (IOPubParentHeader e), Show (IOPubContent e)) =>
  Show (IOPubMessage e)

showsPrec' :: Int -> IOPubMessage e -> ShowS
showsPrec' i m =
  case iopubInsts (m ^. header . msgType) of
    IOPubInsts -> showsPrec i m

instance GShow IOPubMessage where
  gshowsPrec = showsPrec'

instance
  (SingI m, FromJSON (IOPubParentHeader m), FromJSON (IOPubContent m)) =>
  FromJSON (IOPubMessage m)
  where
  parseJSON = genericParseJSON customOptions

data IOPubInsts e where
  IOPubInsts ::
    ( c ~ IOPubContent e,
      Show c,
      FromJSON c,
      Show (IOPubParentHeader e),
      FromJSON (IOPubParentHeader e)
    ) =>
    IOPubInsts
      e

iopubInsts :: SIOPubType e -> IOPubInsts e
iopubInsts SStream = IOPubInsts
iopubInsts SStatus = IOPubInsts
iopubInsts SExecuteInput = IOPubInsts
iopubInsts SExecuteResult = IOPubInsts

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
          -- c' <- eitherDecode $ toLazy c
          -- traceShowM (c' :: A.Object)
          _header <- case singInstance e of
            SingInstance -> eitherDecode @(IOPubHeader e) $ toLazy h
          _parentHeader <- eitherDecode $ toLazy ph
          _metadata <- eitherDecode $ toLazy md
          _content <- eitherDecode $ toLazy c
          _buffers <- traverse (eitherDecode . toLazy) bs
          pure $ Some IOPubMessage {_zmqIdent = decodeUtf8 ids, ..}
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

newHeader :: (MonadIO m) => Kernel z -> SMessageType e -> m (Header e 'Request)
newHeader kernel mt = do
  _date <- liftIO getCurrentTime
  _msgId <- nextJust $ liftIO nextUUID
  pure
    Header
      { _username = kernel ^. username,
        _session = kernel ^. session,
        _msgType = MsgType mt SRequest,
        _version = "5.0",
        ..
      }

newRequest ::
  (SingI e, MonadIO m) =>
  Kernel z ->
  MsgContent e 'Request ->
  m (Message e 'Request)
newRequest kernel c =
  withSing $ \m -> do
    _header <- newHeader kernel m
    pure $
      Message
        { _zmqIdent = "",
          _parentHeader = (),
          _metadata = mempty,
          _content = c,
          _buffers = mempty,
          ..
        }

newHeartbeat ::
  MonadIO m => Kernel z -> m (Message 'Heartbeat 'Request)
newHeartbeat kernel = do
  _header <- newHeader kernel SHeartbeat
  pure
    Message
      { _zmqIdent = "",
        _parentHeader = (),
        _metadata = mempty,
        _content = HeartbeatContent,
        _buffers = mempty,
        ..
      }
