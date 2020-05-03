{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Jupyter.Types.Kernel where

import Control.Concurrent.Chan.Lifted (Chan)
import Control.Lens (makeFields)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    genericParseJSON,
    genericToEncoding,
    genericToJSON,
  )
import Data.UUID (UUID)
import Jupyter.Types.Common
import Ledger.Orphans ()
import Network.Socket (PortNumber)
import System.ZMQ4.Monadic (Dealer, Req, Socket, Sub)

data KernelSpec
  = KernelSpec
      { kernelSpecDisplayName :: Text,
        kernelSpecLanguage :: Text,
        kernelSpecArgv :: NonEmpty String
      }
  deriving (Generic, Show)

makeFields ''KernelSpec

instance FromJSON KernelSpec where
  parseJSON = genericParseJSON (fieldsOptions 10)

data KernelConfig
  = KernelConfig
      { kernelConfigShellPort :: PortNumber,
        kernelConfigIopubPort :: PortNumber,
        kernelConfigStdinPort :: PortNumber,
        kernelConfigControlPort :: PortNumber,
        kernelConfigHbPort :: PortNumber,
        kernelConfigIp :: Text,
        kernelConfigKey :: Text,
        kernelConfigTransport :: Text
      }
  deriving (Generic, Show)

makeFields ''KernelConfig

instance ToJSON KernelConfig where

  toJSON = genericToJSON (fieldsOptions 12)

  toEncoding = genericToEncoding (fieldsOptions 12)

data Kernel z
  = Kernel
      { kernelIp :: Text,
        kernelUsername :: Username,
        kernelSession :: UUID,
        kernelShell :: Socket z Dealer,
        kernelIopub :: Socket z Sub,
        kernelStdin :: Socket z Dealer,
        kernelControl :: Socket z Dealer,
        kernelHb :: Socket z Req,
        kernelState :: MVar ExecutionState,
        kernelOutput :: MVar (Map UUID (Chan KernelOutput))
      }
  deriving (Generic)

makeFields ''Kernel

type family SocketType (s :: KernelSocket) where
  SocketType 'Shell = Dealer
  SocketType 'IOPub = Sub
  SocketType 'Stdin = Dealer
  SocketType 'Control = Dealer
  SocketType 'Hb = Req

class KernelSocket' s where
  kernelSocket :: SKernelSocket s -> Kernel z -> Socket z (SocketType s)

instance KernelSocket' 'Shell where
  kernelSocket _ = kernelShell

instance KernelSocket' 'IOPub where
  kernelSocket _ = kernelIopub

instance KernelSocket' 'Stdin where
  kernelSocket _ = kernelStdin

instance KernelSocket' 'Control where
  kernelSocket _ = kernelControl

instance KernelSocket' 'Hb where
  kernelSocket _ = kernelHb
