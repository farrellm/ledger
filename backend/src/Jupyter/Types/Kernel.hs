{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Jupyter.Types.Kernel where

import Control.Concurrent.Chan.Lifted (Chan)
import Control.Lens.TH
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
      { _displayName :: Text,
        _language :: Text,
        _argv :: NonEmpty String
      }
  deriving (Generic, Show)

instance FromJSON KernelSpec where
  parseJSON = genericParseJSON customOptions

data KernelConfig
  = KernelConfig
      { _shellPort :: PortNumber,
        _iopubPort :: PortNumber,
        _stdinPort :: PortNumber,
        _controlPort :: PortNumber,
        _hbPort :: PortNumber,
        _ip :: Text,
        _key :: Text,
        _transport :: Text
      }
  deriving (Generic, Show)

instance ToJSON KernelConfig where

  toJSON = genericToJSON customOptions

  toEncoding = genericToEncoding customOptions

data KernelOutput
  = KernelStdout Text
  | KernelResult Text
  | KernelDone
  deriving (Show, Eq)

data Kernel z
  = Kernel
      { _kernelIp :: Text,
        _kernelUsername :: Username,
        _kernelSession :: UUID,
        _kernelShell :: Socket z Dealer,
        _kernelIopub :: Socket z Sub,
        _kernelStdin :: Socket z Dealer,
        _kernelControl :: Socket z Dealer,
        _kernelHb :: Socket z Req,
        _kernelState :: MVar ExecutionState,
        _kernelOutput :: MVar (Map UUID (Chan KernelOutput))
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
  kernelSocket _ = _kernelShell

instance KernelSocket' 'IOPub where
  kernelSocket _ = _kernelIopub

instance KernelSocket' 'Stdin where
  kernelSocket _ = _kernelStdin

instance KernelSocket' 'Control where
  kernelSocket _ = _kernelControl

instance KernelSocket' 'Hb where
  kernelSocket _ = _kernelHb
