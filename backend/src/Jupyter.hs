{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Jupyter where

import Control.Concurrent.Async.Lifted (race)
import Control.Concurrent.Chan.Lifted
import Control.Concurrent.Lifted (threadDelay)
import Control.Concurrent.MVar.Lifted (modifyMVar_, withMVar)
import Control.Exception.Lifted
  ( bracket,
    handle,
  )
import Control.Lens
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Map as M
import Data.Singletons
  ( SingI,
    withSing,
  )
import Data.Some (Some (..))
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID.V1 (nextUUID)
import Jupyter.Types
import qualified Network.Socket as S
import Network.Socket (HostAddress, PortNumber)
import Relude.Extra.Map ((!?))
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getHomeDirectory,
    listDirectory,
  )
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.User (getEffectiveUserName)
import System.Process.Typed
  ( createPipe,
    getStdin,
    proc,
    setStdin,
    startProcess,
    stopProcess,
    waitExitCode,
  )
import System.ZMQ4.Monadic hiding (SocketType)
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.String (compile, regexec)
import Prelude hiding (state, stdin)

baton :: MVar ()
baton = unsafePerformIO $ newMVar ()
{-# NOINLINE baton #-}

print' :: (MonadBaseControl IO m, MonadIO m, Show a) => a -> m ()
print' a = withMVar baton $ \() -> print a

putStrLn' :: (MonadBaseControl IO m, MonadIO m) => String -> m ()
putStrLn' a = withMVar baton $ \() -> putStrLn a

kernelDirectories :: [FilePath]
kernelDirectories = ["/usr/share/jupyter/kernels"]

findKernelSpecs :: IO (Map Text FilePath)
findKernelSpecs = do
  home <- getHomeDirectory
  let kds = (home </> ".local/share/jupyter/kernels") : kernelDirectories
  ps <- forM kds $ \kd -> do
    ks <- filterM (isKernelDir . (kd </>)) =<< listDirectory kd
    pure $ (\k -> (T.toLower $ toText k, kd </> k)) <$> ks
  pure . M.fromList $ concat ps
  where
    isKernelDir p = doesDirectoryExist p &&^ doesFileExist (p </> "kernel.json")

getAllSpecs :: IO (Map Text KernelSpec)
getAllSpecs = do
  ks <- M.toList <$> findKernelSpecs
  ss <- forM ks $ \(k, d) -> do
    es <- eitherDecodeFileStrict' (d </> "kernel.json")
    case es of
      Left err -> error $ toText err
      Right s -> pure (k, s)
  pure $ M.fromList ss

startKernel :: KernelSpec -> IO ()
startKernel _ks = pass

makeUrl :: PortNumber -> String
makeUrl port = "tcp://127.0.0.1:" <> show port

renderAddress :: HostAddress -> Text
renderAddress addr =
  let (a, b, c, d) = S.hostAddressToTuple addr
   in T.intercalate "." $ show <$> [a, b, c, d]

withNetworkSocket :: (S.Socket -> IO a) -> IO a
withNetworkSocket = bracket (S.socket S.AF_INET S.Stream S.defaultProtocol) S.close'

mkConf :: HostAddress -> IO KernelConfig
mkConf ip_ =
  withNetworkSocket $ \shellSock -> do
    S.bind shellSock $ S.SockAddrInet 0 ip_
    withNetworkSocket $ \iopubSock -> do
      S.bind iopubSock $ S.SockAddrInet 0 ip_
      withNetworkSocket $ \stdinSock -> do
        S.bind stdinSock $ S.SockAddrInet 0 ip_
        withNetworkSocket $ \controlSock -> do
          S.bind controlSock $ S.SockAddrInet 0 ip_
          withNetworkSocket $ \hbSock ->
            do
              -- S.setSocketOption sock S.Linger 0
              S.bind hbSock $ S.SockAddrInet 0 ip_
              _shellPort <- S.socketPort shellSock
              _iopubPort <- S.socketPort iopubSock
              _stdinPort <- S.socketPort stdinSock
              _controlPort <- S.socketPort controlSock
              _hbPort <- S.socketPort hbSock
              let _ip = renderAddress ip_
                  _key = ""
                  _transport = "tcp"
              pure KernelConfig {..}

withWorker :: (MonadBaseControl IO m) => m Void -> m a -> m a
withWorker worker cont = either absurd id <$> race worker cont

withKernel ::
  KernelConfig ->
  KernelSpec ->
  (forall z. Kernel z -> ZMQ z a) ->
  IO a
withKernel conf spec z =
  withSystemTempDirectory "ledger-kernel" $ \tmp -> do
    putStrLn' tmp
    -- print' ss
    let cf = tmp </> "kernel.json"
        ns = M.fromList [("connection_file", cf)]
    writeFileLBS cf $ encodePretty conf
    pat <-
      either (die . ("invalid regex: " <>)) pure $
        compile defaultCompOpt defaultExecOpt "\\{([A-Za-z0-9_]+)\\}"
    let (cmd :| args) = _argv spec <&> \s ->
          case regexec pat s of
            Right (Just (a, _, b, [k])) -> maybe s (\m -> a <> m <> b) $ ns !? k
            _ -> s
        lingerTime = restrict (1000 :: Int)
    print' (cmd :| args)
    runZMQ
      $ bracket
        -- (startProcess $ proc cmd (args <> one "--debug") & setStdin createPipe)
        (startProcess $ proc cmd args & setStdin createPipe)
        stopProcess
      $ \p -> do
        liftIO $ hClose (getStdin p)
        shell_ <- socket Dealer
        iopub_ <- socket Sub
        stdin_ <- socket Dealer
        control_ <- socket Dealer
        hb_ <- socket Req
        setLinger lingerTime shell_
        setLinger lingerTime iopub_
        setLinger lingerTime stdin_
        setLinger lingerTime control_
        setLinger lingerTime hb_
        connect shell_ $ makeUrl (_shellPort conf)
        connect iopub_ $ makeUrl (_iopubPort conf)
        connect stdin_ $ makeUrl (_stdinPort conf)
        connect control_ $ makeUrl (_controlPort conf)
        connect hb_ $ makeUrl (_hbPort conf)
        subscribe iopub_ ""
        threadDelay 1000000
        --
        _kernelUsername <- Username <$> liftIO getEffectiveUserName
        _kernelSession <- liftIO $ nextJust nextUUID
        _kernelState <- newMVar Idle
        _kernelOutput <- newMVar mempty
        let kernel =
              Kernel
                { _kernelIp = _ip conf,
                  _kernelShell = shell_,
                  _kernelIopub = iopub_,
                  _kernelStdin = stdin_,
                  _kernelControl = control_,
                  _kernelHb = hb_,
                  ..
                }
        void $ heartbeat kernel
        -- void $ communicate kernel KernelInfo {_restart = False}
        res <- z kernel
        void $ communicate kernel ShutdownContent {_restart = False}
        print' =<< waitExitCode p
        pure res

communicate ::
  forall m z.
  (SingI m) =>
  Kernel z ->
  MsgContent m 'Request ->
  ZMQ z (Message m 'Reply)
communicate kernel c =
  withSing @m $ \m ->
    case msgInsts m of
      MsgInsts -> do
        let soc = messageSocket m kernel
        msg <- newRequest kernel c
        putStrLn' (">>>> " <> show (msg ^. content))
        sendMulti soc (serialize msg)
        res <- deserialize <$> receiveMulti soc
        putStrLn' ("<<<< " <> show (res ^. content))
        pure res

execute :: Kernel z -> MsgContent 'Execute 'Request -> ZMQ z (Message 'Execute 'Reply)
execute kernel c = do
  let soc = messageSocket SExecute kernel
  msg <- newRequest kernel c
  modifyMVar_ (kernel ^. output) $ \m ->
    M.insert (msg ^. header . msgId) <$> newChan <*> pure m
  putStrLn' (">>>> " <> show (msg ^. content))
  sendMulti soc (serialize msg)
  res <- deserialize <$> receiveMulti soc
  putStrLn' ("<<<< " <> show (res ^. content))
  pure res

heartbeat :: Kernel z -> ZMQ z UTCTime
heartbeat kernel = do
  let soc = messageSocket SHeartbeat kernel
  msg <- newHeartbeat kernel
  sendMulti soc (serialize msg)
  void $ receiveMulti soc
  t <- liftIO getCurrentTime
  putStrLn' ("#### " <> show t)
  pure t

runKernel :: KernelConfig -> KernelSpec -> IO ()
runKernel conf spec =
  withKernel conf spec $ \kernel ->
    withWorker (runIOPub kernel) $ do
      putStrLn' ("#### kernel session: " <> show (kernel ^. session))
      msg <-
        execute
          kernel
          ExecuteRequest
            { _code = unlines ["print('sup')", "8"],
              _silent = False,
              _storeHistory = True,
              _userExpressions = mempty,
              _allowStdin = False,
              _stopOnError = True
            }
      out <- readMVar (kernel ^. output)
      whenJust (out ^. at (msg ^. parentHeader . msgId)) $ \c ->
        let go = do
              x <- readChan c
              putStrLn' ("<<<< " <> show x)
              unless (x == KernelDone) go
         in go
  where
    runIOPub :: Kernel z -> ZMQ z Void
    runIOPub kernel =
      forever . void $
        poll 10 [Sock (kernel ^. iopub) [In] (Just $ receiveIOPub kernel)]
    receiveIOPub :: Kernel z -> [Event] -> ZMQ z ()
    receiveIOPub kernel _ = do
      io <- receiveMulti (kernel ^. iopub)
      handle (iopubBug io) $ case deserialize @(Some IOPubMessage) io of
        Some msg -> case msg ^. header . msgType of
          SStatus -> do
            let s = msg ^. content . executionState
            void $ swapMVar (kernel ^. state) s
            when (s == Idle)
              $ withMVar (kernel ^. output)
              $ \m ->
                whenJust (flip M.lookup m =<< msg ^. parentHeader . msgId) $ \c ->
                  writeChan c KernelDone
          SExecuteInput -> pass
          SExecuteResult ->
            withMVar (kernel ^. output) $ \m ->
              whenJust (M.lookup (msg ^. parentHeader . msgId) m) $ \c ->
                for_ (M.toList $ msg ^. content . data_) $ \case
                  ("text/plain", d) -> writeChan c (KernelResult d)
                  (t, _) -> putStrLn' ("|||| unknown mime type: " <> toString t)
          SStream ->
            withMVar (kernel ^. output) $ \m ->
              whenJust (m ^. at (msg ^. parentHeader . msgId)) $ \c ->
                if msg ^. content . name == "stdout"
                  then writeChan c (KernelStdout (msg ^. content . text))
                  else putStrLn' ("|||| unknown stream: " <> show msg)
    iopubBug io e =
      putStrLn' . toString $
        unlines
          [ "iopub error: " <> show (e :: DeserializeBug),
            "on " <> show io
          ]

test :: IO ()
test = do
  conf <- mkConf $ S.tupleToHostAddress (127, 0, 0, 1)
  let kernelName = "python3"
  ss <- getAllSpecs
  spec <-
    maybe (die $ "no kernel: " <> toString kernelName) pure $
      ss !? kernelName
  print' spec
  putStrLn' . decodeUtf8 $ encodePretty conf
  runKernel conf spec
