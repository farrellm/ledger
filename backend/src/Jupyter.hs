{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Jupyter
  ( runKernelName,
  )
where

import Control.Concurrent.Async.Lifted (race)
import Control.Concurrent.Chan.Lifted (Chan, newChan, readChan, writeChan)
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.MVar.Lifted (modifyMVar_, withMVar)
import Control.Exception.Lifted (bracket, handle)
import Control.Lens
import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Map as M
import Data.Singletons (SingI, withSing)
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
import System.Posix.User (getEffectiveUserName)
import System.Process.Typed
  ( createPipe,
    getExitCode,
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
import Prelude hiding (ExecuteRequest, state, stdin)

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

getAllSpecs :: (MonadIO m) => m (Map Text KernelSpec)
getAllSpecs = liftIO $ do
  ks <- M.toList <$> findKernelSpecs
  ss <- forM ks $ \(k, d) -> do
    es <- eitherDecodeFileStrict' (d </> "kernel.json")
    case es of
      Left err -> error $ toText err
      Right s -> pure (k, s)
  pure $ M.fromList ss

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

withKernel ::
  (MonadIO m) =>
  KernelConfig ->
  KernelSpec ->
  KernelControl ->
  (forall z. Kernel z -> ZMQ z ()) ->
  m ()
withKernel conf spec kCtrl z =
  liftIO . withSystemTempDirectory "ledger-kernel" $ \tmp -> do
    putStrLn' tmp
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
    bracket
      -- (startProcess $ proc cmd (args <> one "--debug") & setStdin createPipe)
      (startProcess $ proc cmd args & setStdin createPipe)
      ( \p -> do
          getExitCode p >>= \case
            Nothing -> do
              putStrLn' "stopping kernel"
              stopProcess p
            _ -> pass
          putMVar (_done kCtrl) ()
      )
      $ \p ->
        raceDestruct $ runZMQ $ do
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
          putStrLn' ("#### kernel session: " <> show (kernel ^. session))
          void $ heartbeat kernel
          void $ communicate kernel KernelInfoRequest
          flushIOPub kernel
          res <- z kernel
          void $ communicate kernel ShutdownContent {_restart = False}
          print' =<< waitExitCode p
          pure res
  where
    raceDestruct :: IO () -> IO ()
    raceDestruct x = either id id <$> race (readMVar $ _destruct kCtrl) x
    --
    flushIOPub :: Kernel z -> ZMQ z ()
    flushIOPub kernel = do
      ess <- poll 100 [Sock (kernel ^. iopub) [In] Nothing]
      case ess of
        [[]] -> pass
        _ -> do
          void $ receiveMulti (kernel ^. iopub)
          flushIOPub kernel

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

execute :: Kernel z -> MsgContent 'Execute 'Request -> ZMQ z (Chan KernelOutput)
execute kernel c = do
  msg <- newRequest kernel c
  let soc = messageSocket SExecute kernel
      mId = msg ^. header . msgId
  exOut <- newChan
  modifyMVar_ (kernel ^. output) (pure . M.insert mId exOut)
  putStrLn' (">>>> " <> show (msg ^. content))
  sendMulti soc (serialize msg)
  res <- deserialize @(Message 'Execute 'Reply) <$> receiveMulti soc
  putStrLn' ("<<<< " <> show (res ^. content))
  pure exOut

heartbeat :: Kernel z -> ZMQ z UTCTime
heartbeat kernel = do
  let soc = messageSocket SHeartbeat kernel
  msg <- newHeartbeat kernel
  sendMulti soc (serialize msg)
  void $ receiveMulti soc
  t <- liftIO getCurrentTime
  putStrLn' ("#### " <> show t)
  pure t

runKernel :: (MonadIO m) => KernelSpec -> KernelControl -> m ()
runKernel spec kCtrl = do
  conf <- liftIO . mkConf $ S.tupleToHostAddress (127, 0, 0, 1)
  withKernel conf spec kCtrl $ \kernel ->
    withWorker (runIOPub kernel) $
      runShell kernel =<< readChan (_in kCtrl)
  where
    runShell :: Kernel z -> KernelInput -> ZMQ z ()
    runShell _ KernelShutdown =
      putStrLn' "#### shutting down kernel"
    runShell kernel (KernelExecute cUUID expr) = do
      eOut <-
        execute
          kernel
          ExecuteRequest
            { _code = expr,
              _silent = False,
              _storeHistory = True,
              _userExpressions = mempty,
              _allowStdin = False,
              _stopOnError = True
            }
      let go = do
            x <- readChan eOut
            putStrLn' ("|||| " <> show x)
            writeChan (_out kCtrl) (cUUID, x)
            case x of
              KernelDone _ -> putStrLn' "#### execute done"
              _ -> go
      void $ fork go
      runShell kernel =<< readChan (_in kCtrl)
    --
    runIOPub :: Kernel z -> ZMQ z Void
    runIOPub kernel =
      forever . void $ do
        -- putStrLn' "~~~~"
        io <- receiveMulti (kernel ^. iopub)
        -- print' io
        handle (iopubBug io) $ case deserialize @(Some IOPubMessage) io of
          Some msg -> case msg ^. header . msgType of
            SStatus -> do
              let s = msg ^. content . executionState
              putStrLn' ("~~~~ " <> show s)
              void $ swapMVar (kernel ^. state) s
              when (s == Idle)
                $ withMVar (kernel ^. output)
                $ \m ->
                  whenJust (flip M.lookup m $ msg ^. parentHeader . msgId) $ \c ->
                    writeChan c (KernelDone (msg ^. header . msgId))
            SExecuteInput -> pass
            SExecuteResult ->
              withMVar (kernel ^. output) $ \m ->
                whenJust (M.lookup (msg ^. parentHeader . msgId) m) $ \c ->
                  for_ (M.toList $ msg ^. content . data_) $ \case
                    ("text/plain", d) -> writeChan c (KernelResult (msg ^. header . msgId) d)
                    (t, _) -> putStrLn' ("|||| unknown mime type: " <> toString t)
            SStream ->
              withMVar (kernel ^. output) $ \m ->
                whenJust (m ^. at (msg ^. parentHeader . msgId)) $ \c ->
                  if msg ^. content . name == "stdout"
                    then writeChan c (KernelStdout (msg ^. header . msgId) (msg ^. content . text))
                    else putStrLn' ("|||| unknown stream: " <> show msg)
            SError ->
              withMVar (kernel ^. output) $ \m ->
                whenJust (m ^. at (msg ^. parentHeader . msgId)) $ \c ->
                  writeChan
                    c
                    ( KernelError
                        (msg ^. header . msgId)
                        (msg ^. content . traceback)
                        (msg ^. content . ename)
                        (msg ^. content . evalue)
                    )
    --
    iopubBug io e =
      putStrLn' . toString $
        unlines
          [ "|||| iopub error: " <> show (e :: DeserializeBug),
            "on " <> show io
          ]

runKernelName :: (MonadIO m) => Text -> KernelControl -> m ()
runKernelName kernelName kCtrl = do
  ss <- getAllSpecs
  spec <-
    maybe (die $ "no kernel: " <> toString kernelName) pure $
      ss !? kernelName
  runKernel spec kCtrl
-- test :: IO ()
-- test = runKernelName "python3"
