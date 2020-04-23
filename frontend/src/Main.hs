{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Common
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Lens hiding ((#), unsnoc)
import Control.Monad.Fix (MonadFix)
import Data.Aeson (ToJSON, encode)
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A
import Data.Char (isAlpha, isAlphaNum, isSpace)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Traversable (for)
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as U
import Data.Witherable (Filterable, catMaybes, mapMaybe)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Types (JSM, liftJSM)
import Ledger.JS
import Ledger.Types
import Reflex.Dom.Core
import Relude hiding (catMaybes, error, mapMaybe, stdout)
import qualified Relude as R
import Relude.Extra.Map
import System.Random (randomIO)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

filterDuplicates ::
  (Reflex t, Eq a, MonadHold t m, MonadFix m) =>
  Event t a ->
  m (Event t a)
filterDuplicates e = do
  d <- holdDyn Nothing $ Just <$> e
  d' <- holdUniqDyn d
  let e' = updated d'
  pure (catMaybes e')

newChan :: (MonadIO m) => m (Chan a)
newChan = liftIO Chan.newChan

readChan :: (MonadIO m) => Chan a -> m a
readChan = liftIO . Chan.readChan

writeChan :: (MonadIO m) => Chan a -> a -> m ()
writeChan c a = liftIO $ Chan.writeChan c a

withMVar :: (MonadIO m) => MVar a -> (a -> m b) -> m b
withMVar m f = do
  x <- takeMVar m
  r <- f x
  putMVar m x
  pure r

modifyMVar :: MonadIO m => MVar a -> (a -> a) -> m ()
modifyMVar m f = f <$> takeMVar m >>= putMVar m

fork :: MonadIO m => IO () -> m ()
fork = void . liftIO . forkIO

postJson' :: (ToJSON a) => Text -> a -> XhrRequest ByteString
postJson' url a =
  XhrRequest "POST" url $
    def
      { _xhrRequestConfig_headers = headerUrlEnc,
        _xhrRequestConfig_sendData = body
      }
  where
    headerUrlEnc = "Content-type" =: "text/plain"
    body = toStrict $ encode a

main :: IO ()
main =
  runLedger 8001 $
    mainWidgetWithHead htmlHead htmlBody

takeLeft :: Filterable f => f (Either a b) -> f a
takeLeft = mapMaybe leftToMaybe

takeRight :: Filterable f => f (Either a b) -> f b
takeRight = mapMaybe rightToMaybe

nextUUID :: (Reflex t, MonadIO (PushM t)) => Event t a -> Event t UUID
nextUUID = push (const $ Just <$> liftIO randomIO)

htmlHead :: MonadWidget t m => m ()
htmlHead = do
  elAttr
    "meta"
    ( "name" =: "viewport"
        <> "content" =: "width=device-width, initial-scale=1, maximum-scale=1.0, user-scalable=no"
    )
    blank
  el "title" $ text "Ledger"
  elAttr
    "script"
    ( "type" =: "text/javascript"
        <> "src" =: "fontawesome/js/all.js"
        <> "defer" =: ""
    )
    blank
  elAttr
    "script"
    ( "type" =: "text/javascript"
        <> "src" =: "codemirror/lib/codemirror.js"
        <> "async" =: "false"
    )
    blank
  elAttr
    "link"
    ( "rel" =: "stylesheet"
        <> "type" =: "text/css"
        <> "href" =: "codemirror/lib/codemirror.css"
    )
    blank
  elAttr
    "link"
    ( "rel" =: "stylesheet"
        <> "type" =: "text/css"
        <> "href" =: "bulma/css/bulma.min.css"
    )
    blank
  elAttr
    "link"
    ( "href" =: "main.css"
        <> "type" =: "text/css"
        <> "rel" =: "stylesheet"
    )
    blank

htmlBody ::
  forall t m.
  ( MonadWidget t m,
    MonadIO (PushM t),
    DomRenderHook t m
  ) =>
  m ()
htmlBody = do
  url' <-
    liftJSM $
      valToText =<< jsg ("window" :: Text) ! ("location" :: Text) ! ("href" :: Text)
  let url = url' =~ ("https?://[^:]+" :: Text) :: Text
  print' url
  ls <-
    LedgerState <$> newIORef Nothing -- _kernelUUID
      <*> newMVar [] --                 _uuids
      <*> newIORef mempty --            _label
      <*> newIORef mempty --            _parameters
      <*> newIORef mempty --            _code
      <*> newIORef mempty --            _result
      <*> newIORef mempty --            _stdout
      <*> newIORef mempty --            _error
      <*> newIORef mempty --            _dirty
      <*> newChan --                    _queue
      <*> newMVar False --              _stop
      <*> newEmptyMVar --               _ready
      -- manage new kernel requests
  (evNewKernel', triggerNewKernel) <- newTriggerEvent
  evNewKernel'' <- getAndDecode (evNewKernel' $> (url <> ":8000/new_kernel"))
  evNewKernel <- NewKernel <<$>> filterDuplicates evNewKernel''
  liftIO $ triggerNewKernel ()
  (evDeadKernel', triggerDeadKernel) <- newTriggerEvent
  let evDeadKernel = evDeadKernel' $> DeadKernel
  -- manage execute requests
  evExRes <- do
    (evExecute, triggerExecute) <- newTriggerEvent
    fork . forever $ do
      putStrLn' "#### waiting for ready"
      takeMVar (ls ^. ready)
      putStrLn' "#### ready!"
      q <- readChan (ls ^. queue)
      case q of
        Nothing -> void $ swapMVar (ls ^. stop) False
        Just (u, c) -> triggerExecute (u, c)
    evExecute' <- performEvent $
      evExecute <&> \(u, c) ->
        readIORef (ls ^. kernelUUID) >>= \case
          Just k -> pure $ Just $ ExecuteRequest k u c
          Nothing -> pure Nothing
    evX <-
      performRequestAsync $
        catMaybes evExecute' <&> postJson' (url <> ":8000/execute")
    filterDuplicates (evX $> RunningCell)
  -- manage kernel output requests
  (evNewRequest, triggerNewRequest) <- newTriggerEvent
  evNewRequest' <- performEvent $
    evNewRequest <&> \() ->
      readIORef (ls ^. kernelUUID) >>= \case
        Nothing -> do
          liftIO $ putStrLn' "request fail 1"
          pure Nothing
        Just k -> pure . Just $ ResultRequest k
  evOutput' <-
    performRequestAsync $
      catMaybes evNewRequest' <&> postJson' (url <> ":8000/result")
  evOutput'' <- filterDuplicates $ fmap decodeXhrResponse evOutput'
  evOutput''' <- performEvent $
    evOutput'' <&> \case
      Nothing -> do
        liftIO $ do
          putStrLn' "request fail 2"
          triggerDeadKernel ()
        pure Nothing
      Just (c :: UUID, r :: KernelOutput) -> do
        liftIO $ triggerNewRequest ()
        pure . Just $ Output c r
  let evOutput = catMaybes evOutput'''
  -- manage code changes
  (evCodeChange, triggerCodeChange) <- newTriggerEvent
  liftJSM
    . (global <# ("onCodeChange" :: Text))
    . fun
    $ \_ _ [u] -> valToText u >>= liftIO . triggerCodeChange
  evCodeChange' <- debounce 0.2 evCodeChange
  let evCode = UpdateCode <$> catMaybes (U.fromText <$> evCodeChange')
  --
  rec evStartKernel <- navbar dynKernel
      (evCells, evAddEnd) <- divClass "section" . divClass "container" $ do
        dynEvCells <- simpleList dynSnapshotCells $ \dynCode -> do
          dynCell <- holdUniqDyn (extractCell <$> dynCode <*> dynResults)
          evEvCell <- dyn (cell dynCell <$> dynCode)
          switchHold never evEvCell
        evCells' <- switchHold never $ leftmost <$> updated dynEvCells
        (elAdd, _) <-
          elAttr' "button" ("class" =: "button" <> "type" =: "button")
            . elAttr "span" ("class" =: "icon")
            $ elAttr "i" ("class" =: "fas fa-plus") blank
        let evAdd = domEvent Click elAdd
            evUUID' = AddCellEnd <$> nextUUID evAdd
        pure (evCells', evUUID')
      void . dyn $
        dynKernel <&> \case
          Nothing -> blank
          Just _ ->
            elAttr
              "script"
              ( "type" =: "text/javascript"
                  <> "src" =: "codemirror/mode/python/python.js"
                  <> "defer" =: ""
                  <> "async" =: "false"
              )
              blank
      --
      evKernel <-
        performEvent $
          ( refreshState ls
              >=> kernelUpdate ls triggerNewKernel triggerNewRequest
              >=> \_ -> readIORef (ls ^. kernelUUID)
          )
            <$> leftmost [evNewKernel, evDeadKernel, evStartKernel]
      dynKernel <- holdDyn Nothing evKernel
      --
      evSnapshotCells <-
        performEvent $
          (refreshState ls >=> ledgerUpdate ls >=> snapshotCells ls)
            <$> leftmost [evAddEnd, takeRight evCells]
      dynSnapshotCells <- holdUniqDyn =<< holdDyn [] evSnapshotCells
      --
      evResults <-
        performEvent $
          ( refreshState ls
              >=> resultsUpdate ls triggerNewKernel
              >=> snapshotResults ls
          )
            <$> leftmost [takeLeft evCells, evExRes, evOutput, evCode]
      dynResults <-
        holdUniqDyn
          =<< holdDyn (ResultsSnapshot mempty mempty mempty mempty mempty mempty mempty) evResults
  el "script" . text $
    unlines
      [ "cms = new Map()",
        "cs = new Map()"
      ]
  where
    refreshState :: LedgerState -> a -> Performable m a
    refreshState ls u = do
      withMVar (ls ^. uuids) $ \xs -> do
        cs <- for xs $ \x ->
          liftJSM $ do
            cms <- global ^. js ("cms" :: Text)
            cm <- cms ^. js (show x :: Text)
            c <- fromJSVal =<< cm ^. js0 ("getValue" :: Text)
            pure $ fromMaybe "" c
        writeIORef (ls ^. code) $ M.fromList (zip xs cs)
        for_ xs $ updateParameters ls
      pure u
    --
    kernelUpdate ::
      LedgerState ->
      (() -> IO ()) ->
      (() -> IO ()) ->
      KernelUpdate ->
      Performable m ()
    kernelUpdate ls triggerNewKernel triggerNewRequest u =
      case u of
        NewKernel k -> do
          putStrLn "new kernel"
          writeIORef (ls ^. kernelUUID) k
          putMVar (ls ^. ready) ()
          liftIO $ triggerNewRequest ()
          writeChan (ls ^. queue) $ Just (U.nil, "__ledger = dict()")
        ShutdownKernel -> pass
        DeadKernel -> do
          putStrLn' "kernel died"
          writeIORef (ls ^. kernelUUID) Nothing
        StartKernel -> do
          putStrLn' "start new kernel"
          liftIO $ triggerNewKernel ()
    --
    resultsUpdate :: LedgerState -> (() -> IO ()) -> ResultsUpdate -> Performable m ()
    resultsUpdate ls triggerNewKernel u =
      case u of
        ExecuteCell c -> executeCell ls c
        Output c (KernelResult _ r) ->
          modifyIORef (ls ^. result) (M.insert c r)
        Output c (KernelStdout _ r) ->
          modifyIORef (ls ^. stdout) (M.alter (Just . (<> r) . fromMaybe "") c)
        Output c k@(KernelError _ rs _ _) -> do
          let rs' = filterEsc <$> rs
          print' k
          modifyIORef (ls ^. error) (M.alter (Just . (<> unlines rs') . fromMaybe "") c)
        Output _ (KernelMissing _) -> do
          putStrLn' "KernelMissing"
          writeIORef (ls ^. kernelUUID) Nothing
          liftIO $ triggerNewKernel ()
        Output _ (KernelDone _) -> do
          putStrLn' "KernelDone"
          putMVar (ls ^. ready) ()
        UpdateLabel c l -> do
          markDepsDirty ls c
          lbl <- readIORef (ls ^. label)
          let bs =
                S.difference
                  (S.fromList . takeRight $ M.elems lbl)
                  (S.fromList . takeRight $ toList (lbl !? c))
          case l of
            "" -> modifyIORef (ls ^. label) (M.delete c)
            _
              | S.member l bs ->
                modifyIORef (ls ^. label) (M.insert c $ Left l)
              | otherwise ->
                modifyIORef (ls ^. label) (M.insert c $ Right l)
          updateParameters ls c
        UpdateCode c -> markDirty ls c
        r -> print' r
      where
        filterEsc :: Text -> Text
        filterEsc = removeAll "\ESC\\[[0-9;]*m"
    ledgerUpdate :: LedgerState -> LedgerUpdate -> Performable m ()
    ledgerUpdate ls u =
      case u of
        AddCellEnd x -> modifyMVar (ls ^. uuids) (<> [x])
        RemoveCell x -> modifyMVar (ls ^. uuids) $ filter (/= x)
        RaiseCell x -> modifyMVar (ls ^. uuids) $ \xs ->
          case break (== x) xs of
            (f, _ : b) -> insertPenultimate x f ++ b
            _ -> xs
        LowerCell x -> modifyMVar (ls ^. uuids) $ \xs ->
          case break (== x) xs of
            (f, _ : y : b) -> f ++ y : x : b
            _ -> xs
        r -> print' r
      where
        insertPenultimate z [] = [z]
        insertPenultimate z [w] = [z, w]
        insertPenultimate z (w : ws) = w : insertPenultimate z ws

navbar :: forall t m. (MonadWidget t m) => Dynamic t (Maybe UUID) -> m (Event t KernelUpdate)
navbar dynKernel =
  elAttr
    "nav"
    ( "class" =: "navbar has-background-light"
        <> "role" =: "navigation"
        <> ("aria-label" =: "main navigation")
    )
    . divClass "navbar-brand"
    $ do
      elAttr "span" ("class" =: "navbar-item") $ do
        elAttr "img" ("src" =: "notebook-icon.png" <> "width" =: "28" <> "height" =: "28") blank
        divClass "title" $ text "Ledger"
      elAttr "span" ("class" =: "navbar-item") $ do
        let c =
              unwords . (["button", "is-small"] <>) . maybe ["is-loading"] (const [])
                <$> dynKernel
            a = ("type" =: "button" <>) . ("class" =:) <$> c
        (elNewKernel, _) <-
          elDynAttr' "button" a
            . elAttr "span" ("class" =: "icon")
            $ elAttr "i" ("class" =: "fas fa-check") blank
        pure $ domEvent Click elNewKernel $> StartKernel

cell ::
  forall t m.
  (MonadWidget t m) =>
  Dynamic t CellSnapshot ->
  CodeSnapshot ->
  m (Event t (Either ResultsUpdate LedgerUpdate))
cell dynSnapshot c =
  divClass "card" $ do
    (evRes, evLeg) <- divClass "card-header" $ do
      evLhs <- elAttr "p" ("class" =: "card-header-title") $ do
        (elEx, _) <-
          elAttr'
            "button"
            ("class" =: "button is-primary is-outlined is-small" <> "type" =: "button")
            . elAttr "span" ("class" =: "icon is-small")
            $ elAttr "i" ("class" =: "fas fa-play") blank
        elLabel <-
          let attr True = "type" =: "text" <> "class" =: "input"
              attr False = "type" =: "text" <> "class" =: "input is-danger"
              attr' = (Just <$>) . attr
           in inputElement
                (def :: InputElementConfig EventResult t GhcjsDomSpace)
                  { _inputElementConfig_initialValue =
                      maybe "" (either id id) (c ^. label),
                    _inputElementConfig_elementConfig =
                      (def :: ElementConfig EventResult t GhcjsDomSpace)
                        { _elementConfig_initialAttributes =
                            attr . maybe True isRight $ c ^. label,
                          _elementConfig_modifyAttributes =
                            Just
                              ( attr' . maybe True isRight . (^. label)
                                  <$> updated dynSnapshot
                              )
                        }
                  }
        dynText
          ( (\ls -> "= λ(" <> ls <> ")")
              . T.intercalate ", "
              . S.toList
              . fromMaybe mempty
              . (^. parameters)
              <$> dynSnapshot
          )
        let evEx = domEvent @t Click elEx $> ExecuteCell (c ^. uuid)
            evLabel = UpdateLabel (c ^. uuid) <$> _inputElement_input elLabel
        pure (leftmost [evEx, evLabel])
      evRhs <- elAttr "p" ("class" =: "card-header-icon")
        . divClass "buttons are-small has-addons"
        $ do
          (elU, _) <-
            elAttr'
              "button"
              ("class" =: "button" <> "type" =: "button")
              . elAttr "span" ("class" =: "icon is-small")
              $ elAttr "i" ("class" =: "fas fa-arrow-up") blank
          (elD, _) <-
            elAttr'
              "button"
              ("class" =: "button" <> "type" =: "button")
              . elAttr "span" ("class" =: "icon is-small")
              $ elAttr "i" ("class" =: "fas fa-arrow-down") blank
          (elX, _) <-
            elAttr'
              "button"
              ("class" =: "button is-danger is-outlined" <> "type" =: "button")
              . elAttr "span" ("class" =: "icon is-small")
              $ elAttr "i" ("class" =: "fas fa-times") blank
          let evX = domEvent @t Click elX $> RemoveCell (c ^. uuid)
              evU = domEvent @t Click elU $> RaiseCell (c ^. uuid)
              evD = domEvent @t Click elD $> LowerCell (c ^. uuid)
          pure $ leftmost [evX, evU, evD]
      pure (evLhs, evRhs)
    divClass "card-content" $ do
      elAttr "textarea" ("id" =: show (c ^. uuid)) $
        text (fromMaybe "" (c ^. code))
      dyn_ $
        dynSnapshot <&> \snapshot ->
          divClass (if snapshot ^. dirty then "dirty" else "clean") $ do
            case snapshot ^. stdout of
              Nothing -> blank
              Just t -> elClass "pre" "stdout" $ text t
            case snapshot ^. error of
              Nothing -> blank
              Just t -> elClass "pre" "error" $ text t
            case snapshot ^. result of
              Nothing -> blank
              Just t -> elClass "pre" "result" $ text t
    el "script" $ text (mkScript $ c ^. uuid)
    pure (leftmost [Left <$> evRes, Right <$> evLeg])
  where
    mkScript :: UUID -> Text
    mkScript u =
      unlines
        [ "var ta = document.getElementById('" <> show u <> "')",
          "var cm = CodeMirror.fromTextArea(ta,",
          "   {mode: 'python', viewportMargin: Infinity})",
          "cms['" <> show u <> "']=cm",
          "cm.on('changes', function(x) { onCodeChange('" <> show u <> "') })"
        ]

snapshotCells :: (MonadIO m) => LedgerState -> () -> m [CodeSnapshot]
snapshotCells ls () = do
  l <- readIORef (ls ^. label)
  c <- readIORef (ls ^. code)
  fmap (\u -> CodeSnapshot u (l !? u) (c !? u)) <$> readMVar (ls ^. uuids)

snapshotResults :: (MonadIO m) => LedgerState -> () -> m ResultsSnapshot
snapshotResults ls () =
  ResultsSnapshot
    <$> readIORef (ls ^. label)
    <*> readIORef (ls ^. parameters)
    <*> readIORef (ls ^. code)
    <*> readIORef (ls ^. result)
    <*> readIORef (ls ^. stdout)
    <*> readIORef (ls ^. error)
    <*> readIORef (ls ^. dirty)

extractCell :: CodeSnapshot -> ResultsSnapshot -> CellSnapshot
extractCell c r =
  let u = c ^. uuid
   in CellSnapshot
        (r ^. label . at u)
        (r ^. parameters . at u)
        (r ^. result . at u)
        (r ^. stdout . at u)
        (r ^. error . at u)
        (S.member u $ r ^. dirty)

executeCell :: (MonadIO m) => LedgerState -> UUID -> m ()
executeCell ls x =
  withMVar (ls ^. stop) $ \s ->
    unless s $
      (!? x) <$> readIORef (ls ^. code) >>= \case
        Just c ->
          (!? x) <$> readIORef (ls ^. parameters) >>= \case
            Just ps -> do
              lbl <- readIORef (ls ^. label)
              let lbl' = M.fromList $ swap <$> takeRight (flop <$> M.toList lbl)
                  n = T.replace "-" "_" (show x)
                  n' = "_" <> n
                  cs' = addReturn $ lines c
                  c' =
                    unlines
                      ( "def " <> n' <> "(" <> T.intercalate ", " (S.toList ps) <> "):"
                          : (("  " <>) <$> cs')
                      )
                  args =
                    T.intercalate ", " $
                      fmap (\p -> p <> "=__ledger['" <> U.toText (lbl' M.! p) <> "']") (S.toList ps)
                  e = case lbl !? x of
                    Just _ -> T.concat ["__ledger['", show x, "'] = ", n', "(", args, ")"]
                    Nothing -> T.concat [n', "(", args, ")"]
              putStrLn' ("Execute: " <> show x)
              print' c'
              print' e
              modifyIORef (ls ^. result) $ M.delete x
              modifyIORef (ls ^. stdout) $ M.delete x
              modifyIORef (ls ^. error) $ M.delete x
              modifyIORef (ls ^. dirty) $ S.delete x
              writeChan (ls ^. queue) $ Just (x, unlines [c', e])
            Nothing -> R.error "impossible"
        Nothing -> R.error "impossible"
  where
    flop :: (a, Either b c) -> Either (a, b) (a, c)
    flop (a, Left b) = Left (a, b)
    flop (a, Right c) = Right (a, c)

addReturn :: [Text] -> [Text]
addReturn cs =
  let rs = dropWhile isSpaces $ reverse cs
   in case nonEmpty rs of
        Just (l :| ls) ->
          let l' =
                if isSpace (T.head l) || T.isPrefixOf "return" l
                  then l
                  else "return " <> l
           in reverse (l' : ls)
        Nothing -> []
  where
    isSpaces :: Text -> Bool
    isSpaces = all isSpace . toString

consoleLog :: (Show a) => a -> JSM JSVal
consoleLog x = eval ("console.log('" <> show x <> "')" :: Text)

tokenize :: Text -> Set Text
tokenize t =
  case A.parseOnly tokens t of
    Left e -> R.error $ toText e
    Right ts -> S.fromList ts
  where
    isTokenChar c = c == '_' || isAlphaNum c
    initChar = A.satisfy (\c -> c == '_' || isAlpha c)
    token = A.lookAhead initChar *> A.takeWhile1 isTokenChar
    tokens = A.skipMany sc *> token `A.sepBy` A.skipMany sc
    sc = A.satisfy (\c -> not (c == '_' || isAlpha c))

unsnoc :: NonEmpty a -> ([a], a)
unsnoc (x :| []) = ([], x)
unsnoc (x :| (y : ys)) = first (x :) $ unsnoc (y :| ys)

removeAll :: Text -> Text -> Text
removeAll r t = mconcat $ go t
  where
    go :: Text -> [Text]
    go "" = []
    go u =
      let (a, _ :: Text, b) = u =~ r
       in a : go b

updateParameters :: (MonadIO m) => LedgerState -> UUID -> m ()
updateParameters ls x =
  (!? x) <$> readIORef (ls ^. code) >>= \case
    Just c -> do
      lbl <- readIORef (ls ^. label)
      let l = lbl !? x
          bs =
            S.difference
              (S.fromList . takeRight $ M.elems lbl)
              (S.fromList . takeRight $ toList l)
          ts = tokenize c
          ps = S.intersection bs ts
      modifyIORef (ls ^. parameters) (M.insert x ps)
    Nothing -> modifyIORef (ls ^. parameters) (M.delete x)

markDirty :: (MonadIO m) => LedgerState -> UUID -> m ()
markDirty ls u = do
  d <- S.member u <$> readIORef (ls ^. dirty)
  unless d $ do
    modifyIORef (ls ^. dirty) (S.insert u)
    markDepsDirty ls u

markDepsDirty :: forall m. (MonadIO m) => LedgerState -> UUID -> m ()
markDepsDirty ls x = do
  pss <- M.toList <$> readIORef (ls ^. parameters)
  (!? x) <$> readIORef (ls ^. label) >>= \case
    Just (Right l) -> do
      let ys = fst <$> filter (S.member l . snd) pss
      for_ ys $ \y -> do
        d <- S.member y <$> readIORef (ls ^. dirty)
        unless d $ markDirty ls y
    _ -> pass
