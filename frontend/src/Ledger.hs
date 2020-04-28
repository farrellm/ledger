{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger where

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
import qualified Data.Text.Lazy as TL
import Data.Traversable (for)
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as U
import Data.Witherable (Filterable, catMaybes, mapMaybe)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Types (liftJSM)
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
postJson' u a =
  XhrRequest "POST" u $
    def
      { _xhrRequestConfig_headers = headerUrlEnc,
        _xhrRequestConfig_sendData = body
      }
  where
    headerUrlEnc = "Content-type" =: "text/plain"
    body = toStrict $ encode a

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
  evPostBuild <- getPostBuild
  ls <- do
    url' <-
      liftJSM $
        valToText =<< jsg ("window" :: Text) ! ("location" :: Text) ! ("href" :: Text)
    let _ledgerState_url = url' =~ ("https?://[^:]+" :: Text) :: Text
    print' _ledgerState_url
    _ledgerState_file <- newIORef "new.ldgr"
    _ledgerState_kernelUUID <- newEmptyMVar
    _ledgerState_uuids <- newMVar []
    _ledgerState_label <- newIORef mempty
    _ledgerState_badLabel <- newIORef mempty
    _ledgerState_parameters <- newIORef mempty
    _ledgerState_code <- newIORef mempty
    _ledgerState_result <- newIORef mempty
    _ledgerState_stdout <- newIORef mempty
    _ledgerState_error <- newIORef mempty
    _ledgerState_dirty <- newIORef mempty
    _ledgerState_queue <- newChan
    _ledgerState_stop <- newMVar False
    _ledgerState_ready <- newEmptyMVar
    pure LedgerState {..}
  -- manage new kernel requests
  (evNewKernel', triggerNewKernel) <- newTriggerEvent
  evNewKernel'' <- getAndDecode (evNewKernel' $> ((ls ^. url) <> ":8000/new_kernel"))
  evNewKernel <- NewKernel <<$>> catMaybes <$> filterDuplicates evNewKernel''
  (evDeadKernel', triggerDeadKernel) <- newTriggerEvent
  let evDeadKernel = evDeadKernel' $> DeadKernel
  -- manage execute requests
  evExRes <- pollExecute ls
  -- manage kernel output requests
  evOutput <- pollOutput ls triggerDeadKernel
  -- manage code changes
  (evCodeChange, triggerCodeChange) <- newTriggerEvent
  liftJSM
    . (global <# ("onCodeChange" :: Text))
    . fun
    $ \_ _ [u] -> valToText u >>= liftIO . triggerCodeChange
  evCodeChange' <- debounce 0.2 evCodeChange
  let evCode = UpdateCode <$> catMaybes (U.fromText <$> evCodeChange')
  --
  rec (evStartKernel, evSaveLoad) <- navbar dynKernel
      ((evCellsRes, evCellsLedger), evAddEnd) <-
        divClass "section" . divClass "container" $ do
          dynEvCells <- simpleList dynSnapshotCells $ \dynCode -> do
            dynCell <- holdUniqDyn (zipDynWith extractCell dynCode dynResults)
            cell dynCell dynCode
          let evCells = switchDyn $ leftmost <$> dynEvCells
          (elAdd, _) <-
            elAttr' "button" ("class" =: "button" <> "type" =: "button")
              . elAttr "span" ("class" =: "icon")
              $ elAttr "i" ("class" =: "fas fa-plus") blank
          let evAdd = domEvent Click elAdd
              evUUID = AddCellEnd <$> nextUUID evAdd
          pure (fanEither evCells, evUUID)
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
              >=> traverse_ (kernelUpdate ls triggerNewKernel)
              >=> \_ -> tryReadMVar (ls ^. kernelUUID)
          )
            <$> mergeList
              [ evPostBuild $> StartKernel,
                evNewKernel,
                evDeadKernel,
                evStartKernel
              ]
      dynKernel <- holdDyn Nothing evKernel
      --
      evSnapshotCells <-
        performEvent $
          ( refreshState ls
              >=> traverse_ (ledgerUpdate ls)
              >=> snapshotCells ls
          )
            <$> mergeList [evAddEnd, evCellsLedger, evSaveLoad]
      dynSnapshotCells <- holdUniqDyn =<< holdDyn [] evSnapshotCells
      --
      evResults <-
        performEvent $
          ( refreshState ls
              >=> traverse_ (resultsUpdate ls triggerDeadKernel)
              >=> snapshotResults ls
          )
            <$> mergeList [evCellsRes, evExRes, evOutput, evCode]
      dynResults <-
        holdUniqDyn
          =<< holdDyn emptyResultsSnapshot evResults
  el "script" . text $
    unlines
      [ "cms = new Map()",
        "cs = new Map()"
      ]
  where
    pollExecute :: LedgerState -> m (Event t ResultsUpdate)
    pollExecute ls = do
      (evExecute, triggerExecute) <- newTriggerEvent
      fork . forever $ do
        putStrLn' "#### waiting for ready"
        takeMVar (ls ^. ready)
        putStrLn' "#### ready!"
        q <- readChan (ls ^. queue)
        print' q
        case q of
          Nothing -> void $ swapMVar (ls ^. stop) False
          Just (u, c) -> triggerExecute (u, c)
      evExecute' <- performEventAsync $
        evExecute <&> \(u, c) go ->
          fork $ do
            k <- readMVar (ls ^. kernelUUID)
            go $ ExecuteRequest k u c
      evX <-
        performRequestAsync $
          evExecute' <&> postJson' ((ls ^. url) <> ":8000/execute")
      filterDuplicates (evX $> RunningCell)
    --
    pollOutput ls triggerDeadKernel = do
      evPostBuild <- getPostBuild
      (evNewRequest, triggerNewRequest) <- newTriggerEvent
      evNewRequest' <- performEventAsync $
        (evNewRequest <> evPostBuild) <&> \() go ->
          fork $ do
            putStrLn' "read kernel UUID"
            k <- readMVar (ls ^. kernelUUID)
            print' k
            go $ ResultRequest k
      evOutput' <-
        performRequestAsync $
          evNewRequest' <&> postJson' ((ls ^. url) <> ":8000/result")
      evOutput'' <- filterDuplicates $ fmap decodeXhrResponse evOutput'
      evOutput''' <- performEvent $
        evOutput'' <&> \case
          Nothing -> do
            liftIO $ do
              putStrLn' "request fail 2"
              void $ triggerDeadKernel ()
              triggerNewRequest ()
            pure Nothing
          Just (c :: UUID, r :: KernelOutput) -> do
            liftIO $ triggerNewRequest ()
            pure . Just $ Output c r
      pure $ catMaybes evOutput'''
    --
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
      KernelUpdate ->
      Performable m ()
    kernelUpdate ls triggerNewKernel u =
      case u of
        NewKernel k -> do
          putStrLn "new kernel"
          putMVar (ls ^. kernelUUID) k
          putMVar (ls ^. ready) ()
          writeChan (ls ^. queue) $ Just (U.nil, "__ledger = dict()")
        ShutdownKernel -> pass
        DeadKernel -> do
          putStrLn' "kernel died"
          void $ tryTakeMVar (ls ^. kernelUUID)
        StartKernel -> do
          putStrLn' "start new kernel"
          liftIO $ triggerNewKernel ()
    --
    resultsUpdate :: LedgerState -> (() -> IO ()) -> ResultsUpdate -> Performable m ()
    resultsUpdate ls triggerDeadKernel u =
      case u of
        ExecuteCell c -> executeCell ls c
        Output c (KernelResult _ r) ->
          modifyIORef (ls ^. result) (M.insert c r)
        Output c (KernelStdout _ r) ->
          modifyIORef (ls ^. stdout) (M.alter (Just . (<> r) . fromMaybe "") c)
        Output c k@(KernelError _ rs _ _) -> do
          let rs' = filterEsc <$> rs
          print' k
          print' rs'
          modifyIORef (ls ^. error) (M.alter (Just . (<> unlines rs') . fromMaybe "") c)
        Output _ (KernelMissing _) -> do
          putStrLn' "KernelMissing"
          liftIO $ triggerDeadKernel ()
        Output _ (KernelDone _) -> do
          putStrLn' "KernelDone"
          putMVar (ls ^. ready) ()
        UpdateLabel c l -> do
          markDepsDirty ls c
          lbl <- readIORef (ls ^. label)
          let bs = S.fromList . fmap snd . filter ((/= c) . fst) $ M.toList lbl
          modifyIORef (ls ^. badLabel) (S.delete c)
          case l of
            "" -> modifyIORef (ls ^. label) (M.delete c)
            _ -> do
              modifyIORef (ls ^. label) (M.insert c l)
              when (S.member l bs) $ modifyIORef (ls ^. badLabel) (S.insert c)
          updateParameters ls c
        UpdateCode c -> markDirty ls c
        r -> print' r
      where
        filterEsc :: Text -> Text
        filterEsc = toStrict . go . toLazy
          where
            go :: LText -> LText
            go "" = ""
            go t =
              let (a, t') = TL.break (== '\ESC') t
                  b = TL.drop 1 $ TL.dropWhile (/= 'm') t'
               in a <> go b
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

navbar ::
  forall t m.
  (MonadWidget t m) =>
  Dynamic t (Maybe UUID) ->
  m (Event t KernelUpdate, Event t LedgerUpdate)
navbar dynKernel =
  elAttr
    "nav"
    ( "class" =: "navbar has-background-light"
        <> "role" =: "navigation"
        <> ("aria-label" =: "main navigation")
    )
    . divClass "navbar-brand"
    $ do
      evSaveLoad <-
        divClass "navbar-item" $ do
          let da True = "class" =: "dropdown is-active"
              da False = "class" =: "dropdown"
          rec (dynActive, elSaveLoad) <-
                elDynAttr "div" (da <$> dynActive) $ do
                  (elActive, _) <-
                    divClass "dropdown-trigger"
                      . elAttr' "button" ("class" =: "button")
                      $ do
                        elAttr "span" ("class" =: "icon") $
                          elAttr "img" ("src" =: "notebook-icon.png") blank
                        elAttr "span" ("class" =: "icon is-small") $
                          elAttr "i" ("class" =: "fas fa-angle-down") blank
                  evSaveLoad' <-
                    elAttr "div" ("class" =: "dropdown-menu" <> "role" =: "menu")
                      . divClass "dropdown-content"
                      $ do
                        (elSave, _) <- elAttr' "a" ("class" =: "navbar-item") $ text "Save"
                        (elLoad, _) <- elAttr' "a" ("class" =: "navbar-item") $ text "Load"
                        let evSave = domEvent Click elSave $> SaveLedger
                            evLoad =
                              domEvent Click elLoad
                                $> LoadLedger (R.error "implement load dialog")
                        pure $ leftmost @t [evSave, evLoad]
                  dynActive' <- toggle False $ domEvent Click elActive
                  pure (dynActive', evSaveLoad')
          pure elSaveLoad
      elAttr "span" ("class" =: "navbar-item")
        . divClass "title"
        $ text "Ledger"
      elAttr "span" ("class" =: "navbar-item") $ do
        let c =
              unwords . (["button", "is-small"] <>) . maybe ["is-loading"] (const [])
                <$> dynKernel
            a = ("type" =: "button" <>) . ("class" =:) <$> c
        (elNewKernel, _) <-
          elDynAttr' "button" a
            . elAttr "span" ("class" =: "icon")
            $ elAttr "i" ("class" =: "fas fa-check") blank
        pure
          ( domEvent Click elNewKernel $> StartKernel,
            evSaveLoad
          )

cell ::
  forall t m.
  (MonadWidget t m) =>
  Dynamic t CellSnapshot ->
  Dynamic t CodeSnapshot ->
  m (Event t (Either ResultsUpdate LedgerUpdate))
cell dynCell dynCode =
  switchHold never =<< dyn (cellBody <$> dynCode)
  where
    cellBody c =
      divClass "card" $
        do
          (evRes, evLeg) <- divClass "card-header" $ do
            evLhs <- elAttr "p" ("class" =: "card-header-title") $ do
              (elEx, _) <-
                elAttr'
                  "button"
                  ( "class" =: "button is-primary is-outlined is-small"
                      <> "type" =: "button"
                  )
                  . elAttr "span" ("class" =: "icon is-small")
                  $ elAttr "i" ("class" =: "fas fa-play") blank
              evLabel <- labelInput c
              dynText
                ( (\ls -> "= λ(" <> ls <> ")")
                    . T.intercalate ", "
                    . S.toList
                    . maybeToMonoid
                    . (^. parameters)
                    <$> dynCell
                )
              let evEx = domEvent @t Click elEx $> ExecuteCell (c ^. uuid)
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
                let evX = domEvent Click elX $> RemoveCell (c ^. uuid)
                    evU = domEvent Click elU $> RaiseCell (c ^. uuid)
                    evD = domEvent Click elD $> LowerCell (c ^. uuid)
                pure $ leftmost [evX, evU, evD]
            pure (evLhs, evRhs)
          divClass "card-content" $ do
            elAttr "textarea" ("id" =: show (c ^. uuid)) $
              text (c ^. code)
            elDynAttr "div" (cleanDirty <$> dynCell) $ do
              dynStdout <- holdUniqDyn ((^. stdout) <$> dynCell)
              dynError <- holdUniqDyn ((^. error) <$> dynCell)
              dynResult <- holdUniqDyn ((^. result) <$> dynCell)
              dyn_ $ dynStdout <&> maybe blank (elClass "pre" "stdout" . text)
              dyn_ $ dynError <&> maybe blank (elClass "pre" "error" . text)
              dyn_ $ dynResult <&> maybe blank (elClass "pre" "result" . text)
          el "script" $ text (mkScript $ c ^. uuid)
          pure (leftmost [Left <$> evRes, Right <$> evLeg])
    --
    labelInput :: CodeSnapshot -> m (Event t ResultsUpdate)
    labelInput c = do
      elLabel <-
        let attr False = "type" =: "text" <> "class" =: "input"
            attr True = "type" =: "text" <> "class" =: "input is-danger"
            attr' = (Just <$>) . attr
         in inputElement
              (def :: InputElementConfig EventResult t GhcjsDomSpace)
                { _inputElementConfig_initialValue = c ^. label,
                  _inputElementConfig_elementConfig =
                    (def :: ElementConfig EventResult t GhcjsDomSpace)
                      { _elementConfig_initialAttributes =
                          attr $ c ^. badLabel,
                        _elementConfig_modifyAttributes =
                          Just (attr' . (^. badLabel) <$> updated dynCell)
                      }
                }
      pure $ UpdateLabel (c ^. uuid) <$> _inputElement_input elLabel
    --
    mkScript :: UUID -> Text
    mkScript u =
      unlines
        [ "var ta = document.getElementById('" <> show u <> "')",
          "var cm = CodeMirror.fromTextArea(ta,",
          "   {mode: 'python', viewportMargin: Infinity})",
          "cms['" <> show u <> "']=cm",
          "cm.on('changes', function(x) { onCodeChange('" <> show u <> "') })"
        ]
    --
    cleanDirty s =
      if s ^. dirty
        then "class" =: "dirty"
        else "class" =: "clean"

snapshotCells :: (MonadIO m) => LedgerState -> () -> m [CodeSnapshot]
snapshotCells ls () = do
  l <- readIORef (ls ^. label)
  b <- readIORef (ls ^. badLabel)
  c <- readIORef (ls ^. code)
  fmap (\u -> CodeSnapshot u (fromMaybe "" (l !? u)) (S.member u b) (fromMaybe "" (c !? u)))
    <$> readMVar (ls ^. uuids)

snapshotResults :: (MonadIO m) => LedgerState -> () -> m ResultsSnapshot
snapshotResults ls () = do
  _resultsSnapshot_label <- readIORef (ls ^. label)
  _resultsSnapshot_badLabel <- readIORef (ls ^. badLabel)
  _resultsSnapshot_parameters <- readIORef (ls ^. parameters)
  _resultsSnapshot_code <- readIORef (ls ^. code)
  _resultsSnapshot_result <- readIORef (ls ^. result)
  _resultsSnapshot_stdout <- readIORef (ls ^. stdout)
  _resultsSnapshot_error <- readIORef (ls ^. error)
  _resultsSnapshot_dirty <- readIORef (ls ^. dirty)
  pure ResultsSnapshot {..}

extractCell :: CodeSnapshot -> ResultsSnapshot -> CellSnapshot
extractCell c r =
  let u = c ^. uuid
   in CellSnapshot
        (r ^. label . at u)
        (S.member u (r ^. badLabel))
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
              bad <- readIORef (ls ^. badLabel)
              let lbl' =
                    M.fromList $
                      swap <$> filter (flip S.notMember bad . fst) (M.toList lbl)
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
              writeChan (ls ^. queue) $ Just (U.nil, "%reset in out")
            Nothing -> R.error "impossible"
        Nothing -> R.error "impossible"

addReturn :: [Text] -> [Text]
addReturn cs =
  let rs = dropWhile isSpaces $ reverse cs
   in case nonEmpty rs of
        Just (l :| ls) ->
          let l' =
                if isSpace (T.head l)
                  || T.isPrefixOf "return" l
                  || T.isInfixOf ";" l
                  || T.isPrefixOf "%" l
                  then l
                  else "return " <> l
           in reverse (l' : ls)
        Nothing -> []
  where
    isSpaces :: Text -> Bool
    isSpaces = all isSpace . toString

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

updateParameters :: (MonadIO m) => LedgerState -> UUID -> m ()
updateParameters ls x =
  (!? x) <$> readIORef (ls ^. code) >>= \case
    Just c -> do
      lbl <- readIORef (ls ^. label)
      bad <- readIORef (ls ^. badLabel)
      let bs =
            S.fromList
              . fmap snd
              . filter (flip S.notMember bad . fst)
              $ M.toList lbl
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
  bad <- readIORef (ls ^. badLabel)
  (!? x) <$> readIORef (ls ^. label) >>= \case
    Just l | S.notMember x bad -> do
      let ys = fst <$> filter (S.member l . snd) pss
      for_ ys $ \y -> do
        d <- S.member y <$> readIORef (ls ^. dirty)
        unless d $ markDirty ls y
    _ -> pass
