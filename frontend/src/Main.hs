{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Lens hiding ((#))
import qualified Data.Map as M
import Data.Traversable (for)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Types (JSM, liftJSM)
import Ledger.JS
import Ledger.Types
import Reflex.Dom.Core
-- import qualified Reflex.Dom.Xhr as X
import Relude
import System.Random

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

main :: IO ()
main =
  runLedger 8001 $
    mainWidgetWithHead htmlHead htmlBody

htmlHead :: MonadWidget t m => m ()
htmlHead = do
  elAttr
    "meta"
    ( "name" =: "viewport"
        <> "content" =: "width=device-width, initial-scale=1, maximum-scale=1.0, user-scalable=no"
    )
    blank
  el "title" $ text "Ledger"
  elAttr "script" ("defer" =: "" <> "src" =: "fontawesome/js/all.js") blank
  elAttr
    "script"
    ("src" =: "codemirror/lib/codemirror.js" <> "async" =: "false")
    blank
  elAttr
    "link"
    ( "rel" =: "stylesheet"
        <> "type" =: "text/css"
        <> "href" =: "codemirror/lib/codemirror.css"
    )
    blank
  elAttr
    "script"
    ("src" =: "codemirror/mode/python/python.js" <> "async" =: "false" <> "defer" =: "")
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
  ls <- LedgerState <$> newMVar [] <*> newIORef mempty <*> newChan <*> newMVar False
  elAttr
    "nav"
    ( "class" =: "navbar has-background-light"
        <> "role" =: "navigation"
        <> ("aria-label" =: "main navigation")
    )
    . divClass "navbar-brand"
    . elAttr "span" ("class" =: "navbar-item")
    $ do
      elAttr "img" ("src" =: "notebook-icon.png" <> "width" =: "28" <> "height" =: "28") blank
      divClass "title" $ text "Ledger"
  divClass "section"
    . divClass "container"
    $ do
      rec dynEvXs <- simpleList ((^. uuids) <$> dynSnapshot) $ \dynU -> do
            evEvX <- dyn $ liftA2 mkCell dynU dynSnapshot
            switchHold never evEvX
          evXs <- switchHold never $ leftmost <$> updated dynEvXs
          (elAdd, _) <-
            elAttr' "button" ("class" =: "button" <> "type" =: "button")
              . elAttr "span" ("class" =: "icon")
              $ elAttr "i" ("class" =: "fas fa-plus") blank
          let evAdd = domEvent Click elAdd
              evUUID = AddCellEnd <$> nextUUID evAdd
          -- evUpdate <- performUpdate $ leftmost [evUUID, evXs]
          evSnapshot <-
            leftmost [evUUID, evXs]
              & (refreshState ls >=> updateState ls >=> snapshotState ls)
          -- dynUs <- foldDynM (updateLedger ls) [] evUpdate
          dynSnapshot <- holdUniqDyn =<< holdDyn (LedgerSnapshot [] mempty) evSnapshot
      el "script" . text $
        unlines
          [ "cms = new Map()",
            "cs = new Map()"
          ]
  where
    mkScript i =
      unlines
        [ "var ta = document.getElementById('" <> i <> "')",
          "var cm = CodeMirror.fromTextArea(ta,",
          "   {mode: 'python', viewportMargin: Infinity})",
          "cms['" <> i <> "']=cm"
        ]
    --
    nextUUID = push (const $ Just <$> liftIO randomIO)
    --
    refreshState :: LedgerState -> Event t LedgerUpdate -> m (Event t LedgerUpdate)
    refreshState ls ev =
      performEvent $
        ev <&> \u -> do
          withMVar (ls ^. uuids) $ \xs -> do
            cs <- for xs $ \x ->
              liftJSM $ do
                cms <- global ^. js ("cms" :: Text)
                cm <- cms ^. js (show x :: Text)
                c <- fromJSVal =<< cm ^. js0 ("getValue" :: Text)
                pure $ fromMaybe "" c
            writeIORef (ls ^. code) $ M.fromList (zip xs cs)
          pure u
    --
    updateState :: LedgerState -> Event t LedgerUpdate -> m (Event t ())
    updateState ls ev = performEvent $
      ev <&> \case
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
        ExecuteCell x ->
          withMVar (ls ^. stop) $ \s ->
            unless s $
              M.lookup x <$> readIORef (ls ^. code) >>= \case
                Just c -> do
                  putStrLn ("Execute: " <> show (x, c))
                  writeChan (_queue ls) $ Just (x, c)
                Nothing -> pass
        r -> print r
      where
        insertPenultimate z [] = [z]
        insertPenultimate z [w] = [z, w]
        insertPenultimate z (w : ws) = w : insertPenultimate z ws
    --
    snapshotState :: LedgerState -> Event t () -> m (Event t LedgerSnapshot)
    snapshotState ls ev = performEvent $
      ev <&> \() ->
        LedgerSnapshot <$> readMVar (ls ^. uuids) <*> readIORef (ls ^. code)
    --
    --
    mkCell u snapshot =
      divClass "card" $ do
        evX <- divClass "card-header" $ do
          evLhs <- elAttr "p" ("class" =: "card-header-title") $ do
            (elEx, _) <-
              elAttr'
                "button"
                ("class" =: "button is-primary is-outlined is-small" <> "type" =: "button")
                . elAttr "span" ("class" =: "icon is-small")
                $ elAttr "i" ("class" =: "fas fa-play") blank
            text "λ()"
            let evEx = domEvent @t Click elEx $> ExecuteCell u
            pure evEx
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
              let evX = domEvent @t Click elX $> RemoveCell u
                  evU = domEvent @t Click elU $> RaiseCell u
                  evD = domEvent @t Click elD $> LowerCell u
              pure $ leftmost [evX, evU, evD]
          pure $ leftmost [evLhs, evRhs]
        divClass "card-content"
          $ elAttr "textarea" ("id" =: show u)
          $ text (fromMaybe "" $ snapshot ^. code . at u)
        el "script" $ text (mkScript $ show u)
        pure evX

consoleLog :: (Show a) => a -> JSM JSVal
consoleLog x = eval ("console.log('" <> show x <> "')" :: Text)
