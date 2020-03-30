{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Lens hiding ((#))
import qualified Data.Text as T
import Data.Witherable (catMaybes)
import Language.Javascript.JSaddle
-- import Language.Javascript.JSaddle.Evaluate (eval)
-- import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.Types (JSM, liftJSM)
import Ledger.JS
import Reflex.Dom.Core
import qualified Reflex.Dom.Xhr as X
import Relude hiding (catMaybes)
import Relude.Unsafe (fromJust)

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
  -- elAttr "script" ( "defer" =: "" <> "src" =: "fontawesome/js/all.js") blank
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
    ("src" =: "codemirror/mode/javascript/javascript.js" <> "async" =: "false" <> "defer" =: "")
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
htmlBody =
  divClass "section"
    . divClass "container"
    $ do
      elClass "h1" "title" $ text "Ledger"
      rec void . simpleList dynUs $ \dynU ->
            dyn_ $
              dynU <&> \u ->
                divClass "card" $ do
                  divClass "card-content" $
                    elAttr "textarea" ("id" =: u) blank
                  el "script" $ text (mkScript u)
          (elAdd, _) <- elAttr' "button" ("type" =: "button") $ text "+"
          let evAdd = domEvent Click elAdd
          evU <- nextUUID evAdd
          dynUs <- foldDynM (\x xs -> pure (xs <> [x])) [] evU
      el "script" . text $
        unlines
          [ "window.cms = new Map()",
            "window.cs = new Map()"
          ]
  where
    mkScript i =
      unlines
        [ "var ta = document.getElementById('" <> i <> "')",
          "var c",
          "if ('" <> i <> "' in window.cms) {",
          "  c = window.cms['" <> i <> "'].getValue()",
          "} else {",
          "  c = ''",
          "}",
          "console.log(c)",
          "var cm = CodeMirror.fromTextArea(ta,",
          "   {mode: 'javascript', viewportMargin: Infinity})",
          "cm.setValue(c)",
          "window.cms['" <> i <> "']=cm"
        ]
    nextUUID e = do
      evR <-
        X.performRequestAsync
          (e $> X.XhrRequest "GET" "http://farrellm.duckdns.org:8000/uuid" def)
      let evU = X._xhrResponse_responseText <$> evR
      dynU <- holdUniqDyn =<< holdDyn Nothing evU
      pure . catMaybes $ updated dynU
