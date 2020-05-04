{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Widgets where

import Control.Lens hiding ((#))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.UUID.Types (UUID)
import Ledger.Prelude
import Ledger.Types
import Reflex.Dom.Core

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
                                $> LoadLedger (bug $ LedgerError "implement load dialog")
                        pure $ leftmost @t [evSave, evLoad]
                  let evActive = domEvent Click elActive
                  dynActive' <- toggle False $ leftmost [evActive, evSaveLoad' $> ()]
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
