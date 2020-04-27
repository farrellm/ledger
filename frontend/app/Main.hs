module Main where

import Ledger
import Ledger.JS
import Reflex.Dom.Core

main :: IO ()
main =
  runLedger 8001 $
    mainWidgetWithHead htmlHead htmlBody
