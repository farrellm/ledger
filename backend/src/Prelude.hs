{-# LANGUAGE OverloadedStrings #-}

module Prelude
  ( module Relude,
    firstJust,
    nextJust,
  )
where

import Relude

firstJust :: (Monad m) => [m (Maybe a)] -> m a
firstJust [] = error "no Just found!"
firstJust (x : xs) = maybe (firstJust xs) pure =<< x

nextJust :: (Monad m) => m (Maybe a) -> m a
nextJust x = maybe (nextJust x) pure =<< x
