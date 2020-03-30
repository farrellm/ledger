{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.UUID as U
import Data.UUID.V1 (nextUUID)
-- import Lucid
import Jupyter
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static
import Web.Scotty
import Prelude hiding (get)

main :: IO ()
main = test

main2 :: IO ()
main2 = do
  putStrLn "Hello, Haskell!"
  scotty 8000 $ do
    middleware
      ( simpleCors
          . staticPolicy (addBase "static")
          . staticPolicy (addBase "frontend")
      )
    get "/" $ file "./frontend/index.html"
    get "/uuid" $
      text . fromStrict . U.toText
        =<< nextJust
          ( liftIO $ do
              u <- nextUUID
              print u
              pure u
          )
