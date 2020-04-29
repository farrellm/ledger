{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ledger.Utility where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Monad.Fix (MonadFix)
import Data.Aeson (ToJSON, encode)
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text as A
import Data.Char (isAlpha, isAlphaNum, isSpace)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.UUID.Types (UUID)
import Data.Witherable (Filterable, catMaybes, mapMaybe)
import Ledger.Prelude
import Reflex.Dom.Core
import System.Random (randomIO)

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

tokenize :: Text -> Set Text
tokenize t =
  case A.parseOnly tokens t of
    Left e -> bug $ LedgerError e
    Right ts -> S.fromList ts
  where
    isTokenChar c = c == '_' || isAlphaNum c
    initChar = A.satisfy (\c -> c == '_' || isAlpha c)
    token = A.lookAhead initChar *> A.takeWhile1 isTokenChar
    tokens = A.skipMany sc *> token `A.sepBy` A.skipMany sc
    sc = A.satisfy (\c -> not (c == '_' || isAlpha c))

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
