{-# LANGUAGE OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Network.HTTP.Toolkit.Header (
  RequestResponse(..)
, readRequestResponse
, readRequestResponseWithLimit
, defaultHeaderSizeLimit
, combineHeaderLines
, readHeaderLines
, parseHeaderFields
) where

import           Control.Applicative
import           Control.Monad (when)
import           Control.Exception
import           Data.Foldable (Foldable)
import           Data.Traversable (Traversable)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Type
import           Network.HTTP.Toolkit.Connection

data RequestResponse a = RequestResponse a [Header]
  deriving (Eq, Show, Functor, Foldable, Traversable)

readRequestResponse :: Connection -> IO (RequestResponse ByteString)
readRequestResponse = readRequestResponseWithLimit defaultHeaderSizeLimit

readRequestResponseWithLimit :: Int -> Connection -> IO (RequestResponse ByteString)
readRequestResponseWithLimit limit c = do
  hs <- readHeaderLines limit c
  case hs of
    x : xs -> maybe (throwIO InvalidHeader) (return . RequestResponse x) (parseHeaderFields xs)
    [] -> throwIO InvalidHeader

-- http://tools.ietf.org/html/rfc2616#section-4.2
parseHeaderFields :: [ByteString] -> Maybe [Header]
parseHeaderFields = go . combineHeaderLines
  where
    go :: [ByteString] -> Maybe [Header]
    go hs = case hs of
      [] -> Just []
      x:xs -> case B.break (== ':') x of
        (_, "") -> Nothing
        (ys, zs) -> ((CI.mk ys, (stripStart . B.tail) zs) :) <$> go xs

combineHeaderLines :: [ByteString] -> [ByteString]
combineHeaderLines = go
  where
    go hs = case hs of
      x : xs -> case spanStartsWithWhitespace xs of
        (ys, zs) -> B.unwords (stripEnd x : ys) : go zs
      xs -> xs

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\r'

stripStart :: ByteString -> ByteString
stripStart = B.dropWhile isSpace

stripEnd :: ByteString -> ByteString
stripEnd = fst . B.spanEnd isSpace

spanStartsWithWhitespace :: [ByteString] -> ([ByteString], [ByteString])
spanStartsWithWhitespace = go
  where
    go hs = case hs of
      x : xs -> case B.span isSpace x of
        ("", _) -> ([], hs)
        (_, y) -> let (ys, zs) = go xs in (stripEnd y : ys, zs)
      [] -> ([], [])

readHeaderLines :: Int -> Connection -> IO [ByteString]
readHeaderLines n c = go n
  where
    go limit = do
      (newLimit, bs) <- readLine c limit
      if B.null bs then return [] else (bs :) <$> go newLimit

defaultHeaderSizeLimit :: Int
defaultHeaderSizeLimit = 64 * 1024

readLine :: Connection -> Int -> IO (Int, ByteString)
readLine c = fmap (\(n, xs) -> (n, stripCR xs)) . go
  where
    go limit = do
      bs <- connectionRead c
      let n = B.length bs
          newLimit = limit - n
      when (newLimit < 0) (throwIO HeaderTooLarge)
      case B.break (== '\n') bs of
        (xs, "") -> do
          (ll, ys) <- go newLimit
          return (ll, xs `B.append` ys)
        (xs, ys) -> do
          connectionUnread_ c (B.drop 1 ys)
          return (newLimit, xs)
    stripCR bs
      | (not . B.null) bs && B.last bs == '\r' = B.init bs
      | otherwise = bs
