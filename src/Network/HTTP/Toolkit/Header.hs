{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.Header (
  Limit
, readMessageHeader
, defaultHeaderSizeLimit
, parseHeaderFields
, sendHeader

-- * Internals
, readHeaderLines
, combineHeaderLines
) where

import           Control.Applicative
import           Control.Monad (when)
import           Control.Exception
import           Data.Foldable (forM_)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Error
import           Network.HTTP.Toolkit.Connection

-- | Message header size limit in bytes.
type Limit = Int

-- | Read start-line and message headers from provided `Connection`
-- (see <http://tools.ietf.org/html/rfc2616#section-4.1 RFC 2616, Section 4.1>).
--
-- Throws:
--
-- * `HeaderTooLarge` if start-line and headers together exceeds the specified size `Limit`
--
-- * `InvalidHeader` if start-line is missing or a header is malformed
readMessageHeader :: Limit -> Connection -> IO (ByteString, [Header])
readMessageHeader limit c = do
  hs <- readHeaderLines limit c
  maybe (throwIO InvalidHeader) return $ case hs of
    x : xs -> (,) x <$> parseHeaderFields xs
    [] -> Nothing

-- | Parse header fields according to
-- <http://tools.ietf.org/html/rfc2616#section-4.2 RFC 2616, Section 4.2>.
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

readHeaderLines :: Limit -> Connection -> IO [ByteString]
readHeaderLines n c = go n
  where
    go limit = do
      (newLimit, bs) <- readLine c limit
      if B.null bs then return [] else (bs :) <$> go newLimit

-- | The default message header size limit of 65536 bytes (64 KB).
defaultHeaderSizeLimit :: Limit
defaultHeaderSizeLimit = 64 * 1024

readLine :: Connection -> Limit -> IO (Limit, ByteString)
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
          connectionUnread c (B.drop 1 ys)
          return (newLimit, xs)
    stripCR bs
      | (not . B.null) bs && B.last bs == '\r' = B.init bs
      | otherwise = bs

-- | Send given start-line and message headers.
sendHeader :: (ByteString -> IO()) -> ByteString -> [Header] -> IO ()
sendHeader send startLine headers = do
  send startLine
  send "\r\n"
  forM_ headers $ \(k, v) -> do
    send $ B.concat [CI.original k, ": ", v, "\r\n"]
  send "\r\n"
