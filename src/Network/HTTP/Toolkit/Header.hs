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
import           Network.HTTP.Toolkit.InputStream

-- | Message header size limit in bytes.
type Limit = Int

-- | Read start-line and message headers from provided `InputStream`
-- (see <http://tools.ietf.org/html/rfc2616#section-4.1 RFC 2616, Section 4.1>).
--
-- Throws:
--
-- * `HeaderTooLarge` if start-line and headers together exceed the specified size `Limit`
--
-- * `InvalidHeader` if start-line is missing or a header is malformed
readMessageHeader :: Limit -> InputStream -> IO (ByteString, [Header])
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

readHeaderLines :: Limit -> InputStream -> IO [ByteString]
readHeaderLines n c = go n
  where
    go limit = do
      bs <- readHeaderLine c limit
      if B.null bs then return [] else (bs :) <$> go (limit - B.length bs)

-- | The default message header size limit of 65536 bytes (64 KB).
defaultHeaderSizeLimit :: Limit
defaultHeaderSizeLimit = 64 * 1024

readHeaderLine :: InputStream -> Limit -> IO ByteString
readHeaderLine c = fmap stripCR . go
  where
    go limit = do
      when (limit < 0) (throwIO HeaderTooLarge)
      bs <- readInput c
      case B.break (== '\n') bs of
        (xs, "") -> do
          ys <- go (limit - B.length xs)
          return (xs `B.append` ys)
        (xs, ys) -> do
          unreadInput c (B.drop 1 ys)
          return xs
    stripCR bs
      | (not . B.null) bs && B.last bs == '\r' = B.init bs
      | otherwise = bs

-- | Send given start-line and message headers.
--
-- /Note:/ The first argument to this function is used to send the data.  For
-- space efficiency it may be called multiple times.
sendHeader :: (ByteString -> IO ()) -> ByteString -> [Header] -> IO ()
sendHeader send startLine headers = do
  send startLine
  send "\r\n"
  forM_ headers $ \(k, v) -> do
    send $ B.concat [CI.original k, ": ", v, "\r\n"]
  send "\r\n"
