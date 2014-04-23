{-# LANGUAGE OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Network.HTTP.Toolkit.Header (
  MessageHeader(..)
, Limit
, readMessageHeader
, readMessageHeaderWithLimit
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
import           Data.Foldable (Foldable, forM_)
import           Data.Traversable (Traversable)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Error
import           Network.HTTP.Toolkit.Connection

-- | Message header size limit in bytes.
type Limit = Int

-- | An HTTP message header consiting of a start line and a list of header
-- fields.
data MessageHeader a = MessageHeader a [Header]
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Read `MessageHeader` from provided `Connection`.
--
-- Throws:
--
-- * `HeaderTooLarge` if the header size exceeds `defaultHeaderSizeLimit`.
--
-- * `InvalidHeader` if header is malformed.
readMessageHeader :: Connection -> IO (MessageHeader ByteString)
readMessageHeader = readMessageHeaderWithLimit defaultHeaderSizeLimit

-- | Read `MessageHeader` from provided `Connection`.
--
-- Throws:
--
-- * `HeaderTooLarge` if the header size exceeds the specified `Limit`.
--
-- * `InvalidHeader` if header is malformed.
readMessageHeaderWithLimit :: Limit -> Connection -> IO (MessageHeader ByteString)
readMessageHeaderWithLimit limit c = do
  hs <- readHeaderLines limit c
  case hs of
    x : xs -> maybe (throwIO InvalidHeader) (return . MessageHeader x) (parseHeaderFields xs)
    [] -> throwIO InvalidHeader

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

-- | Send given message header.
sendHeader :: (ByteString -> IO()) -> MessageHeader ByteString -> IO ()
sendHeader send (MessageHeader startLine headers) = do
  send startLine
  send "\r\n"
  forM_ headers $ \(k, v) -> do
    send $ B.concat [CI.original k, ": ", v, "\r\n"]
  send "\r\n"
