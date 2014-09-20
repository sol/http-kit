{-# LANGUAGE OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Network.HTTP.Toolkit.Response (
  Response(..)
, readResponse
, readResponseWithLimit
, parseStatusLine
, parseHttpVersion
, parseStatus

, simpleResponse
, sendResponse
, formatStatusLine

, determineResponseBodyType
) where

import           Control.Applicative
import           Control.Monad (guard)
import           Control.Exception
import qualified Text.Read as T
import           Data.Maybe
import           Data.Foldable
import           Data.Traversable
import           Data.Char
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Error
import           Network.HTTP.Toolkit.InputStream
import           Network.HTTP.Toolkit.Header
import           Network.HTTP.Toolkit.Body

data Response a = Response {
  responseVersion :: HttpVersion
, responseStatus :: Status
, responseHeaders :: [Header]
, responseBody :: a
} deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Same as `readResponseWithLimit` with a `Limit` of
-- `defaultHeaderSizeLimit`.
readResponse :: Bool -> Method -> InputStream -> IO (Response BodyReader)
readResponse = readResponseWithLimit defaultHeaderSizeLimit

-- | Read response from provided `InputStream`.
--
-- The second argument is passed to `makeChunkedReader`.
--
-- The corresponding request `Method` has to be specified so that the body length can be determined (see
-- <http://tools.ietf.org/html/rfc2616#section-4.4 RFC 2616, Section 4.4>).
--
-- Throws:
--
-- * `InvalidStatusLine` if status-line is malformed.
--
-- * `HeaderTooLarge` if status-line and headers together exceed the specified size `Limit`
--
-- * `InvalidHeader` if status-line is missing or a header is malformed
readResponseWithLimit :: Limit -> Bool -> Method -> InputStream -> IO (Response BodyReader)
readResponseWithLimit limit raw method c = do
  (startLine, headers) <- readMessageHeader limit c
  (version, status) <- parseStatusLine_ startLine
  Response version status headers <$> makeBodyReader raw (determineResponseBodyType method status headers) c

parseStatusLine_ :: ByteString -> IO (HttpVersion, Status)
parseStatusLine_ input = maybe (throwIO $ InvalidStatusLine input) return (parseStatusLine input)

-- | Parse status-line (see <http://tools.ietf.org/html/rfc2616#section-6.1 RFC 2616, Section 6.1>).
parseStatusLine :: ByteString -> Maybe (HttpVersion, Status)
parseStatusLine input = case breakOnSpace input of
  (version, status) -> (,) <$> parseHttpVersion version <*> (uncurry mkStatus <$> parseStatus status)

parseStatus :: ByteString -> Maybe (Int, ByteString)
parseStatus input = case breakOnSpace input of
  (code, message) -> (,) <$> readMaybe code <*> pure message

breakOnSpace :: ByteString -> (ByteString, ByteString)
breakOnSpace input = B.dropWhile isSpace <$> B.break isSpace input

-- | Parse HTTP version (see <http://tools.ietf.org/html/rfc2616#section-6.1.1 RFC 2616, Section 6.1.1>).
parseHttpVersion :: ByteString -> Maybe HttpVersion
parseHttpVersion input = case B.split '.' <$> B.splitAt 5 input of
  (x, [major, minor]) | CI.mk x == "http/" -> HttpVersion <$> readMaybe major <*> readMaybe minor
  _ -> Nothing

readMaybe :: Read a => ByteString -> Maybe a
readMaybe = T.readMaybe . B.unpack

-- | Determine the message `BodyType` from a given `Method`, `Status`, and list
-- of message headers (as of
-- <http://tools.ietf.org/html/rfc2616#section-4.4 RFC 2616, Section 4.4>).
determineResponseBodyType :: Method -> Status -> [Header] -> BodyType
determineResponseBodyType method status headers = fromMaybe Unlimited $ none <|> bodyTypeFromHeaders headers
  where
    none = guard hasNoResponseBody >> Just None
    code = statusCode status
    hasNoResponseBody =
         method == methodHead
      || (100 <= code && code < 200)
      || code == 204
      || code == 304

-- | Format status-line.
formatStatusLine :: HttpVersion -> Status -> ByteString
formatStatusLine version status = B.concat [B.pack $ show version," ", B.pack $ show (statusCode status), " ", statusMessage status]

-- | Send a simple HTTP/1.1 response.  The provided `ByteString` is used as the
-- message body.  A suitable @Content-Length@ header is added to the specified
-- list of headers.
--
-- /Note:/ The first argument to this function is used to send the data.  For
-- space efficiency it may be called multiple times.
simpleResponse :: (ByteString -> IO ()) -> Status -> [Header] -> ByteString -> IO ()
simpleResponse send status headers_ body = do
  fromByteString body >>= sendResponse send . Response http11 status headers
  where
    headers = ("Content-Length", B.pack . show . B.length $ body) : headers_

-- | Send an HTTP response.
--
-- /Note:/ The first argument to this function is used to send the data.  For
-- space efficiency it may be called multiple times.
sendResponse :: (ByteString -> IO ()) -> (Response BodyReader) -> IO ()
sendResponse send (Response version status headers body) = do
  sendHeader send (formatStatusLine version status) headers
  sendBody send body
