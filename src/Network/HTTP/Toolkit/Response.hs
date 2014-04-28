{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
module Network.HTTP.Toolkit.Response (
  Response(..)
, readResponse
, readResponseWithLimit
, parseStatusLine

, simpleResponse
, sendResponse
, formatStatusLine

, determineResponseBodyType
) where

import           Control.Applicative
import           Control.Monad (guard)
import           Control.Exception
import           Text.Read (readMaybe)
import           Data.Maybe
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Error
import           Network.HTTP.Toolkit.Connection
import           Network.HTTP.Toolkit.Header
import           Network.HTTP.Toolkit.Body

data Response a = Response {
  responseStatus :: Status
, responseHeaders :: [Header]
, responseBody :: a
} deriving (Eq, Show, Functor)

-- | Same as `readResponseWithLimit` with a `Limit` of
-- `defaultHeaderSizeLimit`.
readResponse :: Method -> Connection -> IO (Response BodyReader)
readResponse = readResponseWithLimit defaultHeaderSizeLimit

-- | Read response from provided connection.
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
readResponseWithLimit :: Limit -> Method -> Connection -> IO (Response BodyReader)
readResponseWithLimit limit method c = do
  (startLine, headers) <- readMessageHeader limit c
  status <- parseStatusLine_ startLine
  Response status headers <$> makeBodyReader c (determineResponseBodyType method status headers)

parseStatusLine_ :: ByteString -> IO Status
parseStatusLine_ input = maybe (throwIO $ InvalidStatusLine input) return (parseStatusLine input)

-- | Parse status-line (see <http://tools.ietf.org/html/rfc2616#section-6.1 RFC 2616, Section 6.1>).
parseStatusLine :: ByteString -> Maybe Status
parseStatusLine input = case B.words input of
  _ : status : message : _ -> mkStatus <$> (readMaybe $ B.unpack status) <*> Just message
  _ -> Nothing

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
formatStatusLine :: Status -> ByteString
formatStatusLine status = B.concat ["HTTP/1.1 ", B.pack $ show (statusCode status), " ", statusMessage status]

-- | Send a simple HTTP response.  The provided `ByteString` is used as the
-- message body.  A suitable @Content-Length@ header is added to the specified
-- list of headers.
--
-- /Note:/ The first argument to this function is used to send the data.  For
-- space efficiency it may be called multiple times.
simpleResponse :: (ByteString -> IO ()) -> Status -> [Header] -> ByteString -> IO ()
simpleResponse send status headers_ body = do
  fromByteString body >>= sendResponse send . Response status headers
  where
    headers = ("Content-Length", B.pack . show . B.length $ body) : headers_

-- | Send an HTTP response.
--
-- /Note:/ The first argument to this function is used to send the data.  For
-- space efficiency it may be called multiple times.
sendResponse :: (ByteString -> IO ()) -> (Response BodyReader) -> IO ()
sendResponse send (Response status headers body) = do
  sendHeader send (formatStatusLine status) headers
  sendBody send body
