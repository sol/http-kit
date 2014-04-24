{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
module Network.HTTP.Toolkit.Response (
  Response(..)
, readResponse
, readResponseWithLimit
, parseStatusLine

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
-- Throws:
--
-- * `InvalidStatusLine` if status-line is malformed.
--
-- * `HeaderTooLarge` if the header size exceeds the specified `Limit`.
--
-- * `InvalidHeader` if header is malformed.
readResponseWithLimit :: Limit -> Method -> Connection -> IO (Response BodyReader)
readResponseWithLimit limit method c = do
  (MessageHeader startLine headers) <- readMessageHeader limit c
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

-- | Send an HTTP response.
sendResponse :: (ByteString -> IO ()) -> (Response BodyReader) -> IO ()
sendResponse send (Response status headers body) = do
  sendHeader send (formatStatusLine status) headers
  sendBody send body
