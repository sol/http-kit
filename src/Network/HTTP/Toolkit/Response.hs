{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.Response (
  ResponseHeader
, readResponse
, readResponseWithLimit
, parseStatusLine

, determineResponseBodyType
) where

import           Control.Applicative
import           Control.Monad (join, guard)
import           Control.Exception
import           Text.Read (readMaybe)
import           Data.Maybe
import           Data.Traversable (sequenceA)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Error
import           Network.HTTP.Toolkit.Connection
import           Network.HTTP.Toolkit.Header
import           Network.HTTP.Toolkit.Body

type ResponseHeader = MessageHeader Status

-- | Read response from provided connection.
--
-- Throws:
--
-- * `InvalidStatusLine` if request-line is malformed.
--
-- * `HeaderTooLarge` if the header size exceeds `defaultHeaderSizeLimit`.
--
-- * `InvalidHeader` if header is malformed.
readResponse :: Method -> Connection -> IO (ResponseHeader, BodyReader)
readResponse = readResponseWithLimit defaultHeaderSizeLimit

-- | Read response from provided connection.
--
-- Throws:
--
-- * `InvalidStatusLine` if request-line is malformed.
--
-- * `HeaderTooLarge` if the header size exceeds the specified `Limit`.
--
-- * `InvalidHeader` if header is malformed.
readResponseWithLimit :: Limit -> Method -> Connection -> IO (ResponseHeader, BodyReader)
readResponseWithLimit limit method c = do
  r@(MessageHeader status headers) <- join $ sequenceA . fmap parseStatusLine_ <$> readMessageHeaderWithLimit limit c
  (,) r <$> makeBodyReader c (determineResponseBodyType method status headers)

parseStatusLine_ :: ByteString -> IO Status
parseStatusLine_ input = maybe (throwIO $ InvalidStatusLine input) return (parseStatusLine input)

-- | Parse status-line (see <http://tools.ietf.org/html/rfc2616#section-6.1 RFC 2616, Section 6.1>).
parseStatusLine :: ByteString -> Maybe Status
parseStatusLine input = case B.words input of
  _ : status : message : _ -> mkStatus <$> (readMaybe $ B.unpack status) <*> Just message
  _ -> Nothing

-- | Determine the message `BodyType` from a given `Method`, `Status` and a
-- list of message headers (as of
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
