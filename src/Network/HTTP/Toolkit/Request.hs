{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.Request (
  RequestPath
, RequestHeader
, readRequest
, readRequestWithLimit
, parseRequestLine

, determineRequestBodyType
) where

import           Control.Applicative
import           Control.Monad (join)
import           Control.Exception
import           Data.Maybe
import           Data.Traversable (sequenceA)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Error
import           Network.HTTP.Toolkit.Connection
import           Network.HTTP.Toolkit.Header
import           Network.HTTP.Toolkit.Body

type RequestPath = ByteString
type RequestHeader = MessageHeader (Method, RequestPath)

-- | Read request from provided connection.
--
-- Throws:
--
-- * `InvalidRequestLine` if request-line is malformed.
--
-- * `HeaderTooLarge` if the header size exceeds `defaultHeaderSizeLimit`.
--
-- * `InvalidHeader` if header is malformed.
readRequest :: Connection -> IO (RequestHeader, BodyReader)
readRequest = readRequestWithLimit defaultHeaderSizeLimit

-- | Read request from provided connection.
--
-- Throws:
--
-- * `InvalidRequestLine` if request-line is malformed.
--
-- * `HeaderTooLarge` if the header size exceeds the specified `Limit`.
--
-- * `InvalidHeader` if header is malformed.
readRequestWithLimit :: Limit -> Connection -> IO (RequestHeader, BodyReader)
readRequestWithLimit limit c = do
  r@(MessageHeader _ headers) <- join $ sequenceA . fmap parseRequestLine_ <$> readMessageHeaderWithLimit limit c
  (,) r <$> makeBodyReader c (determineRequestBodyType headers)

parseRequestLine_ :: ByteString -> IO (Method, ByteString)
parseRequestLine_ input = maybe (throwIO $ InvalidRequestLine input) return (parseRequestLine input)

-- | Parse request-line (see <http://tools.ietf.org/html/rfc2616#section-5.1 RFC 2616, Section 5.1>).
parseRequestLine :: ByteString -> Maybe (Method, RequestPath)
parseRequestLine input = case B.words input of
  method : path : _ -> Just (method, path)
  _ -> Nothing

-- | Determine the message `BodyType` from a given list of message headers (as
-- of <http://tools.ietf.org/html/rfc2616#section-4.4 RFC 2616, Section 4.4>).
determineRequestBodyType :: [Header] -> BodyType
determineRequestBodyType = fromMaybe None . bodyTypeFromHeaders
