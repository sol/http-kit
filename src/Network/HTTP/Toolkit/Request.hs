{-# LANGUAGE OverloadedStrings, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Network.HTTP.Toolkit.Request (
  RequestPath
, Request(..)
, readRequest
, readRequestWithLimit
, parseRequestLine

, sendRequest
, formatRequestLine

, determineRequestBodyType
) where

import           Control.Applicative
import           Control.Exception
import           Data.Maybe
import           Data.Foldable
import           Data.Traversable
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Error
import           Network.HTTP.Toolkit.InputStream
import           Network.HTTP.Toolkit.Header
import           Network.HTTP.Toolkit.Body

type RequestPath = ByteString

data Request a = Request {
  requestMethod :: Method
, requestPath :: RequestPath
, requestHeaders :: [Header]
, requestBody :: a
} deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Same as `readRequestWithLimit` with a `Limit` of `defaultHeaderSizeLimit`.
readRequest :: InputStream -> IO (Request BodyReader)
readRequest = readRequestWithLimit defaultHeaderSizeLimit

-- | Read request from provided `InputStream`.
--
-- Throws:
--
-- * `InvalidRequestLine` if request-line is malformed.
--
-- * `HeaderTooLarge` if request-line and headers together exceed the specified size `Limit`
--
-- * `InvalidHeader` if request-line is missing or a header is malformed
readRequestWithLimit :: Limit -> InputStream -> IO (Request BodyReader)
readRequestWithLimit limit c = do
  (startLine, headers) <- readMessageHeader limit c
  (method, path) <- parseRequestLine_ startLine
  Request method path headers <$> makeBodyReader c (determineRequestBodyType headers)

parseRequestLine_ :: ByteString -> IO (Method, RequestPath)
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

-- | Format request-line.
formatRequestLine :: Method -> RequestPath -> ByteString
formatRequestLine method path = B.unwords [method, path, "HTTP/1.1"]

-- | Send an HTTP request.
--
-- /Note:/ The first argument to this function is used to send the data.  For
-- space efficiency it may be called multiple times.
sendRequest :: (ByteString -> IO ()) -> Request BodyReader -> IO ()
sendRequest send (Request method path headers body) = do
  sendHeader send (formatRequestLine method path) headers
  sendBody send body
