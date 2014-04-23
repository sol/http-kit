{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.Request (
  RequestPath
, Request
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
type Request = MessageHeader (Method, RequestPath)

readRequest :: Connection -> IO (Request, BodyReader)
readRequest = readRequestWithLimit defaultHeaderSizeLimit

readRequestWithLimit :: Int -> Connection -> IO (Request, BodyReader)
readRequestWithLimit limit c = do
  r@(MessageHeader _ headers) <- join $ sequenceA . fmap parseRequestLine_ <$> readMessageHeaderWithLimit limit c
  (,) r <$> makeBodyReader c (determineRequestBodyType headers)

parseRequestLine_ :: ByteString -> IO (Method, ByteString)
parseRequestLine_ input = maybe (throwIO $ InvalidRequestLine input) return (parseRequestLine input)

parseRequestLine :: ByteString -> Maybe (Method, ByteString)
parseRequestLine input = case B.words input of
  method : path : _ -> Just (method, path)
  _ -> Nothing

-- as of http://tools.ietf.org/html/rfc2616#section-4.4
determineRequestBodyType :: [Header] -> BodyType
determineRequestBodyType = fromMaybe None . bodyTypeFromHeaders
