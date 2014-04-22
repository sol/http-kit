{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.Response (
  Response
, readResponse
, readResponseWithLimit
, parseStatusLine

, BodyType(..)
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

import           Network.HTTP.Toolkit.Type
import           Network.HTTP.Toolkit.Connection
import           Network.HTTP.Toolkit.Header
import           Network.HTTP.Toolkit.Body

type Response = RequestResponse Status

readResponse :: Method -> Connection -> IO (Response, BodyReader)
readResponse = readResponseWithLimit defaultHeaderSizeLimit

readResponseWithLimit :: Int -> Method -> Connection -> IO (Response, BodyReader)
readResponseWithLimit limit method c = do
  r <- join $ sequenceA . fmap parseStatusLine_ <$> readRequestResponseWithLimit limit c
  body <- case r of
    RequestResponse status headers -> case determineResponseBodyType method status headers of
      Chunked -> makeChunkedReader c
      Length n -> makeLengthReader n c
      Unlimited -> makeUnlimitedReader c
      None -> return (pure "")
  return (r, body)

parseStatusLine_ :: ByteString -> IO Status
parseStatusLine_ input = maybe (throwIO $ InvalidStatusLine input) return (parseStatusLine input)

parseStatusLine :: ByteString -> Maybe Status
parseStatusLine input = case B.words input of
  _ : status : message : _ -> mkStatus <$> (readMaybe $ B.unpack status) <*> Just message
  _ -> Nothing

data BodyType = Chunked | Length Int | Unlimited | None
  deriving (Eq, Show)

-- as of http://tools.ietf.org/html/rfc2616#section-4.4
determineResponseBodyType :: Method -> Status -> [Header] -> BodyType
determineResponseBodyType method status headers = fromMaybe Unlimited $ none <|> chunked <|> length_
  where
    chunked = lookup "Transfer-Encoding" headers >>= guard . (/= "identity") >> Just Chunked
    length_ = Length <$> (lookup "Content-Length" headers >>= readMaybe . B.unpack)
    none = guard hasNoResponseBody >> Just None
    code = statusCode status
    hasNoResponseBody =
         method == methodHead
      || (100 <= code && code < 200)
      || code == 204
      || code == 304
