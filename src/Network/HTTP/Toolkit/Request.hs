module Network.HTTP.Toolkit.Request (
  Request
, readRequest
, readRequestWithLimit
, parseRequestLine
) where

import           Control.Applicative
import           Control.Monad (join)
import           Control.Exception
import           Data.Traversable (sequenceA)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Type
import           Network.HTTP.Toolkit.Connection
import           Network.HTTP.Toolkit.Header


type Request = RequestResponse (Method, ByteString)

readRequest :: Connection -> IO Request
readRequest c = join $ sequenceA . fmap parseRequestLine_ <$> readRequestResponse c

readRequestWithLimit :: Int -> Connection -> IO Request
readRequestWithLimit limit c = join $ sequenceA . fmap parseRequestLine_ <$> readRequestResponseWithLimit limit c

parseRequestLine_ :: ByteString -> IO (Method, ByteString)
parseRequestLine_ input = maybe (throwIO $ InvalidRequestLine input) return (parseRequestLine input)

parseRequestLine :: ByteString -> Maybe (Method, ByteString)
parseRequestLine input = case B.words input of
  method : path : _ -> Just (method, path)
  _ -> Nothing
