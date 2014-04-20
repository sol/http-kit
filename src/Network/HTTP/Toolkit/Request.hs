module Network.HTTP.Toolkit.Request (
  Request
, readRequest
, readRequestWithLimit
, parseRequestLine
) where

import           Control.Applicative
import           Data.Traversable (sequenceA)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Connection
import           Network.HTTP.Toolkit.Header

type Request = RequestResponse (Method, ByteString)

readRequest :: Connection -> IO (Maybe Request)
readRequest c = sequenceA . fmap parseRequestLine <$> readRequestResponse c

readRequestWithLimit :: Int -> Connection -> IO (Maybe Request)
readRequestWithLimit limit c = sequenceA . fmap parseRequestLine <$> readRequestResponseWithLimit limit c

parseRequestLine :: ByteString -> Maybe (Method, ByteString)
parseRequestLine input = case B.words input of
  method : path : _ -> Just (method, path)
  _ -> Nothing
