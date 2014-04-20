module Network.HTTP.Toolkit.Response (
  Response
, readResponse
, readResponseWithLimit
, parseStatusLine
) where

import           Control.Applicative
import           Control.Monad (join)
import           Control.Exception
import           Text.Read (readMaybe)
import           Data.Traversable (sequenceA)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Type
import           Network.HTTP.Toolkit.Connection
import           Network.HTTP.Toolkit.Header

type Response = RequestResponse Status

readResponse :: Connection -> IO Response
readResponse c = join $ sequenceA . fmap parseStatusLine_ <$> readRequestResponse c

readResponseWithLimit :: Int -> Connection -> IO Response
readResponseWithLimit limit c = join $ sequenceA . fmap parseStatusLine_ <$> readRequestResponseWithLimit limit c

parseStatusLine_ :: ByteString -> IO Status
parseStatusLine_ input = maybe (throwIO $ InvalidStatusLine input) return (parseStatusLine input)

parseStatusLine :: ByteString -> Maybe Status
parseStatusLine input = case B.words input of
  _ : status : message : _ -> mkStatus <$> (readMaybe $ B.unpack status) <*> Just message
  _ -> Nothing
