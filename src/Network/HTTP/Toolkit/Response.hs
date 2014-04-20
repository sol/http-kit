module Network.HTTP.Toolkit.Response (
  Response
, readResponse
, readResponseWithLimit
, parseStatusLine
) where

import           Control.Applicative
import           Text.Read (readMaybe)
import           Data.Traversable (sequenceA)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Connection
import           Network.HTTP.Toolkit.Header

type Response = RequestResponse Status

readResponse :: Connection -> IO (Maybe Response)
readResponse c = sequenceA . fmap parseStatusLine <$> readRequestResponse c

readResponseWithLimit :: Int -> Connection -> IO (Maybe Response)
readResponseWithLimit limit c = sequenceA . fmap parseStatusLine <$> readRequestResponseWithLimit limit c

parseStatusLine :: ByteString -> Maybe Status
parseStatusLine input = case B.words input of
  _ : status : message : _ -> mkStatus <$> (readMaybe $ B.unpack status) <*> Just message
  _ -> Nothing
