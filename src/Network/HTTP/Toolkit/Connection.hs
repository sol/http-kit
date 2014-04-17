module Network.HTTP.Toolkit.Connection (
  Connection(..)
, connectionReadAtLeast
) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

data Connection = Connection {
  connectionRead :: IO ByteString
, connectionUnread :: ByteString -> IO ()
}

connectionReadAtLeast :: Connection -> Int -> IO ByteString
connectionReadAtLeast conn n = connectionRead conn >>= go
  where
    go :: ByteString -> IO ByteString
    go xs
      | B.length xs < n = do
          ys <- connectionRead conn
          go (xs `B.append` ys)
      | otherwise = return xs
