module Network.HTTP.Toolkit.Connection (
  Connection(..)
, connectionUnread_
, connectionReadAtLeast
) where

import           Control.Monad (unless)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

data Connection = Connection {
  connectionRead :: IO ByteString
, connectionUnread :: ByteString -> IO ()
}

connectionUnread_ :: Connection -> ByteString -> IO ()
connectionUnread_ conn bs = unless (B.null bs) (connectionUnread conn bs)

connectionReadAtLeast :: Connection -> Int -> IO ByteString
connectionReadAtLeast conn n = connectionRead conn >>= go
  where
    go :: ByteString -> IO ByteString
    go xs
      | B.length xs < n = do
          ys <- connectionRead conn
          go (xs `B.append` ys)
      | otherwise = return xs
