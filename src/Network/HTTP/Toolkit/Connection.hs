module Network.HTTP.Toolkit.Connection (
  Connection(..)
, makeConnection
, connectionUnread_
, connectionReadAtLeast
) where

import           Control.Monad (join, unless)
import           Data.IORef
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

data Connection = Connection {
  connectionRead :: IO ByteString
, connectionUnread :: ByteString -> IO ()
}

makeConnection :: IO ByteString -> IO Connection
makeConnection r = do
  ref <- newIORef []
  return $ Connection {
      connectionRead = join $ atomicModifyIORef ref $ \xs -> case xs of
        y : ys -> (ys, return y)
        _ -> (xs, r)
    , connectionUnread = \x -> atomicModifyIORef ref $ \xs -> (x : xs, ())
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
