module Network.HTTP.Toolkit.Connection (
  Connection(..)
, makeConnection
, connectionRead
, connectionUnread
, connectionReadAtLeast
) where

import           Control.Monad (join, unless)
import           Data.IORef
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

data Connection = Connection {
  _read :: IO ByteString
, _unread :: ByteString -> IO ()
}

makeConnection :: IO ByteString -> IO Connection
makeConnection r = do
  ref <- newIORef []
  return $ Connection {
      _read = join $ atomicModifyIORef ref $ \xs -> case xs of
        y : ys -> (ys, return y)
        _ -> (xs, r)
    , _unread = \x -> atomicModifyIORef ref $ \xs -> (x : xs, ())
    }

connectionRead :: Connection -> IO ByteString
connectionRead = _read

connectionUnread :: Connection -> ByteString -> IO ()
connectionUnread conn bs = unless (B.null bs) (_unread conn bs)

connectionReadAtLeast :: Connection -> Int -> IO ByteString
connectionReadAtLeast conn n = connectionRead conn >>= go
  where
    go :: ByteString -> IO ByteString
    go xs
      | B.length xs < n = do
          ys <- connectionRead conn
          go (xs `B.append` ys)
      | otherwise = return xs
