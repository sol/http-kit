module Network.HTTP.Toolkit.Connection (
  Connection(..)
, makeConnection
, connectionFromHandle
, connectionRead
, connectionUnread
, connectionReadAtLeast
) where

import           Prelude hiding (read)
import           Control.Monad (join, unless)
import           System.IO (Handle)
import           Data.IORef
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

-- | An abstract connection type that allows to read and unread input.
data Connection = Connection {
  _read :: IO ByteString
, _unread :: ByteString -> IO ()
}

-- | Create a `Connection` from provided `Handle`.
connectionFromHandle :: Handle -> IO Connection
connectionFromHandle h = makeConnection (B.hGetSome h 4096)

-- | Create a `Connection` from provided @IO@ action.
makeConnection :: IO ByteString -> IO Connection
makeConnection read = do
  ref <- newIORef []
  return $ Connection {
      _read = join $ atomicModifyIORef ref $ \xs -> case xs of
        y : ys -> (ys, return y)
        _ -> (xs, read)
    , _unread = \x -> atomicModifyIORef ref $ \xs -> (x : xs, ())
    }

-- | Read some input.
connectionRead :: Connection -> IO ByteString
connectionRead = _read

-- | Push back some input.  The pushed back input will be returned by a later
-- call to `connectionRead`.
connectionUnread :: Connection -> ByteString -> IO ()
connectionUnread conn bs = unless (B.null bs) (_unread conn bs)

-- | Read at least the specified number of bytes from the input stream.
connectionReadAtLeast :: Connection -> Int -> IO ByteString
connectionReadAtLeast conn n = connectionRead conn >>= go
  where
    go :: ByteString -> IO ByteString
    go xs
      | B.length xs < n = do
          ys <- connectionRead conn
          go (xs `B.append` ys)
      | otherwise = return xs
