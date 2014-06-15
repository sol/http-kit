module Network.HTTP.Toolkit.InputStream (
  InputStream(..)
, makeInputStream
, inputStreamFromHandle
, readInput
, unreadInput
, readAtLeast
) where

import           Prelude hiding (read)
import           Control.Monad (join, when, unless)
import           Control.Exception
import           System.IO (Handle)
import           Data.IORef
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import           Network.HTTP.Toolkit.Error

-- | An abstraction for input streams that allows to read and unread input.
data InputStream = InputStream {
  _read :: IO ByteString
, _unread :: ByteString -> IO ()
}

-- | Create an `InputStream` from provided `Handle`.
inputStreamFromHandle :: Handle -> IO InputStream
inputStreamFromHandle h = makeInputStream (B.hGetSome h 4096)

-- | Create an `InputStream` from provided @IO@ action.
makeInputStream :: IO ByteString -> IO InputStream
makeInputStream read = do
  ref <- newIORef []
  return $ InputStream {
      _read = join $ atomicModifyIORef ref $ \xs -> case xs of
        y : ys -> (ys, return y)
        _ -> (xs, read)
    , _unread = \x -> atomicModifyIORef ref $ \xs -> (x : xs, ())
    }

-- | Read some input.
readInput :: InputStream -> IO ByteString
readInput c = do
  bs <- _read c
  when (B.null bs) $ throwIO UnexpectedEndOfInput
  return bs

-- | Push back some input.  The pushed back input will be returned by a later
-- call to `readInput`.
unreadInput :: InputStream -> ByteString -> IO ()
unreadInput conn bs = unless (B.null bs) (_unread conn bs)

-- | Read at least the specified number of bytes from the input stream.
readAtLeast :: InputStream -> Int -> IO ByteString
readAtLeast conn n = readInput conn >>= go
  where
    go :: ByteString -> IO ByteString
    go xs
      | B.length xs < n = do
          ys <- readInput conn
          go (xs `B.append` ys)
      | otherwise = return xs
