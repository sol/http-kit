module Network.HTTP.Toolkit.Connection {-# DEPRECATED "use \"Network.HTTP.Toolkit.InputStream\" instead" #-} (
  Connection
, InputStream(..)
, makeConnection
, connectionFromHandle
, connectionRead
, connectionUnread
, connectionReadAtLeast
) where

import           System.IO
import           Data.ByteString (ByteString)

import           Network.HTTP.Toolkit.InputStream

type Connection = InputStream

makeConnection :: IO ByteString -> IO Connection
makeConnection = makeInputStream

connectionFromHandle :: Handle -> IO Connection
connectionFromHandle = inputStreamFromHandle

connectionRead :: Connection -> IO ByteString
connectionRead = readInput

connectionUnread :: Connection -> ByteString -> IO ()
connectionUnread = unreadInput

connectionReadAtLeast :: Connection -> Int -> IO ByteString
connectionReadAtLeast = readAtLeast
