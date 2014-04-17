{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Network.HTTP.Toolkit.Body (
-- * Reader
  BodyReader
, InvalidBody(..)
, consumeBody

-- * Chunked body
, maxChunkSize
, makeChunkedReader
, readChunkSize
) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Data.Typeable
import           Data.Char
import           Data.Bits
import           Data.IORef
import           Numeric
import           Data.ByteString (ByteString, breakByte)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import           Network.HTTP.Toolkit.Connection

maxChunkSize :: Int
maxChunkSize = pred $ 2 ^ (maxChunkSizeDigits * 4)

maxChunkSizeDigits :: Int
maxChunkSizeDigits = pred (bitSize (undefined :: Int) `div` 4)

data InvalidBody = ChunkTooLarge | InvalidChunk
  deriving (Eq, Show, Typeable)

instance Exception InvalidBody

connectionUnread_ :: Connection -> ByteString -> IO ()
connectionUnread_ conn bs = unless (B.null bs) (connectionUnread conn bs)

-- |
-- A reader for HTTP bodies.  It returns chunks of the body as long as there is
-- more data to consume.  When the body has been fully consumed, it returns
-- `B.empty`.
type BodyReader	= IO ByteString

consumeBody :: IO ByteString -> IO ByteString
consumeBody bodyReader = B.concat <$> go
  where
    go :: IO [ByteString]
    go = do
      bs <- bodyReader
      case bs of
        "" -> return []
        _ -> (bs:) <$> go

data Where = Data | Extension

data State = More Int Where | Trailer | Done

-- | Create a reader for chunked bodies.
--
-- The reader throws `InvalidChunk` if the body is malformed.
--
-- The reader throws `ChunkTooLarge` if a chunk exceeds `maxChunkSize`.
makeChunkedReader :: Connection -> IO BodyReader
makeChunkedReader conn = do
  ref <- newIORef (More 0 Data)
  return $ do
    c <- readIORef ref
    case c of
      More 0 Data -> do
        (n, xs) <- readChunkSize conn
        writeIORef ref (More n Extension)
        return xs
      More n Extension -> do
        bs <- connectionRead conn
        case breakOnNewline bs of
          ("", _) -> 
            if n > 0
              then do
                handleChunkData ref (n + 3) bs
              else do
                writeIORef ref Trailer
                connectionUnread_ conn bs
                readTrailer ref
          (xs, ys) -> do
            connectionUnread_ conn ys
            return xs
      More n Data -> do
        connectionRead conn >>= handleChunkData ref n
      Trailer -> readTrailer ref
      Done -> return ""
  where
    handleChunkData :: IORef State -> Int -> ByteString -> IO ByteString
    handleChunkData ref n bs = do
      let (xs, ys) = B.splitAt n bs
      connectionUnread_ conn ys
      writeIORef ref (More (n - B.length xs) Data)
      return xs

    readTrailer :: IORef State -> IO ByteString
    readTrailer ref = do
      xs <- connectionReadAtLeast conn 3
      if "\n\r\n" `B.isPrefixOf` xs
        then do
          writeIORef ref Done
          let (ys, zs) = B.splitAt 3 xs
          connectionUnread_ conn zs
          return ys
        else do
          let Just (y, ys) = B.uncons xs
          case breakOnNewline ys of
            (zs, rest) -> do
              connectionUnread_ conn rest
              return (y `B.cons` zs)

breakOnNewline :: ByteString -> (ByteString, ByteString)
breakOnNewline = breakByte 10

readChunkSize :: Connection -> IO (Int, ByteString)
readChunkSize conn = do
  xs <- go 0
  case readHex (B8.unpack xs) of
    [(n, "")] -> return (n, xs)
    _ -> throwIO InvalidChunk
  where
    go :: Int -> IO ByteString
    go n = do
      bs <- connectionRead conn
      case B8.span isHexDigit bs of
        (xs, ys) -> do
          let m = (n + B.length xs)
          when (m > maxChunkSizeDigits) $
            throwIO ChunkTooLarge
          case ys of
            "" -> do
              zs <- go m
              return (xs `B.append` zs)
            _ -> do
              connectionUnread conn ys
              return xs
