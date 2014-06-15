{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.Body (
  BodyReader
, BodyType(..)
, bodyTypeFromHeaders
, makeBodyReader
, consumeBody
, sendBody
, fromByteString

-- * Handling of specific body types
, maxChunkSize
, makeChunkedReader
, readChunkSize
, makeLengthReader
, makeUnlimitedReader
) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception
import           Text.Read (readMaybe)
import           Data.Maybe
import           Data.Char
import           Data.Bits
import           Data.IORef
import           Numeric
import           Data.ByteString (ByteString, breakByte)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Util
import           Network.HTTP.Toolkit.Error
import           Network.HTTP.Toolkit.InputStream

data BodyType =
    -- | The message has no body.
    None
    -- | The message has a body. /Chunked transfer coding/ is used to determine
    -- the message length (see
    -- <http://tools.ietf.org/html/rfc2616#section-3.6.1 RFC 2616, Section 3.6.1>).
  | Chunked
    -- | The message has a body with a specified length.
  | Length Int
    -- | The message has a body. The body length is determined by the server
    -- closing the connection.  This is only a valid approach for response
    -- bodies. It can not be used for request bodies.
  | Unlimited
  deriving (Eq, Show)

-- | Determine the message `BodyType` from a given list of message headers (as
-- of <http://tools.ietf.org/html/rfc2616#section-4.4 RFC 2616, Section 4.4>).
--
-- This is only a partial breakdown.  Additional rules apply for request and
-- response bodies respectively (see
-- `Network.HTTP.Toolkit.Request.determineRequestBodyType` and
-- `Network.HTTP.Toolkit.Response.determineResponseBodyType`).
bodyTypeFromHeaders :: [Header] -> Maybe BodyType
bodyTypeFromHeaders headers = chunked <|> length_
  where
    chunked = lookup "Transfer-Encoding" headers >>= guard . (/= "identity") >> Just Chunked
    length_ = Length <$> (lookup "Content-Length" headers >>= readMaybe . B8.unpack)

-- | Create a `BodyReader` from provided `InputStream` and specified `BodyType`.
makeBodyReader :: InputStream -> BodyType -> IO BodyReader
makeBodyReader c bodyType = case bodyType of
  Chunked -> makeChunkedReader c
  Length n -> makeLengthReader n c
  Unlimited -> makeUnlimitedReader c
  None -> return (pure "")

-- |
-- The maximum size of a chunk in bytes when chunked transfer coding is used.
-- The value depends on the `bitSize` of `Int`:
--
-- * @2^28@ on 32-bit systems
--
-- * @2^60@ on 64-bit systems
maxChunkSize :: Int
maxChunkSize = pred $ 2 ^ (maxChunkSizeDigits * 4)

maxChunkSizeDigits :: Int
maxChunkSizeDigits = pred (bitSize (undefined :: Int) `div` 4)

-- | A reader for HTTP bodies.  It returns chunks of the body as long as there
-- is more data to consume.  When the body has been fully consumed, it returns
-- `B.empty`.
type BodyReader	= IO ByteString

-- | Strictly consume all input from provided `BodyReader`.
consumeBody :: BodyReader -> IO ByteString
consumeBody bodyReader = B.concat <$> go
  where
    go :: IO [ByteString]
    go = do
      bs <- bodyReader
      case bs of
        "" -> return []
        _ -> (bs:) <$> go

-- | Read input from provided `BodyReader` and wirte it to provided sink until
-- all input has been consumed.
--
-- /Note:/ The first argument to this function is used to send the data.  For
-- space efficiency it may be called multiple times.
sendBody :: (ByteString -> IO ()) -> BodyReader -> IO ()
sendBody send body = while (not . B.null) body send

-- | Create a `BodyReader` from provided `ByteString`.
fromByteString :: ByteString -> IO BodyReader
fromByteString input = do
  ref <- newIORef (Just input)
  return $ atomicModifyIORef ref $ ((,) Nothing) . fromMaybe ""

-- |
-- Create a reader for when the body length is determined by the server closing
-- the connection.
makeUnlimitedReader :: InputStream -> IO BodyReader
makeUnlimitedReader c = do
  ref <- newIORef False
  return $ do
    done <- readIORef ref
    if done
      then return ""
      else do
        xs <- readInput c `catchOnly` UnexpectedEndOfInput $ do
          writeIORef ref True
          return ""
        return xs

-- | Create a reader for bodies with a specified length.
makeLengthReader :: Int -> InputStream -> IO BodyReader
makeLengthReader total c = do
  ref <- newIORef total
  return $ do
    n <- readIORef ref
    if n == 0
      then return ""
      else do
        bs <- readInput c
        case B.splitAt n bs of
          (xs, ys) -> do
            writeIORef ref (n - B.length xs)
            unreadInput c ys
            return xs

data Where = Data | Extension

data State = More Int Where | Trailer | Done

-- | Create a reader for bodies with chunked transfer coding.
--
-- The reader throws:
--
-- * `InvalidChunk` if the body is malformed.
--
-- * `ChunkTooLarge` if the size of a chunk exceeds `maxChunkSize`.
makeChunkedReader :: InputStream -> IO BodyReader
makeChunkedReader conn = do
  ref <- newIORef (More 0 Data)
  return $ go ref `catchOnly` UnexpectedEndOfInput $ do
    writeIORef ref Done
    return ""
  where
    go ref = do
      c <- readIORef ref
      case c of
        More 0 Data -> do
          (n, xs) <- readChunkSize conn
          writeIORef ref (More n Extension)
          return xs
        More n Extension -> do
          bs <- readInput conn
          case breakOnNewline bs of
            ("", _) ->
              if n > 0
                then do
                  unreadInput conn bs
                  handleChunkData ref (n + 3)
                else do
                  writeIORef ref Trailer
                  unreadInput conn bs
                  readTrailer ref
            (xs, ys) -> do
              unreadInput conn ys
              return xs
        More n Data -> do
          handleChunkData ref n
        Trailer -> readTrailer ref
        Done -> return ""

    handleChunkData :: IORef State -> Int -> IO ByteString
    handleChunkData ref n = do
      bs <- readInput conn
      let (xs, ys) = B.splitAt n bs
      unreadInput conn ys
      writeIORef ref (More (n - B.length xs) Data)
      return xs

    readTrailer :: IORef State -> IO ByteString
    readTrailer ref = do
      xs <- readAtLeast conn 3
      if "\n\r\n" `B.isPrefixOf` xs
        then do
          writeIORef ref Done
          let (ys, zs) = B.splitAt 3 xs
          unreadInput conn zs
          return ys
        else do
          let Just (y, ys) = B.uncons xs
          case breakOnNewline ys of
            (zs, rest) -> do
              unreadInput conn rest
              return (y `B.cons` zs)

breakOnNewline :: ByteString -> (ByteString, ByteString)
breakOnNewline = breakByte 10

-- |
-- Read size of next body chunk for when chunked transfer coding is used.
--
-- Throws:
--
-- * `ChunkTooLarge` if chunk size exceeds `maxChunkSize`.
readChunkSize :: InputStream -> IO (Int, ByteString)
readChunkSize conn = do
  xs <- go 0
  case readHex (B8.unpack xs) of
    [(n, "")] -> return (n, xs)
    _ -> throwIO InvalidChunk
  where
    go :: Int -> IO ByteString
    go n = do
      bs <- readInput conn
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
              _unread conn ys
              return xs
