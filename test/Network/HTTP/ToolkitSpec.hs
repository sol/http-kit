{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ToolkitSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           System.Timeout

import           Control.Applicative
import           Control.Exception
import           Control.Concurrent.MVar
import           Data.Char
import           Data.Monoid
import           Data.List
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Numeric

import           Network.HTTP.Toolkit

main :: IO ()
main = hspec spec

mkConnection :: [ByteString] -> IO Connection
mkConnection input = do
  mvar <- newMVar input
  let cRead = modifyMVar mvar $ \bs -> case bs of
        x:xs -> return (xs, x)
        _ -> throwIO (userError "tried to read after body was fully consumed")
  let cUnread bs = modifyMVar_ mvar (return . (bs:))
  return $ Connection cRead cUnread

slice :: Int -> ByteString -> [ByteString]
slice n bs = case B.splitAt n bs of
  (xs, "") -> return xs
  (xs, ys) -> xs : slice n ys

data ChunkedBody = ChunkedBody [Chunk] Trailer
  deriving Show

data Chunk = Chunk ByteString ByteString
  deriving Show

type Trailer = [ByteString]

instance Arbitrary ChunkedBody where
  arbitrary = ChunkedBody <$> arbitrary <*> listOf header
    where
      header = B.pack <$> listOf1 (elements allowed)
      allowed = [chr 32 .. chr 126] \\ "\r\n"

instance Arbitrary Chunk where
  arbitrary = Chunk <$> extensions <*> body
    where
      body = arbitrary `suchThat` (not . B.null)
      extensions = B.pack <$> listOf (elements allowed)
      allowed = [chr 32 .. chr 126] \\ "\r\n"

formatChunkedBody :: ChunkedBody -> ByteString
formatChunkedBody (ChunkedBody chunks trailer) = mconcat (map formatChunk chunks ++ ["0\r\n", formatTrailer trailer, "\r\n"])
  where
    formatChunk :: Chunk -> ByteString
    formatChunk (Chunk e c) = mconcat [B.pack $ showHex (B.length c) "", formatExtension e, "\r\n", c, "\r\n"]

    formatExtension :: ByteString -> ByteString
    formatExtension e
      | B.null e = ""
      | otherwise = ";" `mappend` e

    formatTrailer :: [ByteString] -> ByteString
    formatTrailer xs
      | null xs = ""
      | otherwise = mconcat (map (`mappend` "\r\n") xs)

spec :: Spec
spec = do
  describe "readChunkSize" $ do
    it "reads chunk size" $ do
      c <- mkConnection ["fffffffffffffff;"]
      readChunkSize c `shouldReturn` (maxChunkSize, "fffffffffffffff")

    context "when chunk size is too large" $ do
      it "throws ChunkTooLarge" $ do
        c <- mkConnection ["1000000000000000"]
        readChunkSize c `shouldThrow` (== ChunkTooLarge)

    context "when attacker tries to exhaust resources by sending infinite chunk size" $ do
      it "throws ChunkTooLarge" $ do
        let xs = cycle ["17"]
        c <- mkConnection xs
        timeout 100000 (readChunkSize c) `shouldThrow` (== ChunkTooLarge)

  describe "makeChunkedReader" $ do
    it "reads chunked body" $ do
      c <- mkConnection ["5\r\nhello\r\n0\r\n\r\n"]
      bodyReader <- makeChunkedReader c
      consumeBody bodyReader `shouldReturn` "5\r\nhello\r\n0\r\n\r\n"

    it "reads *arbitrary* chunked bodies" $ do
      property $ \body (Positive n) -> do
        let bs = formatChunkedBody body
        bodyReader <- mkConnection (slice n bs) >>= makeChunkedReader
        consumeBody bodyReader `shouldReturn` bs

    context "when there is excess input" $ do
      it "unreads excess input" $ do
        c <- mkConnection ["5\r\nhello\r\n0\r\n\r\nfoo"]
        bodyReader <- makeChunkedReader c
        _ <- consumeBody bodyReader
        connectionRead c `shouldReturn` "foo"

      it "unreads *any* excess input" $ do
        property $ \body (Positive n) remaining -> (not . B.null) remaining ==> do
          let bs = formatChunkedBody body
          c <- mkConnection (slice n $ bs `B.append` remaining)
          bodyReader <- makeChunkedReader c
          _ <- consumeBody bodyReader
          connectionReadAtLeast c (B.length remaining) `shouldReturn` remaining

    context "when chunk size is too large" $ do
      it "throws ChunkTooLarge" $ do
        let xs = [B.pack $ showHex (succ maxChunkSize) "", "\r\nfoo\r\n0\r\n\r\n"]
        bodyReader <- mkConnection xs >>= makeChunkedReader
        consumeBody bodyReader `shouldThrow` (== ChunkTooLarge)

    context "when chunk size is missing" $ do
      it "throws InvalidChunk" $ do
        let xs = ["xxx"]
        bodyReader <- mkConnection xs >>= makeChunkedReader
        consumeBody bodyReader `shouldThrow` (== InvalidChunk)

    context "with chunk extensions" $ do
      it "reads chunked body" $ do
        c <- mkConnection ["5;foo=bar\r\nhello\r\n0\r\n\r\n"]
        bodyReader <- makeChunkedReader c
        consumeBody bodyReader `shouldReturn` "5;foo=bar\r\nhello\r\n0\r\n\r\n"

    context "with trailer" $ do
      it "reads chunked body" $ do
        c <- mkConnection ["5;foo=bar\r\nhello\r\n0\r\nfoo: 23\r\nbar: 42\r\n\r\n"]
        bodyReader <- makeChunkedReader c
        consumeBody bodyReader `shouldReturn` "5;foo=bar\r\nhello\r\n0\r\nfoo: 23\r\nbar: 42\r\n\r\n"

    context "when body has been fully consumed" $ do
      it "returns empty string" $ do
        property $ \body (Positive n) -> do
          let bs = formatChunkedBody body
          bodyReader <- mkConnection (slice n bs) >>= makeChunkedReader
          _ <- consumeBody bodyReader
          bodyReader `shouldReturn` ""
