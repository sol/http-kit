{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.BodySpec (main, spec) where

import           Helper
import           System.Timeout

import           Data.Char
import           Data.Monoid
import           Data.List
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Numeric

import           Network.HTTP.Toolkit.Type
import           Network.HTTP.Toolkit.Connection
import           Network.HTTP.Toolkit.Body

main :: IO ()
main = hspec spec

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
  describe "makeLengthReader" $ do
    it "reads body with specified length" $ do
      property $ \body n -> do
        c <- mkConnection (slice n body)
        bodyReader <- makeLengthReader (B.length body) c
        consumeBody bodyReader `shouldReturn` body

    it "unreads *any* excess input" $ do
      property $ \body n remaining -> (not . B.null) remaining ==> do
        c <- mkConnection (slice n $ body `B.append` remaining)
        bodyReader <- makeLengthReader (B.length body) c
        _ <- consumeBody bodyReader
        connectionReadAtLeast c (B.length remaining) `shouldReturn` remaining

  describe "readChunkSize" $ do
    it "reads chunk size" $ do
      property $ \n -> do
        c <- mkConnection (slice n "fffffffffffffff;")
        readChunkSize c `shouldReturn` (maxChunkSize, "fffffffffffffff")

    context "when chunk size is too large" $ do
      it "throws ChunkTooLarge" $ do
        property $ \n -> do
          c <- mkConnection (slice n "1000000000000000")
          readChunkSize c `shouldThrow` (== ChunkTooLarge)

    context "when attacker tries to exhaust resources by sending infinite chunk size" $ do
      it "throws ChunkTooLarge" $ do
        let xs = repeat "17"
        c <- mkConnection xs
        timeout 100000 (readChunkSize c) `shouldThrow` (== ChunkTooLarge)

  describe "makeChunkedReader" $ do
    it "reads chunked body" $ do
      c <- mkConnection ["5\r\nhello\r\n0\r\n\r\n"]
      bodyReader <- makeChunkedReader c
      consumeBody bodyReader `shouldReturn` "5\r\nhello\r\n0\r\n\r\n"

    it "reads *arbitrary* chunked bodies" $ do
      property $ \body n -> do
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
        property $ \body n remaining -> (not . B.null) remaining ==> do
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
        property $ \body n -> do
          let bs = formatChunkedBody body
          bodyReader <- mkConnection (slice n bs) >>= makeChunkedReader
          _ <- consumeBody bodyReader
          bodyReader `shouldReturn` ""
