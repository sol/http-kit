{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.HeaderSpec (main, spec) where

import           Helper

import qualified Data.ByteString.Char8 as B

import           Network.HTTP.Toolkit.Type
import           Network.HTTP.Toolkit.Header

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "readRequestResponse" $ do
    it "reads request/response header" $ do
      c <- mkConnection ["HTTP/1.1 200 OK\r\nfoo: 23\r\nbar: 42\r\n\r\n"]
      readRequestResponse c `shouldReturn` RequestResponse "HTTP/1.1 200 OK" [("foo", "23"), ("bar", "42")]

    context "when start-line is missing" $ do
      it "throws InvalidHeader" $ do
        c <- mkConnection ["\r\n\r\n"]
        readRequestResponse c `shouldThrow` (== InvalidHeader)

    context "when header is malformed" $ do
      it "throws InvalidHeader" $ do
        c <- mkConnection ["HTTP/1.1 200 OK\r\nfoo\r\n\r\n"]
        readRequestResponse c `shouldThrow` (== InvalidHeader)

    context "when attacker sends infinite header" $ do
      it "throws HeaderTooLarge" $ do
        c <- mkConnection ("GET / HTTP/1.1" : repeat "foo: 23\r\n")
        readRequestResponse c `shouldThrow` (== HeaderTooLarge)

  describe "combineHeaderLines" $ do
    it "strips trailing whitespace" $ do
      combineHeaderLines ["foo\r", "bar\t", "baz \t\r"] `shouldBe` ["foo", "bar", "baz"]

    context "when a header line starts with whitespace" $ do
      it "combines that line with the previous line" $ do
        combineHeaderLines ["foo", "bar", "  baz"] `shouldBe` ["foo", "bar baz"]

    context "when multiple header lines start with whitespace" $ do
      it "combines consecutive lines" $ do
        combineHeaderLines ["foo", "  bar \r", "  baz"] `shouldBe` ["foo bar baz"]

  describe "readHeaderLines" $ do
    it "reads header lines" $ do
      property $ \n -> do
        c <- mkConnection (slice n "foo\r\nbar\r\nbaz\r\n\r\n")
        readHeaderLines defaultMaxHeaderSize c `shouldReturn` ["foo", "bar", "baz"]

    it "reads *arbitrary* header lines" $ do
      property $ \xs n -> do
        let ys = filter ((&&) <$> not . B.null <*> B.notElem '\n') xs
        c <- mkConnection (slice n $ B.intercalate "\r\n" ys `B.append` "\r\n\r\n")
        readHeaderLines maxBound c `shouldReturn` ys

  describe "parseHeaderFields" $ do
    it "reads headers" $ do
      parseHeaderFields ["foo: 23", "bar: 42"] `shouldBe` Just [("foo", "23"), ("bar", "42")]

    it "ignores empty header lines" $ do
      parseHeaderFields ["foo: 23", "  ", "bar: 42"] `shouldBe` Just [("foo", "23 "), ("bar", "42")]

    context "when header line starts with whitespace" $ do
      it "combines that line with the previous line" $ do
        parseHeaderFields ["foo: bar,", "  baz"] `shouldBe` Just [("foo", "bar, baz")]

    context "when a header line does not contain a colon" $ do
      it "returns Nothing" $ do
        parseHeaderFields ["foo"] `shouldBe` Nothing
