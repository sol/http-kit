{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.ResponseSpec (main, spec) where

import           Helper

import           Data.Foldable (forM_)
import qualified Data.ByteString.Char8 as B
import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Body
import           Network.HTTP.Toolkit.Response

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "readResponse" $ do
    context "when Transfer-Encoding is specified" $ do
      it "reads chunked body" $ do
        c <- mkConnection [
            "HTTP/1.1 200 OK\r\n"
          , "Transfer-Encoding: chunked\r\n"
          , "\r\n"
          , "5\r\nhello\r\n"
          , "0\r\n\r\n"
          ]
        (responseBody <$> readResponse "GET" c >>= consumeBody) `shouldReturn` "5\r\nhello\r\n0\r\n\r\n"

      context "when connection is closed early" $ do
        it "terminates body" $ do
          c <- mkConnection [
              "HTTP/1.1 200 OK\r\n"
            , "Transfer-Encoding: chunked\r\n"
            , "\r\n"
            , "5\r\nhel"
            , ""
            ]
          (responseBody <$> readResponse "GET" c >>= consumeBody) `shouldReturn` "5\r\nhel"

  describe "determineResponseBodyType" $ do
    let arbitraryHeaders :: Gen [Header]
        arbitraryHeaders = listOf arbitraryHeader
        arbitraryHeader :: Gen Header
        arbitraryHeader = elements [("Content-Length", "5"), ("Transfer-Encoding", "foo"), ("Transfer-Encoding", "identity")]

    it "returns Unlimited" $ do
      determineResponseBodyType "GET" status200 [] `shouldBe` Unlimited

    context "when Transfer-Encoding is specified" $ do
      it "returns Chunked" $ do
        determineResponseBodyType "GET" status200 [("Transfer-Encoding", "foo")] `shouldBe` Chunked

      it "gives Transfer-Encoding precedence over Content-Length" $ do
        determineResponseBodyType "GET" status200 [("Transfer-Encoding", "foo"), ("Content-Length", "5")] `shouldBe` Chunked

      it "ignores Transfer-Encoding when set to identity" $ do
        determineResponseBodyType "GET" status200 [("Transfer-Encoding", "identity"), ("Content-Length", "5")] `shouldBe` Length 5

    context "when Content-Length is specified" $ do
      it "returns Length" $ do
        property $ \(NonNegative n) -> do
          determineResponseBodyType "GET" status200 [("Content-Length", B.pack $ show n)] `shouldBe` Length n

    context "when request method is HEAD" $ do
      it "returns None" $ do
        forAll arbitraryHeaders $ \headers -> do
          determineResponseBodyType "HEAD" status200 headers `shouldBe` None

    context "when response status is 1xx, 204, or 304" $ do
      it "returns None" $ do
        forAll arbitraryHeaders $ \headers -> do
          forM_ [status100, status101, status204, status304] $ \status -> do
            determineResponseBodyType "GET" status headers `shouldBe` None

  describe "parseStatusLine" $ do
    it "parses HTTP status line" $ do
      parseStatusLine "HTTP/1.1 200 OK" `shouldBe` Just (mkStatus 200 "OK")

    it "returns Nothing on invalid status code" $ do
      parseStatusLine "HTTP/1.1 foo OK" `shouldBe` Nothing

    it "returns Nothing on parse error" $ do
      parseStatusLine "foo" `shouldBe` Nothing
