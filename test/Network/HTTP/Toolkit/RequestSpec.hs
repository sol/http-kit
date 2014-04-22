{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.RequestSpec (main, spec) where

import           Helper

import qualified Data.ByteString.Char8 as B

import           Network.HTTP.Toolkit.Body
import           Network.HTTP.Toolkit.Request

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "determineRequestBodyType" $ do
    it "returns None" $ do
      determineRequestBodyType [] `shouldBe` None

    context "when Transfer-Encoding is specified" $ do
      it "returns Chunked" $ do
        determineRequestBodyType [("Transfer-Encoding", "foo")] `shouldBe` Chunked

      it "gives Transfer-Encoding precedence over Content-Length" $ do
        determineRequestBodyType [("Transfer-Encoding", "foo"), ("Content-Length", "5")] `shouldBe` Chunked

      it "ignores Transfer-Encoding when set to identity" $ do
        determineRequestBodyType [("Transfer-Encoding", "identity"), ("Content-Length", "5")] `shouldBe` Length 5

    context "when Content-Length is specified" $ do
      it "returns Length" $ do
        property $ \(NonNegative n) -> do
          determineRequestBodyType [("Content-Length", B.pack $ show n)] `shouldBe` Length n

  describe "parseRequestLine" $ do
    it "parses HTTP request line" $ do
      parseRequestLine "GET /index.html HTTP/1.1" `shouldBe` Just ("GET", "/index.html")

    it "returns Nothing on parse error" $ do
      parseRequestLine "foo" `shouldBe` Nothing
