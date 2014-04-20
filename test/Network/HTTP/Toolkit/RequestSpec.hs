{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.RequestSpec (main, spec) where

import           Helper

import           Network.HTTP.Toolkit.Request

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseRequestLine" $ do
    it "parses HTTP request line" $ do
      parseRequestLine "GET /index.html HTTP/1.1" `shouldBe` Just ("GET", "/index.html")

    it "returns Nothing on parse error" $ do
      parseRequestLine "foo" `shouldBe` Nothing
