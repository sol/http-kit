{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Toolkit.ResponseSpec (main, spec) where

import           Helper

import           Network.HTTP.Types

import           Network.HTTP.Toolkit.Response

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseStatusLine" $ do
    it "parses HTTP status line" $ do
      parseStatusLine "HTTP/1.1 200 OK" `shouldBe` Just (mkStatus 200 "OK")

    it "returns Nothing on invalid status code" $ do
      parseStatusLine "HTTP/1.1 foo OK" `shouldBe` Nothing

    it "returns Nothing on parse error" $ do
      parseStatusLine "foo" `shouldBe` Nothing
