{-# LANGUAGE OverloadedStrings #-}
module HelperSpec (main, spec) where

import           Helper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "slice" $ do
    it "slices a ByteString" $ do
      slice 1 "hello" `shouldBe` ["h", "e", "l", "l", "o"]
      slice 5 "hello" `shouldBe` ["hello"]
      slice 3 "hello" `shouldBe` ["hel", "lo"]

    it "works for *arbitrary* input" $ do
      property $ \n bs -> do
        mconcat (slice n bs) `shouldBe` bs
