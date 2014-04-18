{-# LANGUAGE OverloadedStrings #-}
module Helper (
  module Test.Hspec
, module Test.QuickCheck
, module Control.Applicative
, mkConnection
, slice
) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Control.Applicative
import           Control.Exception
import           Control.Concurrent.MVar
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B

import           Network.HTTP.Toolkit.Connection

slice :: Int -> ByteString -> [ByteString]
slice m bs
  | B.null bs = []
  | otherwise = go bs
  where
    len = B.length bs
    n = case m `mod` len of
      0 -> len
      o -> o
    go xs = case B.splitAt n xs of
      (ys, "") -> return ys
      (ys, zs) -> ys : go zs

mkConnection :: [ByteString] -> IO Connection
mkConnection input = do
  mvar <- newMVar input
  let cRead = modifyMVar mvar $ \bs -> case bs of
        x:xs -> return (xs, x)
        _ -> throwIO (userError "tried to read after body was fully consumed")
  let cUnread bs = modifyMVar_ mvar (return . (bs:))
  return $ Connection cRead cUnread
