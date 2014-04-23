{-# LANGUAGE DeriveDataTypeable #-}
module Network.HTTP.Toolkit.Error where

import           Data.Typeable
import           Control.Exception
import           Data.ByteString (ByteString)

data ToolkitError =
    InvalidRequestLine ByteString
  | InvalidStatusLine ByteString
  | InvalidHeader
  | HeaderTooLarge
  | ChunkTooLarge
  | InvalidChunk
  deriving (Eq, Show, Typeable)

instance Exception ToolkitError
