{-# LANGUAGE DeriveDataTypeable #-}
module Network.HTTP.Toolkit.Type where

import           Data.Typeable
import           Control.Exception

data ToolkitError = InvalidHeader | HeaderTooLarge | ChunkTooLarge | InvalidChunk
  deriving (Eq, Show, Typeable)

instance Exception ToolkitError
