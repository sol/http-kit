{-# LANGUAGE DeriveDataTypeable #-}
module Network.HTTP.Toolkit.Error where

import           Data.Typeable
import           Control.Exception
import           Data.ByteString (ByteString)

data ToolkitError =
    -- | Parsing of HTTP request-line failed.
    InvalidRequestLine ByteString
    -- | Parsing of HTTP status-line failed.
  | InvalidStatusLine ByteString
    -- | A header field is malformed.
  | InvalidHeader
    -- | The message header exceeds the specified
    -- `Network.HTTP.Toolkit.Header.Limit`.
  | HeaderTooLarge
    -- | The size of a body chunk exceeds
    -- `Network.HTTP.Toolkit.Body.maxChunkSize`.
  | ChunkTooLarge
    -- | A body chunk is malformed.
  | InvalidChunk
  deriving (Eq, Show, Typeable)

instance Exception ToolkitError
