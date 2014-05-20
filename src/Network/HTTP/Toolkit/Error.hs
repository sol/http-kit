{-# LANGUAGE DeriveDataTypeable #-}
module Network.HTTP.Toolkit.Error where

import           Data.Typeable
import           Control.Monad (guard)
import           Control.Exception
import           Data.ByteString (ByteString)

data ToolkitError =
    -- | The input ended unpexpectedly while reading a message.
    UnexpectedEndOfInput
    -- | The request-line of the message is malformed.
  | InvalidRequestLine ByteString
    -- | The status-line of the message is malformed.
  | InvalidStatusLine ByteString
    -- | A header field is malformed.
  | InvalidHeader
    -- | The start-line of the message and all header fields together exceed
    -- the specified size `Network.HTTP.Toolkit.Header.Limit`.
  | HeaderTooLarge
    -- | The size of a body chunk exceeds
    -- `Network.HTTP.Toolkit.Body.maxChunkSize`.
  | ChunkTooLarge
    -- | A body chunk is malformed.
  | InvalidChunk
  deriving (Eq, Show, Typeable)

instance Exception ToolkitError

catchOnly :: (Eq e, Exception e) => IO a -> e -> IO a -> IO a
(action `catchOnly` e) handler = catchJust (guard . (== e)) action $ \() -> handler
