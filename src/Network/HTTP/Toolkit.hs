{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Network.HTTP.Toolkit (
-- * Exceptions
-- |
-- * All functions that consume input fail with `UnexpectedEndOfInput` if the
--   input ends before the function can completely successfully.
--
-- * All cases where a function may fail with an exception other than
--   @UnexpectedEndOfInput@ are documented thoroughly on a per function level.
--
  ToolkitError(..)

-- * Input handling
, InputStream(..)
, makeInputStream
, inputStreamFromHandle

-- * Handling requests
, RequestPath
, Request(..)
, readRequestWithLimit
, readRequest
, sendRequest

-- * Handling responses
, Response(..)
, readResponseWithLimit
, readResponse
, sendResponse
, simpleResponse

-- * Handling message bodies
, BodyReader
, sendBody
, consumeBody

-- * Deprecated types and functions
, Connection
, makeConnection
, connectionFromHandle
) where

import           Network.HTTP.Toolkit.Body
import           Network.HTTP.Toolkit.InputStream
import           Network.HTTP.Toolkit.Connection
import           Network.HTTP.Toolkit.Request
import           Network.HTTP.Toolkit.Response
import           Network.HTTP.Toolkit.Error
