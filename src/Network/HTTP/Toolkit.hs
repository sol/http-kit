module Network.HTTP.Toolkit (
-- * Connection
  Connection
, makeConnection
, connectionFromHandle

-- * Handling requests
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

-- * Error type
, ToolkitError(..)
) where

import           Network.HTTP.Toolkit.Body
import           Network.HTTP.Toolkit.Connection
import           Network.HTTP.Toolkit.Request
import           Network.HTTP.Toolkit.Response
import           Network.HTTP.Toolkit.Error
