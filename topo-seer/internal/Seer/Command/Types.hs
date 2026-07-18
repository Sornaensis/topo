{-# LANGUAGE StrictData #-}

-- | Private in-process envelopes used while command-backed handlers implement
-- the typed AppService boundary.
module Seer.Command.Types
  ( SeerCommand(..)
  , SeerResponse(..)
  , okResponse
  , errResponse
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)

-- | An in-process command dispatched by topo-seer compatibility code and tests.
data SeerCommand = SeerCommand
  { scId     :: !Int
  , scMethod :: !Text
  , scParams :: !Value
  } deriving (Eq, Show)

-- | The in-process result of dispatching a command-backed handler.
data SeerResponse = SeerResponse
  { srId      :: !Int
  , srSuccess :: !Bool
  , srResult  :: !Value
  , srError   :: !(Maybe Text)
  } deriving (Eq, Show)

okResponse :: Int -> Value -> SeerResponse
okResponse reqId result = SeerResponse
  { srId = reqId
  , srSuccess = True
  , srResult = result
  , srError = Nothing
  }

errResponse :: Int -> Text -> SeerResponse
errResponse reqId msg = SeerResponse
  { srId = reqId
  , srSuccess = False
  , srResult = Null
  , srError = Just msg
  }
