{-# LANGUAGE OverloadedStrings #-}

-- | IPC handler for log access: @get_logs@.
--
-- Returns recent log entries from the shared 'LogSnapshotRef',
-- with optional level filtering and pagination.
module Seer.Command.Handlers.Log
  ( handleGetLogs
  ) where

import Data.Aeson (Value(..), object, (.=), (.:?))
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)

import Actor.Log (LogLevel(..), LogEntry(..), LogSnapshot(..), LogSnapshotRef, readLogSnapshotRef)
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)

-- | Handle @get_logs@ — return recent log entries.
--
-- Params (all optional):
--
-- @{ "level"?: "debug"|"info"|"warn"|"error", "limit"?: int, "offset"?: int }@
--
-- Defaults: level=info, limit=50, offset=0.
-- Entries are returned newest-first.
handleGetLogs :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetLogs ctx reqId params = do
  let opts = maybe defaultOpts id (Aeson.parseMaybe parseLogOpts params)
  case ccLogSnapshotRef ctx of
    Nothing ->
      pure $ errResponse reqId "log snapshot not available"
    Just ref -> do
      snap <- readLogSnapshotRef ref
      let filtered = filter (\e -> levelOrd (leLevel e) >= levelOrd (loMinLevel opts))
                            (lsEntries snap)
          -- lsEntries is oldest-first; reverse for newest-first
          reversed = reverse filtered
          paged = take (loLimit opts) (drop (loOffset opts) reversed)
          entries = map entryToJSON paged
      pure $ okResponse reqId $ object
        [ "count"   .= length entries
        , "total"   .= length filtered
        , "entries" .= entries
        ]

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

data LogOpts = LogOpts
  { loMinLevel :: !LogLevel
  , loLimit    :: !Int
  , loOffset   :: !Int
  }

defaultOpts :: LogOpts
defaultOpts = LogOpts LogInfo 50 0

parseLogOpts :: Value -> Aeson.Parser LogOpts
parseLogOpts = Aeson.withObject "get_logs" $ \o -> do
  mLevel  <- o .:? "level"
  mLimit  <- o .:? "limit"
  mOffset <- o .:? "offset"
  let level  = maybe LogInfo parseLevel mLevel
      limit  = maybe 50 (max 1 . min 1000) mLimit
      offset = maybe 0 (max 0) mOffset
  pure (LogOpts level limit offset)

parseLevel :: Text -> LogLevel
parseLevel "debug" = LogDebug
parseLevel "info"  = LogInfo
parseLevel "warn"  = LogWarn
parseLevel "error" = LogError
parseLevel _       = LogInfo

levelOrd :: LogLevel -> Int
levelOrd LogDebug = 0
levelOrd LogInfo  = 1
levelOrd LogWarn  = 2
levelOrd LogError = 3

levelToText :: LogLevel -> Text
levelToText LogDebug = "debug"
levelToText LogInfo  = "info"
levelToText LogWarn  = "warn"
levelToText LogError = "error"

entryToJSON :: LogEntry -> Value
entryToJSON e = object
  [ "level"   .= levelToText (leLevel e)
  , "message" .= leMessage e
  ]
