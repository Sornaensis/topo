{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Actor.Log
  ( Log
  , LogLevel(..)
  , LogEntry(..)
  , LogSnapshot(..)
  , LogSnapshotRef
  , logActorDef
  , appendLog
  , resetLogFile
  , setLogCollapsed
  , setLogMinLevel
  , scrollLog
  , setLogScroll
  , LogSnapshotReply
  , requestLogSnapshot
  , getLogSnapshot
  , setLogSnapshotRef
  , readLogSnapshotRef
  , newLogSnapshotRef
  , setLogFileHandle
  ) where

import Control.Exception (SomeException, handle)
import Data.Foldable (toList)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Hyperspace.Actor.Spec (OpTag(..))
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(..), BufferMode(..), hSetBuffering, openFile, hFlush)

data LogLevel
  = LogDebug
  | LogInfo
  | LogWarn
  | LogError
  deriving (Eq, Show)

data LogEntry = LogEntry
  { leLevel :: !LogLevel
  , leMessage :: !Text
  } deriving (Eq, Show)

data LogSnapshot = LogSnapshot
  { lsEntries :: ![LogEntry]
  , lsCollapsed :: !Bool
  , lsScroll :: !Int
  , lsMinLevel :: !LogLevel
  } deriving (Eq, Show)

-- | Shared reference for lock-free log snapshot reads by the render loop.
type LogSnapshotRef = IORef LogSnapshot

data LogState = LogState
  { stEntries :: !(Seq LogEntry)
  , stCollapsed :: !Bool
  , stScroll :: !Int
  , stLimit :: !Int
  , stMinLevel :: !LogLevel
  , stFileHandle :: !(Maybe Handle)
  , stSnapshotRef :: !(Maybe LogSnapshotRef)
  }

emptyLogState :: LogState
emptyLogState = LogState
  { stEntries = Seq.empty
  , stCollapsed = False
  , stScroll = 0
  , stLimit = 1000
  , stMinLevel = LogDebug
  , stFileHandle = Nothing
  , stSnapshotRef = Nothing
  }

appendEntry :: LogEntry -> LogState -> LogState
appendEntry entry st =
  let entries' = stEntries st Seq.|> entry
      trim = Seq.length entries' - stLimit st
      entries'' = if trim > 0 then Seq.drop trim entries' else entries'
  in st { stEntries = entries'' }

appendEntryWithFile :: LogEntry -> LogState -> IO LogState
appendEntryWithFile entry st = do
  appendToHandle (stFileHandle st) entry
  let st' = appendEntry entry st
  publishLogSnapshot st'
  pure st'

-- | Write a log entry to the cached file handle.  Falls back to a no-op if
-- the handle is unavailable.
appendToHandle :: Maybe Handle -> LogEntry -> IO ()
appendToHandle Nothing _ = pure ()
appendToHandle (Just h) entry =
  handle ignoreLogError $ do
    timestamp <- getCurrentTime
    let tsText = Text.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%z" timestamp)
        levelText = logLevelText (leLevel entry)
        line = tsText <> " [" <> levelText <> "] " <> leMessage entry <> "\n"
    TextIO.hPutStr h line
    hFlush h

-- | Publish the current log snapshot to the shared 'IORef', if registered.
publishLogSnapshot :: LogState -> IO ()
publishLogSnapshot st =
  case stSnapshotRef st of
    Nothing -> pure ()
    Just ref -> writeIORef ref (snapshotLog st)

logLevelText :: LogLevel -> Text
logLevelText level =
  case level of
    LogDebug -> "DEBUG"
    LogInfo -> "INFO"
    LogWarn -> "WARN"
    LogError -> "ERROR"

snapshotLog :: LogState -> LogSnapshot
snapshotLog st = LogSnapshot
  { lsEntries = filterEntries (stMinLevel st) (toList (stEntries st))
  , lsCollapsed = stCollapsed st
  , lsScroll = stScroll st
  , lsMinLevel = stMinLevel st
  }

logSnapshotTag :: OpTag "logSnapshot"
logSnapshotTag = OpTag

[hyperspace|
-- | Reply protocol for emitting log snapshots.
replyprotocol LogSnapshotReply =
  cast logSnapshot :: LogSnapshot

actor Log
  state LogState
  lifetime Singleton
  schedule pinned 4
  noDeps
  mailbox Unbounded

  cast append :: LogEntry
  cast setCollapsed :: Bool
  cast setMinLevel :: LogLevel
  cast scroll :: Int
  cast setScroll :: Int
  cast snapshotAsync :: () reply LogSnapshotReply
  cast setFileHandle :: Handle
  cast setSnapshotRef :: LogSnapshotRef
  call snapshot :: () -> LogSnapshot

  initial emptyLogState
  on_ append = \entry st -> do
    st' <- appendEntryWithFile entry st
    pure st'
  on_ setCollapsed = \flag st -> do
    let st' = st { stCollapsed = flag }
    publishLogSnapshot st'
    pure st'
  on_ setMinLevel = \level st -> do
    let st' = st { stMinLevel = level }
    publishLogSnapshot st'
    pure st'
  on_ scroll = \delta st -> do
    let st' = st { stScroll = max 0 (stScroll st + delta) }
    publishLogSnapshot st'
    pure st'
  on_ setScroll = \value st -> do
    let st' = st { stScroll = max 0 value }
    publishLogSnapshot st'
    pure st'
  onReply snapshotAsync = \() replyTo st -> do
    replyCast replyTo logSnapshotTag (snapshotLog st)
    pure st
  on_ setFileHandle = \h st -> pure st { stFileHandle = Just h }
  on_ setSnapshotRef = \ref st -> do
    let st' = st { stSnapshotRef = Just ref }
    publishLogSnapshot st'
    pure st'
  onPure snapshot = \() st -> (st, snapshotLog st)
|]

appendLog :: ActorHandle Log (Protocol Log) -> LogEntry -> IO ()
appendLog logHandle entry =
  cast @"append" logHandle #append entry

-- | Reset the on-disk log file at startup and return an open 'Handle' for
-- subsequent writes.  The handle uses line buffering for a good balance
-- between throughput and latency.
resetLogFile :: IO Handle
resetLogFile = do
  home <- getHomeDirectory
  let dir = home </> ".topo"
      path = dir </> "LOG.txt"
  createDirectoryIfMissing True dir
  h <- openFile path WriteMode
  hSetBuffering h LineBuffering
  pure h

ignoreLogError :: SomeException -> IO ()
ignoreLogError _ = pure ()

setLogCollapsed :: ActorHandle Log (Protocol Log) -> Bool -> IO ()
setLogCollapsed handle flag =
  cast @"setCollapsed" handle #setCollapsed flag

setLogMinLevel :: ActorHandle Log (Protocol Log) -> LogLevel -> IO ()
setLogMinLevel handle level =
  cast @"setMinLevel" handle #setMinLevel level

scrollLog :: ActorHandle Log (Protocol Log) -> Int -> IO ()
scrollLog handle delta =
  cast @"scroll" handle #scroll delta

setLogScroll :: ActorHandle Log (Protocol Log) -> Int -> IO ()
setLogScroll handle value =
  cast @"setScroll" handle #setScroll value

-- | Synchronous snapshot call.
--
-- __Test-only:__ No production callers — the 'LogSnapshotRef' IORef
-- supersedes this.  Kept exported for test coverage.
getLogSnapshot :: ActorHandle Log (Protocol Log) -> IO LogSnapshot
getLogSnapshot handle =
  call @"snapshot" handle #snapshot ()

-- | Request a log snapshot via a reply-capable cast.
requestLogSnapshot :: ActorHandle Log (Protocol Log) -> ReplyTo LogSnapshotReply -> IO ()
requestLogSnapshot logHandle replyTo =
  castReply @"snapshotAsync" logHandle replyTo #snapshotAsync ()

-- | Register the file handle for log file writes within the Log actor.
setLogFileHandle :: ActorHandle Log (Protocol Log) -> Handle -> IO ()
setLogFileHandle logHandle h =
  cast @"setFileHandle" logHandle #setFileHandle h

-- | Register a shared 'IORef' for self-publishing log snapshots.
--
-- Once registered, the Log actor writes its current 'LogSnapshot' to this
-- ref after every state change, enabling the render thread to read the
-- latest snapshot without blocking on the actor's mailbox.
setLogSnapshotRef :: ActorHandle Log (Protocol Log) -> LogSnapshotRef -> IO ()
setLogSnapshotRef logHandle ref =
  cast @"setSnapshotRef" logHandle #setSnapshotRef ref

-- | Read the latest log snapshot from the shared 'IORef'.
readLogSnapshotRef :: LogSnapshotRef -> IO LogSnapshot
readLogSnapshotRef = readIORef

-- | Create a new 'LogSnapshotRef' with an empty initial snapshot.
newLogSnapshotRef :: IO LogSnapshotRef
newLogSnapshotRef = newIORef (LogSnapshot [] False 0 LogDebug)

logLevelRank :: LogLevel -> Int
logLevelRank level =
  case level of
    LogDebug -> 0
    LogInfo -> 1
    LogWarn -> 2
    LogError -> 3

filterEntries :: LogLevel -> [LogEntry] -> [LogEntry]
filterEntries minLevel entries =
  filter (\entry -> logLevelRank (leLevel entry) >= logLevelRank minLevel) entries
