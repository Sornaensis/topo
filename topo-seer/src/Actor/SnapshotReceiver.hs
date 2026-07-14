-- | Ordered snapshot publication for the render loop.
--
-- Each domain keeps its own 'IORef', while 'SnapshotVersionRef' is also the
-- publication lock. Writers publish every ref belonging to one logical change
-- under that lock and commit one version only after all writes complete.
-- Each commit also captures an immutable five-domain 'RenderSnapshot'; readers
-- acquire that snapshot and its version together, so live UI/log writes cannot
-- leak into an older committed generation.
module Actor.SnapshotReceiver
  ( RenderSnapshot(..)
  , SnapshotVersion(..)
    -- * Data snapshot ref
  , DataSnapshotRef
  , newDataSnapshotRef
  , writeDataSnapshot
  , readDataSnapshot
    -- * Terrain snapshot ref
  , TerrainSnapshotRef
  , newTerrainSnapshotRef
  , writeTerrainSnapshot
  , readTerrainSnapshot
    -- * Ordered publication
  , SnapshotVersionRef
  , SnapshotUpdate
  , uiSnapshotUpdate
  , logSnapshotUpdate
  , dataSnapshotUpdate
  , terrainSnapshotUpdate
  , dataAndTerrainSnapshotUpdate
  , withUiSnapshot
  , withLogSnapshot
  , newSnapshotVersionRef
  , newRenderSnapshotVersionRef
  , readSnapshotVersion
  , publishSnapshot
  , publishSnapshotIfVersion
  , invalidatePublishedSnapshot
  , readCommittedRenderSnapshot
  , readCommittedUiAndLog
  , publishChangedUiAndLog
  , withCommittedRenderSnapshot
  ) where

import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Actor.Log (LogSnapshot(..), LogSnapshotRef, readLogSnapshotRef)
import Actor.UI (UiSnapshotRef, UiState(..), readUiSnapshotRef)
import Control.Monad (void)
import Control.Concurrent.MVar
  ( MVar
  , modifyMVarMasked
  , newMVar
  , readMVar
  , withMVar
  )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word64)

-- | Composite render snapshot consumed by the render loop.
--
-- Contains one sub-snapshot per domain: UI state, log, data, and terrain.
data RenderSnapshot = RenderSnapshot
  { rsUi      :: !UiState
  , rsLog     :: !LogSnapshot
  , rsData    :: !DataSnapshot
  , rsTerrain :: !TerrainSnapshot
  } deriving (Eq, Show)

-- | Monotonic version for the latest committed snapshot publication.
newtype SnapshotVersion = SnapshotVersion { unSnapshotVersion :: Word64 }
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Domain refs
-- ---------------------------------------------------------------------------

type DataSnapshotRef = IORef DataSnapshot

newDataSnapshotRef :: DataSnapshot -> IO DataSnapshotRef
newDataSnapshotRef = newIORef

-- | Low-level write for initialization and tests. Production publishers must
-- use 'publishSnapshot' so ordering and the version commit cannot be split.
writeDataSnapshot :: DataSnapshotRef -> DataSnapshot -> IO ()
writeDataSnapshot = writeIORef

readDataSnapshot :: DataSnapshotRef -> IO DataSnapshot
readDataSnapshot = readIORef

type TerrainSnapshotRef = IORef TerrainSnapshot

newTerrainSnapshotRef :: TerrainSnapshot -> IO TerrainSnapshotRef
newTerrainSnapshotRef = newIORef

-- | Low-level write for initialization and tests. Production publishers must
-- use 'publishSnapshot' so ordering and the version commit cannot be split.
writeTerrainSnapshot :: TerrainSnapshotRef -> TerrainSnapshot -> IO ()
writeTerrainSnapshot = writeIORef

readTerrainSnapshot :: TerrainSnapshotRef -> IO TerrainSnapshot
readTerrainSnapshot = readIORef

-- ---------------------------------------------------------------------------
-- Ordered publication
-- ---------------------------------------------------------------------------

-- | The committed version, optional render-domain registration, and the mutex
-- serializing publishers with coherent readers. Keeping these as one opaque
-- value prevents a publisher from committing a version without participating
-- in the ordering protocol.
newtype SnapshotVersionRef = SnapshotVersionRef (MVar SnapshotPublicationState)

data SnapshotPublicationState = SnapshotPublicationState
  { spsVersion :: !SnapshotVersion
  , spsDomains :: !(Maybe SnapshotDomainRefs)
  , spsCommittedRenderSnapshot :: !(Maybe RenderSnapshot)
  }

data SnapshotDomainRefs = SnapshotDomainRefs
  { sdrUi :: !UiSnapshotRef
  , sdrLog :: !LogSnapshotRef
  , sdrData :: !DataSnapshotRef
  , sdrTerrain :: !TerrainSnapshotRef
  }

-- | All domain writes belonging to one logical publication.
--
-- Constructors are intentionally hidden; use the smart constructors below.
data SnapshotUpdate = SnapshotUpdate
  { suUi :: !(Maybe UiState)
  , suLog :: !(Maybe LogSnapshot)
  , suData :: !(Maybe (DataSnapshotRef, DataSnapshot))
  , suTerrain :: !(Maybe (TerrainSnapshotRef, TerrainSnapshot))
  }

-- | Publish a mailbox-barrier UI snapshot without changing data or terrain.
uiSnapshotUpdate :: UiState -> SnapshotUpdate
uiSnapshotUpdate uiSnapshot =
  SnapshotUpdate (Just uiSnapshot) Nothing Nothing Nothing

-- | Publish a mailbox-barrier log snapshot without changing other domains.
logSnapshotUpdate :: LogSnapshot -> SnapshotUpdate
logSnapshotUpdate logSnapshot =
  SnapshotUpdate Nothing (Just logSnapshot) Nothing Nothing

dataSnapshotUpdate :: DataSnapshotRef -> DataSnapshot -> SnapshotUpdate
dataSnapshotUpdate ref snapshot =
  SnapshotUpdate Nothing Nothing (Just (ref, snapshot)) Nothing

terrainSnapshotUpdate :: TerrainSnapshotRef -> TerrainSnapshot -> SnapshotUpdate
terrainSnapshotUpdate ref snapshot =
  SnapshotUpdate Nothing Nothing Nothing (Just (ref, snapshot))

dataAndTerrainSnapshotUpdate
  :: DataSnapshotRef
  -> DataSnapshot
  -> TerrainSnapshotRef
  -> TerrainSnapshot
  -> SnapshotUpdate
dataAndTerrainSnapshotUpdate dataRef dataSnapshot terrainRef terrainSnapshot =
  SnapshotUpdate
    Nothing
    Nothing
    (Just (dataRef, dataSnapshot))
    (Just (terrainRef, terrainSnapshot))

-- | Include UI state obtained from the actor mailbox barrier in this commit.
withUiSnapshot :: UiState -> SnapshotUpdate -> SnapshotUpdate
withUiSnapshot uiSnapshot update = update { suUi = Just uiSnapshot }

-- | Include log state obtained from the actor mailbox barrier in this commit.
withLogSnapshot :: LogSnapshot -> SnapshotUpdate -> SnapshotUpdate
withLogSnapshot logSnapshot update = update { suLog = Just logSnapshot }

-- | Create a detached publication counter for tests and non-render consumers.
newSnapshotVersionRef :: IO SnapshotVersionRef
newSnapshotVersionRef = SnapshotVersionRef <$> newMVar SnapshotPublicationState
  { spsVersion = SnapshotVersion 0
  , spsDomains = Nothing
  , spsCommittedRenderSnapshot = Nothing
  }

-- | Create the production publication coordinator and capture generation zero.
newRenderSnapshotVersionRef
  :: UiSnapshotRef
  -> LogSnapshotRef
  -> DataSnapshotRef
  -> TerrainSnapshotRef
  -> IO SnapshotVersionRef
newRenderSnapshotVersionRef uiRef logRef dataRef terrainRef = do
  let domains = SnapshotDomainRefs uiRef logRef dataRef terrainRef
  initialSnapshot <- captureRenderSnapshot domains
  SnapshotVersionRef <$> newMVar SnapshotPublicationState
    { spsVersion = SnapshotVersion 0
    , spsDomains = Just domains
    , spsCommittedRenderSnapshot = Just initialSnapshot
    }

-- | Read the latest committed version. This waits for an in-flight publication.
readSnapshotVersion :: SnapshotVersionRef -> IO SnapshotVersion
readSnapshotVersion (SnapshotVersionRef publicationRef) =
  spsVersion <$> readMVar publicationRef

-- | Publish one logical snapshot update and return its committed version.
--
-- Concurrent publishers are serialized. Every requested ref write and the
-- complete committed-domain update happen before exactly one monotonic commit.
publishSnapshot :: SnapshotVersionRef -> SnapshotUpdate -> IO SnapshotVersion
publishSnapshot (SnapshotVersionRef publicationRef) update =
  modifyMVarMasked publicationRef $ \state -> do
    (state', committed) <- commitSnapshotUpdate state update
    pure (state', committed)

-- | Publish only if no other writer has advanced beyond the observed version.
-- A failed conditional commit performs no ref writes and lets callers recapture
-- actor state before retrying, preventing stale compensating publications.
publishSnapshotIfVersion
  :: SnapshotVersionRef
  -> SnapshotVersion
  -> SnapshotUpdate
  -> IO (Maybe SnapshotVersion)
publishSnapshotIfVersion (SnapshotVersionRef publicationRef) expected update =
  modifyMVarMasked publicationRef $ \state ->
    if spsVersion state /= expected
      then pure (state, Nothing)
      else do
        (state', committed) <- commitSnapshotUpdate state update
        pure (state', Just committed)

commitSnapshotUpdate
  :: SnapshotPublicationState
  -> SnapshotUpdate
  -> IO (SnapshotPublicationState, SnapshotVersion)
commitSnapshotUpdate state update = do
  mapM_ (uncurry writeIORef) (suData update)
  mapM_ (uncurry writeIORef) (suTerrain update)
  let committedSnapshot = applySnapshotUpdate update <$> spsCommittedRenderSnapshot state
      SnapshotVersion version = spsVersion state
      committed = SnapshotVersion (version + 1)
      state' = state
        { spsVersion = committed
        , spsCommittedRenderSnapshot = committedSnapshot
        }
  pure (state', committed)

-- | Commit an invalidation after UI/log actors have already published state.
--
-- Actor setters are asynchronous casts. Callers /must/ first perform a
-- synchronous request to every affected actor (for example 'getUiSnapshot') as
-- a mailbox barrier. The returned request proves the preceding casts and their
-- snapshot-ref writes happen-before this version commit.
invalidatePublishedSnapshot :: SnapshotVersionRef -> IO SnapshotVersion
invalidatePublishedSnapshot (SnapshotVersionRef publicationRef) =
  modifyMVarMasked publicationRef $ \state -> do
    committedSnapshot <- traverse captureRenderSnapshot (spsDomains state)
    let SnapshotVersion version = spsVersion state
        committed = SnapshotVersion (version + 1)
        state' = state
          { spsVersion = committed
          , spsCommittedRenderSnapshot = committedSnapshot
          }
    pure (state', committed)

-- | Read the immutable five-domain snapshot captured by the latest commit.
-- UI/log actors may continue publishing their live refs concurrently, but
-- readers only observe those values after an ordered publication copies the
-- complete tuple into this committed slot.
readCommittedRenderSnapshot :: SnapshotVersionRef -> IO (SnapshotVersion, RenderSnapshot)
readCommittedRenderSnapshot publicationRef =
  withCommittedRenderSnapshot publicationRef (\version snapshot -> pure (version, snapshot))

-- | Read the committed UI/log domains when render refs are registered.
-- Detached test coordinators return 'Nothing' rather than throwing.
readCommittedUiAndLog
  :: SnapshotVersionRef
  -> IO (Maybe (SnapshotVersion, UiState, LogSnapshot))
readCommittedUiAndLog (SnapshotVersionRef publicationRef) =
  withMVar publicationRef $ \state -> pure $ do
    snapshot <- spsCommittedRenderSnapshot state
    pure (spsVersion state, rsUi snapshot, rsLog snapshot)

-- | Publish UI and log domains changed since the supplied actor snapshots.
--
-- The committed tuple is observed before each pair of mailbox-barrier captures.
-- A concurrent commit therefore makes the conditional publication fail and
-- forces fresh captures, rather than allowing an older capture to overwrite a
-- newer committed domain. UI and log are compared independently so an explicit
-- publication of one domain cannot hide a still-unpublished change in the
-- other.
publishChangedUiAndLog
  :: SnapshotVersionRef
  -> UiState
  -> LogSnapshot
  -> IO UiState
  -> IO LogSnapshot
  -> IO ()
publishChangedUiAndLog versionRef uiBefore logBefore captureUi captureLog = retry
  where
    retry = do
      committed <- readCommittedUiAndLog versionRef
      uiAfter <- captureUi
      logAfter <- captureLog
      case committed of
        Nothing ->
          if uiAfter /= uiBefore || logAfter /= logBefore
            then void $ publishSnapshot versionRef
              (withLogSnapshot logAfter (uiSnapshotUpdate uiAfter))
            else pure ()
        Just (expected, committedUi, committedLog) -> do
          let uiMissing = uiAfter /= uiBefore && uiAfter /= committedUi
              logMissing = logAfter /= logBefore && logAfter /= committedLog
          if not (uiMissing || logMissing)
            then pure ()
            else do
              let update = case (uiMissing, logMissing) of
                    (True, True) -> withLogSnapshot logAfter (uiSnapshotUpdate uiAfter)
                    (True, False) -> uiSnapshotUpdate uiAfter
                    (False, True) -> logSnapshotUpdate logAfter
                    (False, False) -> uiSnapshotUpdate uiAfter
              published <- publishSnapshotIfVersion versionRef expected update
              case published of
                Just _ -> pure ()
                Nothing -> retry

-- | Run a short coherent read while excluding concurrent publishers.
-- This callback form is primarily useful when a consumer must derive a value
-- from one committed generation without copying or re-reading live refs.
withCommittedRenderSnapshot
  :: SnapshotVersionRef
  -> (SnapshotVersion -> RenderSnapshot -> IO a)
  -> IO a
withCommittedRenderSnapshot (SnapshotVersionRef publicationRef) consume =
  withMVar publicationRef $ \state ->
    case spsCommittedRenderSnapshot state of
      Just snapshot -> consume (spsVersion state) snapshot
      Nothing -> fail "withCommittedRenderSnapshot: render domains are not registered"

applySnapshotUpdate :: SnapshotUpdate -> RenderSnapshot -> RenderSnapshot
applySnapshotUpdate update snapshot = snapshot
  { rsUi = maybe (rsUi snapshot) id (suUi update)
  , rsLog = maybe (rsLog snapshot) id (suLog update)
  , rsData = maybe (rsData snapshot) snd (suData update)
  , rsTerrain = maybe (rsTerrain snapshot) snd (suTerrain update)
  }

captureRenderSnapshot :: SnapshotDomainRefs -> IO RenderSnapshot
captureRenderSnapshot domains = do
  uiSnapshot <- readUiSnapshotRef (sdrUi domains)
  logSnapshot <- readLogSnapshotRef (sdrLog domains)
  dataSnapshot <- readDataSnapshot (sdrData domains)
  terrainSnapshot <- readTerrainSnapshot (sdrTerrain domains)
  pure RenderSnapshot
    { rsUi = uiSnapshot
    , rsLog = logSnapshot
    , rsData = dataSnapshot
    , rsTerrain = terrainSnapshot
    }
