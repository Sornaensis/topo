-- | Shared atlas freshness state for latest-wins scheduling.
--
-- The render/scheduler path publishes the currently desired atlas key so
-- queued worker messages can cheaply skip obsolete builds before doing CPU
-- geometry work.  Scheduler-dispatched builds also carry the snapshot version
-- that produced them, while key-only updates preserve layer-specific terrain
-- version semantics when no newer atlas work was requested.
module Actor.AtlasFreshness
  ( AtlasFreshness(..)
  , AtlasFreshnessRef
  , newAtlasFreshnessRef
  , writeAtlasFreshness
  , writeAtlasFreshnessKey
  , readAtlasFreshnessRef
  , atlasKeyIsCurrent
  , atlasBuildIsFresh
  ) where

import Actor.AtlasCache (AtlasKey)
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)

data AtlasFreshness = AtlasFreshness
  { afKey :: !AtlasKey
  , afSnapshotVersion :: !SnapshotVersion
  } deriving (Eq, Show)

type AtlasFreshnessRef = IORef (Maybe AtlasFreshness)

newAtlasFreshnessRef :: IO AtlasFreshnessRef
newAtlasFreshnessRef = newIORef Nothing

writeAtlasFreshness :: AtlasFreshnessRef -> AtlasFreshness -> IO ()
writeAtlasFreshness ref freshness = writeIORef ref (Just freshness)

writeAtlasFreshnessKey :: AtlasFreshnessRef -> AtlasKey -> IO ()
writeAtlasFreshnessKey ref key =
  atomicModifyIORef' ref $ \latest ->
    let snapshotVersion = maybe (SnapshotVersion 0) afSnapshotVersion latest
    in (Just AtlasFreshness { afKey = key, afSnapshotVersion = snapshotVersion }, ())

readAtlasFreshnessRef :: AtlasFreshnessRef -> IO (Maybe AtlasFreshness)
readAtlasFreshnessRef = readIORef

-- | Return whether a key still matches the latest scheduler-published key.
-- A missing freshness value is treated as fresh so startup/tests cannot wedge
-- builds before the first render scheduling pass publishes a key.
atlasKeyIsCurrent :: Maybe AtlasFreshness -> AtlasKey -> Bool
atlasKeyIsCurrent Nothing _ = True
atlasKeyIsCurrent (Just freshness) key = afKey freshness == key

atlasBuildIsFresh :: Maybe AtlasFreshness -> AtlasKey -> SnapshotVersion -> Bool
atlasBuildIsFresh Nothing _ _ = True
atlasBuildIsFresh (Just freshness) key snapshotVersion =
  afKey freshness == key && afSnapshotVersion freshness == snapshotVersion
