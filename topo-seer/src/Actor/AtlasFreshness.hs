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
  , emptyAtlasFreshness
  , newAtlasFreshnessRef
  , writeAtlasFreshness
  , writeAtlasFreshnessCurrent
  , writeAtlasFreshnessBuild
  , writeAtlasFreshnessKey
  , readAtlasFreshnessRef
  , atlasKeyIsCurrent
  , atlasBuildIsFresh
  , atlasTargetBuildIsFresh
  ) where

import Actor.AtlasCache (AtlasKey)
import Actor.AtlasResult (AtlasBuildId, AtlasBuildTarget(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)

data AtlasFreshness = AtlasFreshness
  { afKey :: !AtlasKey
  , afSnapshotVersion :: !SnapshotVersion
  , afLatestBuildIds :: !(Map.Map AtlasBuildTarget AtlasBuildId)
  } deriving (Eq, Show)

type AtlasFreshnessRef = IORef (Maybe AtlasFreshness)

emptyAtlasFreshness :: AtlasKey -> SnapshotVersion -> AtlasFreshness
emptyAtlasFreshness key snapshotVersion = AtlasFreshness
  { afKey = key
  , afSnapshotVersion = snapshotVersion
  , afLatestBuildIds = Map.empty
  }

newAtlasFreshnessRef :: IO AtlasFreshnessRef
newAtlasFreshnessRef = newIORef Nothing

writeAtlasFreshness :: AtlasFreshnessRef -> AtlasFreshness -> IO ()
writeAtlasFreshness ref freshness = writeIORef ref (Just freshness)

-- | Publish the current key/snapshot while preserving per-target build ids.
writeAtlasFreshnessCurrent :: AtlasFreshnessRef -> AtlasKey -> SnapshotVersion -> IO ()
writeAtlasFreshnessCurrent ref key snapshotVersion =
  atomicModifyIORef' ref $ \latest ->
    let buildIds = maybe Map.empty afLatestBuildIds latest
        freshness = AtlasFreshness
          { afKey = key
          , afSnapshotVersion = snapshotVersion
          , afLatestBuildIds = buildIds
          }
    in (Just freshness, ())

-- | Publish the latest accepted build id for one target scale.
--
-- The atlas manager owns this authoritative latest-build id.  Workers and the
-- render result drain both consult it so obsolete same-key viewport builds are
-- rejected before texture upload; the cache only has to prevent cross-build
-- tile merging for already accepted results and tests/direct callers.
writeAtlasFreshnessBuild :: AtlasFreshnessRef -> AtlasBuildTarget -> AtlasBuildId -> IO ()
writeAtlasFreshnessBuild ref target buildId =
  atomicModifyIORef' ref $ \latest ->
    let buildIds = maybe Map.empty afLatestBuildIds latest
        freshness = AtlasFreshness
          { afKey = abtKey target
          , afSnapshotVersion = abtSnapshotVersion target
          , afLatestBuildIds = Map.insert target buildId buildIds
          }
    in (Just freshness, ())

writeAtlasFreshnessKey :: AtlasFreshnessRef -> AtlasKey -> IO ()
writeAtlasFreshnessKey ref key =
  atomicModifyIORef' ref $ \latest ->
    let snapshotVersion = maybe (SnapshotVersion 0) afSnapshotVersion latest
        buildIds = maybe Map.empty afLatestBuildIds latest
    in ( Just AtlasFreshness
          { afKey = key
          , afSnapshotVersion = snapshotVersion
          , afLatestBuildIds = buildIds
          }
       , ()
       )

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

atlasTargetBuildIsFresh :: Maybe AtlasFreshness -> AtlasBuildTarget -> AtlasBuildId -> Bool
atlasTargetBuildIsFresh Nothing _ _ = True
atlasTargetBuildIsFresh (Just freshness) target buildId =
  afKey freshness == abtKey target
    && afSnapshotVersion freshness == abtSnapshotVersion target
    && maybe True (== buildId) (Map.lookup target (afLatestBuildIds freshness))
