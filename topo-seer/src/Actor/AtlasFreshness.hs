-- | Shared atlas freshness state for latest-wins scheduling.
--
-- The render/scheduler path publishes the currently desired atlas keys so
-- queued worker messages can cheaply skip obsolete builds before doing CPU
-- geometry work.  Layered rendering keeps base and overlay keys current at the
-- same time by storing lightweight sentinel targets in the build-id map while
-- preserving the legacy single representative 'afKey'.
module Actor.AtlasFreshness
  ( AtlasFreshness(..)
  , AtlasFreshnessRef
  , emptyAtlasFreshness
  , emptyAtlasFreshnessForKeys
  , newAtlasFreshnessRef
  , writeAtlasFreshness
  , writeAtlasFreshnessCurrent
  , writeAtlasFreshnessCurrentKeys
  , writeAtlasFreshnessBuild
  , writeAtlasFreshnessKey
  , readAtlasFreshnessRef
  , atlasKeyIsCurrent
  , atlasBuildIsFresh
  , atlasTargetBuildIsFresh
  ) where

import Actor.AtlasCache (AtlasKey)
import Actor.AtlasResult (AtlasBuildId(..), AtlasBuildTarget(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)

data AtlasFreshness = AtlasFreshness
  { afKey :: !AtlasKey
    -- ^ Representative current key retained for legacy single-key callers and
    -- diagnostics. Layer-aware freshness also reads current-key sentinels from
    -- 'afLatestBuildIds'.
  , afSnapshotVersion :: !SnapshotVersion
  , afLatestBuildIds :: !(Map.Map AtlasBuildTarget AtlasBuildId)
  } deriving (Eq, Show)

type AtlasFreshnessRef = IORef (Maybe AtlasFreshness)

emptyAtlasFreshness :: AtlasKey -> SnapshotVersion -> AtlasFreshness
emptyAtlasFreshness key snapshotVersion = emptyAtlasFreshnessForKeys [key] snapshotVersion

emptyAtlasFreshnessForKeys :: [AtlasKey] -> SnapshotVersion -> AtlasFreshness
emptyAtlasFreshnessForKeys keys snapshotVersion = AtlasFreshness
  { afKey = representativeKey keys
  , afSnapshotVersion = snapshotVersion
  , afLatestBuildIds = currentKeySentinels keys snapshotVersion
  }

representativeKey :: [AtlasKey] -> AtlasKey
representativeKey (key:_) = key
representativeKey [] = error "emptyAtlasFreshnessForKeys: expected at least one key"

currentKeySentinelTarget :: AtlasKey -> SnapshotVersion -> AtlasBuildTarget
currentKeySentinelTarget key snapshotVersion = AtlasBuildTarget
  { abtKey = key
  , abtSnapshotVersion = snapshotVersion
  , abtHexRadius = -1
  , abtAtlasScale = -1
  }

isCurrentKeySentinel :: AtlasBuildTarget -> Bool
isCurrentKeySentinel target = abtHexRadius target == -1 && abtAtlasScale target == -1

currentKeySentinels :: [AtlasKey] -> SnapshotVersion -> Map.Map AtlasBuildTarget AtlasBuildId
currentKeySentinels keys snapshotVersion = Map.fromList
  [ (currentKeySentinelTarget key snapshotVersion, AtlasBuildId 0)
  | key <- keys
  ]

targetSlot :: AtlasBuildTarget -> (AtlasKey, Int, Int)
targetSlot target = (abtKey target, abtHexRadius target, abtAtlasScale target)

latestTargetsForKeys :: Set.Set AtlasKey -> Map.Map AtlasBuildTarget AtlasBuildId -> Map.Map AtlasBuildTarget AtlasBuildId
latestTargetsForKeys keySet buildIds = Map.fromList (map snd (Map.elems latestBySlot))
  where
    latestBySlot = Map.fromListWith newer
      [ (targetSlot target, (buildId, (target, buildId)))
      | (target, buildId) <- Map.toList buildIds
      , not (isCurrentKeySentinel target)
      , abtKey target `Set.member` keySet
      ]
    newer left@(leftId, _) right@(rightId, _)
      | leftId >= rightId = left
      | otherwise = right

insertLatestTargetBuildId :: AtlasBuildTarget -> AtlasBuildId -> Map.Map AtlasBuildTarget AtlasBuildId -> Map.Map AtlasBuildTarget AtlasBuildId
insertLatestTargetBuildId target buildId buildIds =
  Map.insert target buildId $
    Map.filterWithKey
      (\existingTarget _ -> isCurrentKeySentinel existingTarget || targetSlot existingTarget /= targetSlot target)
      buildIds

latestBuildIdForTarget :: AtlasBuildTarget -> Map.Map AtlasBuildTarget AtlasBuildId -> Maybe AtlasBuildId
latestBuildIdForTarget target buildIds = foldr newest Nothing
  [ buildId
  | (storedTarget, buildId) <- Map.toList buildIds
  , not (isCurrentKeySentinel storedTarget)
  , targetSlot storedTarget == targetSlot target
  ]
  where
    newest buildId Nothing = Just buildId
    newest buildId (Just latestId) = Just (max buildId latestId)

newAtlasFreshnessRef :: IO AtlasFreshnessRef
newAtlasFreshnessRef = newIORef Nothing

writeAtlasFreshness :: AtlasFreshnessRef -> AtlasFreshness -> IO ()
writeAtlasFreshness ref freshness = writeIORef ref (Just freshness)

-- | Publish one current key/snapshot while preserving per-target build ids.
writeAtlasFreshnessCurrent :: AtlasFreshnessRef -> AtlasKey -> SnapshotVersion -> IO ()
writeAtlasFreshnessCurrent ref key snapshotVersion =
  writeAtlasFreshnessCurrentKeys ref [key] snapshotVersion

-- | Publish all current layer keys for a snapshot while preserving per-target
-- build ids for those keys. The first key is kept as 'afKey' for legacy
-- diagnostics; additional current keys are represented by sentinel targets in
-- 'afLatestBuildIds'.
writeAtlasFreshnessCurrentKeys :: AtlasFreshnessRef -> [AtlasKey] -> SnapshotVersion -> IO ()
writeAtlasFreshnessCurrentKeys _ [] _ = pure ()
writeAtlasFreshnessCurrentKeys ref keys snapshotVersion =
  atomicModifyIORef' ref $ \latest ->
    case latest of
      Just freshness | snapshotVersion < afSnapshotVersion freshness -> (latest, ())
      _ ->
        let keySet = Set.fromList keys
            buildIds = maybe Map.empty afLatestBuildIds latest
            preserved = latestTargetsForKeys keySet buildIds
            freshness = AtlasFreshness
              { afKey = representativeKey keys
              , afSnapshotVersion = snapshotVersion
              , afLatestBuildIds = currentKeySentinels keys snapshotVersion <> preserved
              }
        in (Just freshness, ())

-- | Publish the latest accepted build id for one target scale.
--
-- The atlas manager owns this authoritative latest-build id. Workers and the
-- render result drain both consult it so obsolete same-key viewport builds are
-- rejected before texture upload; the cache only has to prevent cross-build
-- tile merging for already accepted results and tests/direct callers.
writeAtlasFreshnessBuild :: AtlasFreshnessRef -> AtlasBuildTarget -> AtlasBuildId -> IO ()
writeAtlasFreshnessBuild ref target buildId =
  atomicModifyIORef' ref $ \latest ->
    let buildIds = maybe Map.empty afLatestBuildIds latest
        freshness = AtlasFreshness
          { afKey = maybe (abtKey target) afKey latest
          , afSnapshotVersion = max (abtSnapshotVersion target) (maybe (abtSnapshotVersion target) afSnapshotVersion latest)
          , afLatestBuildIds = insertLatestTargetBuildId target buildId buildIds
          }
    in (Just freshness, ())

writeAtlasFreshnessKey :: AtlasFreshnessRef -> AtlasKey -> IO ()
writeAtlasFreshnessKey ref key =
  atomicModifyIORef' ref $ \latest ->
    let snapshotVersion = maybe (SnapshotVersion 0) afSnapshotVersion latest
        buildIds = maybe Map.empty afLatestBuildIds latest
        preserved = latestTargetsForKeys (Set.singleton key) buildIds
    in ( Just AtlasFreshness
          { afKey = key
          , afSnapshotVersion = snapshotVersion
          , afLatestBuildIds = currentKeySentinels [key] snapshotVersion <> preserved
          }
       , ()
       )

readAtlasFreshnessRef :: AtlasFreshnessRef -> IO (Maybe AtlasFreshness)
readAtlasFreshnessRef = readIORef

freshnessCurrentKeys :: AtlasFreshness -> Set.Set AtlasKey
freshnessCurrentKeys freshness =
  Set.insert (afKey freshness) $ Set.fromList
    [ abtKey target
    | target <- Map.keys (afLatestBuildIds freshness)
    , isCurrentKeySentinel target
    ]

-- | Return whether a key still matches the latest scheduler-published keys.
-- A missing freshness value is treated as fresh so startup/tests cannot wedge
-- builds before the first render scheduling pass publishes keys.
atlasKeyIsCurrent :: Maybe AtlasFreshness -> AtlasKey -> Bool
atlasKeyIsCurrent Nothing _ = True
atlasKeyIsCurrent (Just freshness) key = key `Set.member` freshnessCurrentKeys freshness

atlasBuildIsFresh :: Maybe AtlasFreshness -> AtlasKey -> SnapshotVersion -> Bool
atlasBuildIsFresh Nothing _ _ = True
atlasBuildIsFresh (Just freshness) key snapshotVersion =
  key `Set.member` freshnessCurrentKeys freshness
    && snapshotVersion <= afSnapshotVersion freshness

atlasTargetBuildIsFresh :: Maybe AtlasFreshness -> AtlasBuildTarget -> AtlasBuildId -> Bool
atlasTargetBuildIsFresh Nothing _ _ = True
atlasTargetBuildIsFresh (Just freshness) target buildId =
  abtKey target `Set.member` freshnessCurrentKeys freshness
    && abtSnapshotVersion target <= afSnapshotVersion freshness
    && maybe True (== buildId) (latestBuildIdForTarget target (afLatestBuildIds freshness))
