{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Derived terrain parameter layers.
--
-- Computes per-tile 'DirectionalSlope' (elevation difference to each of the
-- six hex neighbours), curvature, relief, ruggedness, terrain form
-- classification, rock\/soil types, roughness, soil depth, and fertility from
-- elevation and other base terrain fields.
--
-- Cross-chunk neighbor lookups are used so that tile stencils produce correct
-- values at chunk boundaries.
--
-- __Hardness note:__ Tectonics-computed @tcHardness@ is preserved (not
-- recomputed).  Soil depth is derived from the Tectonics value.
module Topo.Parameters
  ( ParameterConfig(..)
  , defaultParameterConfig
  , TerrainFormConfig(..)
  , defaultTerrainFormConfig
  , computeReliefIndex
  , applyParameterLayersStage
  -- * Chunk derivation
  , deriveChunk
  -- * Stencil functions (exported for testing)
  , mkElevLookup
  , slopeAt
  , curvatureAt
  , reliefAt
  , ruggednessAt
  , isLocalMinimum
  , classifyTerrainForm
  ) where

import Control.Monad.Reader (asks)
import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Control.Monad.ST (runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word16)
import Topo.Math (clamp01)
import Topo.Parameters.TerrainForm
  ( TerrainFormConfig(..)
  , classifyTerrainForm
  , computeReliefIndex
  , defaultTerrainFormConfig
  )
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Plugin (logInfo, modifyWorldP, peSeed)
import qualified Topo.TerrainForm.Metrics as TerrainMetrics
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

---------------------------------------------------------------------------
-- Configuration
---------------------------------------------------------------------------

-- | Parameter derivation configuration.
data ParameterConfig = ParameterConfig
  { pcDetailScale              :: !Float
  -- ^ Scale factor for noise details (unused after hardness removal, kept
  --   for future procedural layers).
  , pcRoughnessScale           :: !Float
  , pcRockElevationThreshold   :: !Float
  , pcRockHardnessThreshold    :: !Float
  , pcRockHardnessSecondary    :: !Float
  } deriving (Eq, Show, Generic)

instance ToJSON ParameterConfig where
  toJSON = genericToJSON (configOptions "pc")

instance FromJSON ParameterConfig where
  parseJSON v = genericParseJSON (configOptions "pc")
                  (mergeDefaults (toJSON defaultParameterConfig) v)

-- | Default parameter configuration.
defaultParameterConfig :: ParameterConfig
defaultParameterConfig = ParameterConfig
  { pcDetailScale             = 1
  , pcRoughnessScale          = 0.75
  , pcRockElevationThreshold  = 0.6
  , pcRockHardnessThreshold   = 0.6
  , pcRockHardnessSecondary   = 0.45
  }

---------------------------------------------------------------------------
-- Pipeline stage
---------------------------------------------------------------------------

-- | Compute derived parameter layers from terrain fields.
--
-- The full @IntMap TerrainChunk@ is threaded into each chunk's derivation
-- so that stencil functions can read neighbor elevations across chunk
-- boundaries.  The water level is needed for elevation-above-sea-level
-- used by plateau\/mesa\/foothill classification.
applyParameterLayersStage
  :: ParameterConfig -> TerrainFormConfig -> Float -> PipelineStage
applyParameterLayersStage cfg formCfg waterLevel =
    PipelineStage StageParameters "applyParameterLayers" "applyParameterLayers" Nothing [] Nothing $ do
  logInfo "applyParameterLayers: deriving fields"
  _seed <- asks peSeed
  modifyWorldP $ \world ->
    let config = twConfig world
        chunks = twTerrain world
        terrain' = IntMap.mapWithKey
          (deriveChunk config chunks cfg formCfg waterLevel) chunks
    in world { twTerrain = terrain' }

deriveChunk
  :: WorldConfig
  -> IntMap TerrainChunk
  -> ParameterConfig
  -> TerrainFormConfig
  -> Float            -- ^ water level (for elevation ASL)
  -> Int
  -> TerrainChunk
  -> TerrainChunk
deriveChunk config chunks cfg formCfg waterLevel key chunk =
  let origin@(TileCoord _ox _oy) =
        chunkOriginTile config (chunkCoordFromId (ChunkId key))
      size    = wcChunkSize config
      elev    = tcElevation chunk
      n       = U.length elev
      padded  = size + 6

      -- Phase 7.2: Build padded elevation buffer once per chunk.
      -- Interior tiles use the fast local-vector path; only the 3-tile
      -- boundary ring falls through to IntMap lookups.  The wider border
      -- supports ring-2 and ring-3 relief stencils.
      !paddedBuf = mkPaddedElevation chunks config origin elev

      -- Phase 7.1: Fused single-pass stencil derivation.
      -- Reads center + 8 neighbors = 9 array reads per tile (was ~36
      -- closure calls across 5 separate U.generate passes).
      --
      -- Substrate/micro-relief vectors are read from the chunk
      -- for the enriched terrain form classification.
      !hardnessVec = tcHardness chunk
      !microReliefVec = tcMicroRelief chunk
      !(dirSlope, curvature, relief, relief2, relief3, rugged, terrForm) = runST $ do
        dsV       <- UM.new n
        curvV     <- UM.new n
        reliefV   <- UM.new n
        relief2V  <- UM.new n
        relief3V  <- UM.new n
        ruggedV   <- UM.new n
        formV     <- UM.new n
        let {-# INLINE p #-}
            p !row !col = U.unsafeIndex paddedBuf (row * padded + col)
            go !i
              | i >= n    = pure ()
              | otherwise =
                  let !lx     = i `mod` size
                      !ly     = i `div` size
                      -- Padded coords: center at (ly+3, lx+3)
                      !cx     = lx + 3
                      !cy     = ly + 3
                      !metrics = TerrainMetrics.terrainNeighborhoodAt (\x y -> p y x) cx cy
                      !e0      = TerrainMetrics.tnElevation metrics
                      !ds      = TerrainMetrics.tnDirectionalSlope metrics
                      !c       = TerrainMetrics.tnCurvature metrics
                      !r       = TerrainMetrics.tnRelief metrics
                      !r2      = TerrainMetrics.tnRelief2Ring metrics
                      !r3      = TerrainMetrics.tnRelief3Ring metrics
                      !tri     = TerrainMetrics.tnRuggedness metrics
                      !localMin = TerrainMetrics.tnIsLocalMinimum metrics
                      -- Substrate and elevation above sea level for
                      -- enriched terrain form classification.
                      !hard    = if i < U.length hardnessVec
                                 then hardnessVec U.! i else 0.5
                      !microRelief = if i < U.length microReliefVec
                                 then microReliefVec U.! i else 0.5
                      !elevASL = e0 - waterLevel
                      !tf     = classifyTerrainForm formCfg ds r r2 r3 c
                                 localMin hard microRelief elevASL
                  in do
                    UM.unsafeWrite dsV      i ds
                    UM.unsafeWrite curvV    i c
                    UM.unsafeWrite reliefV  i r
                    UM.unsafeWrite relief2V i r2
                    UM.unsafeWrite relief3V i r3
                    UM.unsafeWrite ruggedV  i tri
                    UM.unsafeWrite formV    i tf
                    go (i + 1)
        go 0
        (,,,,,,) <$> U.unsafeFreeze dsV
                 <*> U.unsafeFreeze curvV
                 <*> U.unsafeFreeze reliefV
                 <*> U.unsafeFreeze relief2V
                 <*> U.unsafeFreeze relief3V
                 <*> U.unsafeFreeze ruggedV
                 <*> U.unsafeFreeze formV

      -- Non-stencil derived fields.
      hardness  = tcHardness chunk
      rockType  = U.generate n (rockTypeAt cfg elev hardness)
      roughness = U.map (clamp01 . (* pcRoughnessScale cfg) . abs) curvature

  in chunk
      { tcDirSlope    = dirSlope
      , tcCurvature   = curvature
      , tcRockType    = rockType
      , tcRoughness   = roughness
      , tcRelief      = relief
      , tcRelief2Ring = relief2
      , tcRelief3Ring = relief3
      , tcRuggedness  = rugged
      , tcTerrainForm = terrForm
      }

---------------------------------------------------------------------------
-- Padded elevation buffer
---------------------------------------------------------------------------

-- | Build a padded @(size+6) × (size+6)@ elevation buffer that includes
-- a 3-tile boundary ring from neighboring chunks.  Interior tiles use the
-- fast local-vector path; only the boundary ring falls through to @IntMap@
-- lookups.  The wider 3-tile border supports ring-2 and ring-3 relief
-- stencils without additional lookups.
{-# INLINE mkPaddedElevation #-}
mkPaddedElevation
  :: IntMap TerrainChunk
  -> WorldConfig
  -> TileCoord          -- ^ chunk origin in global tile space
  -> U.Vector Float     -- ^ local elevation vector
  -> U.Vector Float     -- ^ padded buffer, row-major, origin at (-3,-3)
mkPaddedElevation chunks config origin@(TileCoord ox oy) localElev =
  let size   = wcChunkSize config
      padded = size + 6
      elevAt = mkElevLookup chunks config origin localElev
  in U.generate (padded * padded) $ \i ->
       let !px = i `mod` padded
           !py = i `div` padded
       in elevAt (ox + px - 3) (oy + py - 3)

---------------------------------------------------------------------------
-- Elevation lookup
---------------------------------------------------------------------------

-- | Create an elevation lookup function for a given chunk.
--
-- For tiles within the chunk the local elevation vector is used (fast
-- path, no map lookup).  For tiles outside the chunk the containing
-- chunk is looked up from the @IntMap@.  Returns @0@ (notional sea level)
-- if the neighbor chunk does not exist.
{-# INLINE mkElevLookup #-}
mkElevLookup
  :: IntMap TerrainChunk
  -> WorldConfig
  -> TileCoord          -- ^ chunk origin in global tile space
  -> U.Vector Float     -- ^ local elevation vector
  -> (Int -> Int -> Float)
mkElevLookup chunks config (TileCoord ox oy) localElev = go
  where
    size = wcChunkSize config
    go gx gy =
      let !lx = gx - ox
          !ly = gy - oy
      in if lx >= 0 && lx < size && ly >= 0 && ly < size
           then localElev U.! (ly * size + lx)
           else globalLookup gx gy
    globalLookup gx gy =
      let (cc, TileCoord lx ly) =
            chunkCoordFromTile config (TileCoord gx gy)
          ChunkId cid = chunkIdFromCoord cc
      in case IntMap.lookup cid chunks of
           Nothing    -> 0   -- sea-level fallback for world edge
           Just chunk -> tcElevation chunk U.! (ly * size + lx)

---------------------------------------------------------------------------
-- Stencil functions
---------------------------------------------------------------------------

-- | Compute directional slope to all 6 hex neighbours.
slopeAt :: (Int -> Int -> Float) -> Int -> Int -> DirectionalSlope
slopeAt = TerrainMetrics.slopeAt

-- | Laplacian curvature from 6 hex neighbours.
curvatureAt :: (Int -> Int -> Float) -> Int -> Int -> Float
curvatureAt = TerrainMetrics.curvatureAt

-- | Local elevation relief: range over 6 hex neighbours plus center.
reliefAt :: (Int -> Int -> Float) -> Int -> Int -> Float
reliefAt = TerrainMetrics.reliefAt

-- | Terrain Ruggedness Index: mean absolute elevation difference to
--   6 hex neighbours.
ruggednessAt :: (Int -> Int -> Float) -> Int -> Int -> Float
ruggednessAt = TerrainMetrics.ruggednessAt

-- | Whether a tile is a local elevation minimum (all 6 hex neighbours ≥ center).
isLocalMinimum :: (Int -> Int -> Float) -> Int -> Int -> Bool
isLocalMinimum = TerrainMetrics.isLocalMinimum

---------------------------------------------------------------------------
-- Derived-field helpers
---------------------------------------------------------------------------

-- | Convert a tile index within a chunk to global tile coordinates.
{-# INLINE globalCoords #-}
globalCoords :: Int -> Int -> Int -> Int -> (Int, Int)
globalCoords ox oy size i = (ox + i `mod` size, oy + i `div` size)

{-# INLINE rockTypeAt #-}
rockTypeAt :: ParameterConfig -> U.Vector Float -> U.Vector Float -> Int -> Word16
rockTypeAt cfg elev hardness i =
  let e0 = elev U.! i
      h0 = hardness U.! i
  in if e0 > pcRockElevationThreshold cfg && h0 > pcRockHardnessThreshold cfg
       then 2
       else if h0 > pcRockHardnessSecondary cfg
         then 1
         else 0

