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
  , applyParameterLayersStage
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
import Data.List (foldl')
import Data.Word (Word16)
import Topo.Hex (HexDirection(..), hexOpposite, allHexDirections, dsSlopeIn)
import Topo.Math (clamp01)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (logInfo, modifyWorldP, peSeed)
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

-- | Terrain form classification thresholds.
--
-- The existing thresholds ('tfcCliffSlope' through 'tfcRollingSlope') control
-- the 7 original terrain forms.  The extended thresholds control the 8 forms
-- added in Phase 9 (ridge, escarpment, plateau, badlands, pass, canyon, mesa,
-- foothill).  All values are in normalised slope/relief/elevation units.
data TerrainFormConfig = TerrainFormConfig
  { tfcCliffSlope      :: !Float
  -- ^ Slope above which terrain is classified as 'FormCliff'.
  , tfcMountainSlope   :: !Float
  -- ^ Slope threshold for 'FormMountainous'.
  , tfcMountainRelief  :: !Float
  -- ^ Relief threshold for 'FormMountainous'.
  , tfcValleyCurvature :: !Float
  -- ^ Curvature magnitude for 'FormValley' (positive value; compared as
  --   curvature < -this).
  , tfcHillSlope       :: !Float
  -- ^ Slope threshold for 'FormHilly'.
  , tfcHillRelief      :: !Float
  -- ^ Relief threshold for 'FormHilly'.
  , tfcRollingSlope    :: !Float
  -- ^ Slope threshold for 'FormRolling'.

  -- Ridge thresholds
  , tfcRidgeMinSlope     :: !Float
  -- ^ Min slope on the steep axis for 'FormRidge' (default 0.12).
  , tfcRidgeMaxAxisSlope :: !Float
  -- ^ Max slope along the ridge axis (default 0.06).
  , tfcRidgeMinAsymmetry :: !Float
  -- ^ Min asymmetry between steep and axial slopes (default 0.08).

  -- Escarpment thresholds
  , tfcEscarpmentMinSlope    :: !Float
  -- ^ Min slope on the steep face for 'FormEscarpment' (default 0.15).
  , tfcEscarpmentMaxOppSlope :: !Float
  -- ^ Max slope on the opposite (gentle) face (default 0.04).

  -- Plateau thresholds
  , tfcPlateauMaxSlope   :: !Float
  -- ^ Max slope anywhere for 'FormPlateau' (default 0.03).
  , tfcPlateauMinElevASL :: !Float
  -- ^ Min elevation above sea level for 'FormPlateau' (default 0.08).

  -- Badlands thresholds
  , tfcBadlandsMinMaxSlope  :: !Float
  -- ^ Min 'dsMaxSlope' for 'FormBadlands' (default 0.15).
  , tfcBadlandsMaxHardness  :: !Float
  -- ^ Max substrate hardness — badlands form in soft rock (default 0.35).
  , tfcBadlandsMinAsymmetry :: !Float
  -- ^ Min slope asymmetry for 'FormBadlands' (default 0.06).

  -- Pass thresholds
  , tfcPassMaxAxisSlope  :: !Float
  -- ^ Max slope along the pass axis (default 0.04).
  , tfcPassMinCrossSlope :: !Float
  -- ^ Min slope perpendicular to the axis (default 0.10).

  -- Canyon thresholds
  , tfcCanyonMinRelief    :: !Float
  -- ^ Min relief for 'FormCanyon' (default 0.20).
  , tfcCanyonMinWallSlope :: !Float
  -- ^ Min slope on steep walls (default 0.18).
  , tfcCanyonMinHardness  :: !Float
  -- ^ Min hardness — canyons carve through hard rock (default 0.40).

  -- Mesa thresholds
  , tfcMesaMaxTopSlope   :: !Float
  -- ^ Max slope on the mesa top (default 0.03).
  , tfcMesaMinEdgeRelief :: !Float
  -- ^ Min relief at mesa edges (default 0.12).
  , tfcMesaMinHardness   :: !Float
  -- ^ Min hardness — cap rock (default 0.45).
  , tfcMesaMinElevASL    :: !Float
  -- ^ Min elevation above sea level (default 0.05).

  -- Foothill thresholds
  , tfcFoothillMinSlope  :: !Float
  -- ^ Min 'dsAvgSlope' for 'FormFoothill' (default 0.03).
  , tfcFoothillMaxSlope  :: !Float
  -- ^ Max 'dsAvgSlope' for 'FormFoothill' (default 0.10).
  , tfcFoothillMinElevASL :: !Float
  -- ^ Min elevation above sea level (default 0.02).
  , tfcFoothillMaxElevASL :: !Float
  -- ^ Max elevation — above this is mountain, not foothill (default 0.12).
  } deriving (Eq, Show, Generic)

instance ToJSON TerrainFormConfig where
  toJSON = genericToJSON (configOptions "tfc")

instance FromJSON TerrainFormConfig where
  parseJSON v = genericParseJSON (configOptions "tfc")
                  (mergeDefaults (toJSON defaultTerrainFormConfig) v)

-- | Default terrain form thresholds.
defaultTerrainFormConfig :: TerrainFormConfig
defaultTerrainFormConfig = TerrainFormConfig
  { tfcCliffSlope      = 0.40
  , tfcMountainSlope   = 0.20
  , tfcMountainRelief  = 0.25
  , tfcValleyCurvature = 0.15
  , tfcHillSlope       = 0.08
  , tfcHillRelief      = 0.10
  , tfcRollingSlope    = 0.02
  -- Ridge
  , tfcRidgeMinSlope     = 0.12
  , tfcRidgeMaxAxisSlope = 0.06
  , tfcRidgeMinAsymmetry = 0.08
  -- Escarpment
  , tfcEscarpmentMinSlope    = 0.15
  , tfcEscarpmentMaxOppSlope = 0.04
  -- Plateau
  , tfcPlateauMaxSlope   = 0.03
  , tfcPlateauMinElevASL = 0.08
  -- Badlands
  , tfcBadlandsMinMaxSlope  = 0.15
  , tfcBadlandsMaxHardness  = 0.35
  , tfcBadlandsMinAsymmetry = 0.06
  -- Pass
  , tfcPassMaxAxisSlope  = 0.04
  , tfcPassMinCrossSlope = 0.10
  -- Canyon
  , tfcCanyonMinRelief    = 0.20
  , tfcCanyonMinWallSlope = 0.18
  , tfcCanyonMinHardness  = 0.40
  -- Mesa
  , tfcMesaMaxTopSlope   = 0.03
  , tfcMesaMinEdgeRelief = 0.12
  , tfcMesaMinHardness   = 0.45
  , tfcMesaMinElevASL    = 0.05
  -- Foothill
  , tfcFoothillMinSlope   = 0.03
  , tfcFoothillMaxSlope   = 0.10
  , tfcFoothillMinElevASL = 0.02
  , tfcFoothillMaxElevASL = 0.12
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
    PipelineStage "applyParameterLayers" "applyParameterLayers" $ do
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
      padded  = size + 2

      -- Phase 7.2: Build padded elevation buffer once per chunk.
      -- Interior tiles use the fast local-vector path; only the 1-tile
      -- boundary ring (≤ 4·(size+1) tiles) falls through to IntMap.
      !paddedBuf = mkPaddedElevation chunks config origin elev

      -- Phase 7.1: Fused single-pass stencil derivation.
      -- Reads center + 8 neighbors = 9 array reads per tile (was ~36
      -- closure calls across 5 separate U.generate passes).
      --
      -- Substrate vectors (hardness, soil depth) are read from the chunk
      -- for the enriched terrain form classification.
      !hardnessVec = tcHardness chunk
      !soilVec     = tcSoilDepth chunk
      !(dirSlope, curvature, relief, rugged, terrForm) = runST $ do
        dsV     <- UM.new n
        curvV   <- UM.new n
        reliefV <- UM.new n
        ruggedV <- UM.new n
        formV   <- UM.new n
        let {-# INLINE p #-}
            p !row !col = U.unsafeIndex paddedBuf (row * padded + col)
            go !i
              | i >= n    = pure ()
              | otherwise = do
                  let !lx     = i `mod` size
                      !ly     = i `div` size
                      -- Padded coords: center at (ly+1, lx+1)
                      !e0     = p (ly + 1) (lx + 1)
                      -- 6 hex neighbours (axial offsets mapped to padded grid)
                      -- E:  (dq=+1, dr= 0) → col+1, row+0
                      !eHexE  = p (ly + 1) (lx + 2)
                      -- NE: (dq=+1, dr=−1) → col+1, row−1
                      !eHexNE = p  ly      (lx + 2)
                      -- NW: (dq= 0, dr=−1) → col+0, row−1
                      !eHexNW = p  ly      (lx + 1)
                      -- W:  (dq=−1, dr= 0) → col−1, row+0
                      !eHexW  = p (ly + 1)  lx
                      -- SW: (dq=−1, dr=+1) → col−1, row+1
                      !eHexSW = p (ly + 2)  lx
                      -- SE: (dq= 0, dr=+1) → col+0, row+1
                      !eHexSE = p (ly + 2) (lx + 1)

                      -- Directional slopes (neighbour − center)
                      !ds = DirectionalSlope
                              (eHexE  - e0)
                              (eHexNE - e0)
                              (eHexNW - e0)
                              (eHexW  - e0)
                              (eHexSW - e0)
                              (eHexSE - e0)

                      -- Curvature (Laplacian over 6 hex neighbours)
                      !c  = (eHexE + eHexNE + eHexNW
                           + eHexW + eHexSW + eHexSE)
                          - 6 * e0

                      -- Also read the 4 diagonal corners for relief/ruggedness
                      -- (these are NOT hex neighbours but complete the 3×3 grid)
                      !eNW_sq = p  ly       lx
                      !eNE_sq = p  ly      (lx + 2)
                      !eSW_sq = p (ly + 2)  lx
                      !eSE_sq = p (ly + 2) (lx + 2)

                      -- Relief (range over hex neighbours + center)
                      !lo     = min e0 $ min eHexE $ min eHexNE $ min eHexNW
                              $ min eHexW $ min eHexSW eHexSE
                      !hi     = max e0 $ max eHexE $ max eHexNE $ max eHexNW
                              $ max eHexW $ max eHexSW eHexSE
                      !r      = hi - lo

                      -- Ruggedness (mean abs diff to 6 hex neighbours)
                      !tri    = ( abs (eHexE  - e0) + abs (eHexNE - e0)
                                + abs (eHexNW - e0) + abs (eHexW  - e0)
                                + abs (eHexSW - e0) + abs (eHexSE - e0)
                                ) / 6.0

                      -- Local minimum (all 6 hex neighbours ≥ center)
                      !localMin = eHexE >= e0 && eHexNE >= e0 && eHexNW >= e0
                               && eHexW >= e0 && eHexSW >= e0 && eHexSE >= e0

                      -- Substrate and elevation above sea level for
                      -- enriched terrain form classification.
                      !hard    = if i < U.length hardnessVec
                                 then hardnessVec U.! i else 0.5
                      !soil    = if i < U.length soilVec
                                 then soilVec U.! i else 0.5
                      !elevASL = e0 - waterLevel

                      !tf     = classifyTerrainForm formCfg ds r c localMin
                                  hard soil elevASL

                      -- Suppress unused diagonal reads at -O2
                      _ = (eNW_sq, eNE_sq, eSW_sq, eSE_sq)
                  UM.unsafeWrite dsV     i ds
                  UM.unsafeWrite curvV   i c
                  UM.unsafeWrite reliefV i r
                  UM.unsafeWrite ruggedV i tri
                  UM.unsafeWrite formV   i tf
                  go (i + 1)
        go 0
        (,,,,) <$> U.unsafeFreeze dsV
               <*> U.unsafeFreeze curvV
               <*> U.unsafeFreeze reliefV
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
      , tcRuggedness  = rugged
      , tcTerrainForm = terrForm
      }

---------------------------------------------------------------------------
-- Padded elevation buffer
---------------------------------------------------------------------------

-- | Build a padded @(size+2) × (size+2)@ elevation buffer that includes
-- a 1-tile boundary ring from neighboring chunks.  Interior tiles use the
-- fast local-vector path; only the boundary ring (≤ @4·(size+1)@ tiles)
-- falls through to @IntMap@ lookups.
{-# INLINE mkPaddedElevation #-}
mkPaddedElevation
  :: IntMap TerrainChunk
  -> WorldConfig
  -> TileCoord          -- ^ chunk origin in global tile space
  -> U.Vector Float     -- ^ local elevation vector
  -> U.Vector Float     -- ^ padded buffer, row-major, origin at (-1,-1)
mkPaddedElevation chunks config origin@(TileCoord ox oy) localElev =
  let size   = wcChunkSize config
      padded = size + 2
      elevAt = mkElevLookup chunks config origin localElev
  in U.generate (padded * padded) $ \i ->
       let !px = i `mod` padded
           !py = i `div` padded
       in elevAt (ox + px - 1) (oy + py - 1)

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

-- | 6-direction hex neighbor offsets @(dq, dr)@.
hexNeighborOffsets :: [(Int, Int)]
hexNeighborOffsets =
  [ ( 1,  0), ( 1, -1), ( 0, -1)
  , (-1,  0), (-1,  1), ( 0,  1)
  ]

-- | Compute directional slope to all 6 hex neighbours.
slopeAt :: (Int -> Int -> Float) -> Int -> Int -> DirectionalSlope
slopeAt elevAt gx gy =
  let !e0 = elevAt gx gy
  in DirectionalSlope
       (elevAt (gx + 1)  gy      - e0)   -- E
       (elevAt (gx + 1) (gy - 1) - e0)   -- NE
       (elevAt  gx      (gy - 1) - e0)   -- NW
       (elevAt (gx - 1)  gy      - e0)   -- W
       (elevAt (gx - 1) (gy + 1) - e0)   -- SW
       (elevAt  gx      (gy + 1) - e0)   -- SE

-- | Laplacian curvature from 6 hex neighbours.
curvatureAt :: (Int -> Int -> Float) -> Int -> Int -> Float
curvatureAt elevAt gx gy =
  let !e0 = elevAt gx gy
  in elevAt (gx + 1)  gy
   + elevAt (gx + 1) (gy - 1)
   + elevAt  gx      (gy - 1)
   + elevAt (gx - 1)  gy
   + elevAt (gx - 1) (gy + 1)
   + elevAt  gx      (gy + 1)
   - 6 * e0

-- | Local elevation relief: range over 6 hex neighbours plus center.
reliefAt :: (Int -> Int -> Float) -> Int -> Int -> Float
reliefAt elevAt gx gy =
  let !e0 = elevAt gx gy
      step (!mn, !mx) (dx, dy) =
        let !e = elevAt (gx + dx) (gy + dy)
        in (min mn e, max mx e)
      (!lo, !hi) = foldl' step (e0, e0) hexNeighborOffsets
  in hi - lo

-- | Terrain Ruggedness Index: mean absolute elevation difference to
--   6 hex neighbours.
ruggednessAt :: (Int -> Int -> Float) -> Int -> Int -> Float
ruggednessAt elevAt gx gy =
  let !e0 = elevAt gx gy
      step !acc (dx, dy) = acc + abs (elevAt (gx + dx) (gy + dy) - e0)
      !triSum = foldl' step 0 hexNeighborOffsets
  in triSum / 6.0

-- | Whether a tile is a local elevation minimum (all 6 hex neighbours ≥ center).
isLocalMinimum :: (Int -> Int -> Float) -> Int -> Int -> Bool
isLocalMinimum elevAt gx gy =
  let !e0 = elevAt gx gy
  in all (\(dx, dy) -> elevAt (gx + dx) (gy + dy) >= e0) hexNeighborOffsets

-- | Classify terrain form from directional slope, relief, curvature,
--   local-minimum flag, substrate properties, and elevation above sea level.
--
-- Decision cascade (first match wins):
--
--    1. 'FormCliff'       — @dsMaxSlope > cliffThreshold@
--    2. 'FormCanyon'      — valley curvature + high relief + steep opposite
--                           walls + hard rock
--    3. 'FormBadlands'    — high max slope + soft substrate + high asymmetry
--    4. 'FormMountainous' — high avg slope or high relief (and not soft/badlands)
--    5. 'FormRidge'       — steep on an opposite-direction pair, flat along axis
--    6. 'FormPass'        — ridge-like cross-axis profile + local minimum along axis
--    7. 'FormEscarpment'  — steep face on one side, gentle on opposite
--    8. 'FormMesa'        — flat top + steep edges + hard cap rock + elevated
--    9. 'FormPlateau'     — low slope + high elevation ASL
--   10. 'FormValley'      — strongly negative curvature
--   11. 'FormDepression'  — local minimum
--   12. 'FormFoothill'    — moderate slope + moderate elevation
--   13. 'FormHilly'       — moderate slope + relief above hill thresholds
--   14. 'FormRolling'     — slope above rolling threshold
--   15. 'FormFlat'        — otherwise
{-# INLINE classifyTerrainForm #-}
classifyTerrainForm
  :: TerrainFormConfig
  -> DirectionalSlope   -- ^ per-direction slopes
  -> Float              -- ^ relief (max − min in neighbourhood)
  -> Float              -- ^ curvature (Laplacian)
  -> Bool               -- ^ local minimum
  -> Float              -- ^ hardness [0,1]
  -> Float              -- ^ soil depth [0,1]
  -> Float              -- ^ elevation above sea level
  -> TerrainForm
classifyTerrainForm cfg ds r c localMin hardness _soilDepth elevASL
  -- 1. Cliff — sheer face dominates
  | maxS > tfcCliffSlope cfg
  = FormCliff

  -- 2. Canyon — valley + high relief + steep opposite walls + hard rock
  | c < negate (tfcValleyCurvature cfg)
    && r > tfcCanyonMinRelief cfg
    && hassteepOppPair (tfcCanyonMinWallSlope cfg)
    && hardness >= tfcCanyonMinHardness cfg
  = FormCanyon

  -- 3. Badlands — steep + soft substrate + high asymmetry
  | maxS > tfcBadlandsMinMaxSlope cfg
    && hardness <= tfcBadlandsMaxHardness cfg
    && asym > tfcBadlandsMinAsymmetry cfg
  = FormBadlands

  -- 4. Mountainous — high average slope or high relief
  | avgS > tfcMountainSlope cfg || r > tfcMountainRelief cfg
  = FormMountainous

  -- 5. Pass — ridge-like cross-axis + local minimum along the axis
  --    (more specific than Ridge: same profile but requires localMin)
  | localMin && hasPassProfile
  = FormPass

  -- 6. Ridge — steep on an opposite pair, flat along perpendicular axis
  | hasRidgeProfile
  = FormRidge

  -- 7. Escarpment — steep face on one side, gentle on opposite
  | hasEscarpmentProfile
  = FormEscarpment

  -- 8. Mesa — flat top + steep neighbour edges + hard cap rock + elevated
  | maxS <= tfcMesaMaxTopSlope cfg
    && r > tfcMesaMinEdgeRelief cfg
    && hardness >= tfcMesaMinHardness cfg
    && elevASL >= tfcMesaMinElevASL cfg
  = FormMesa

  -- 9. Plateau — low slope + high elevation
  | maxS <= tfcPlateauMaxSlope cfg
    && elevASL >= tfcPlateauMinElevASL cfg
  = FormPlateau

  -- 10. Valley — strongly negative curvature
  | c < negate (tfcValleyCurvature cfg)
  = FormValley

  -- 11. Depression — local minimum
  | localMin
  = FormDepression

  -- 12. Foothill — moderate slope + moderate elevation
  | avgS >= tfcFoothillMinSlope cfg && avgS <= tfcFoothillMaxSlope cfg
    && elevASL >= tfcFoothillMinElevASL cfg
    && elevASL <= tfcFoothillMaxElevASL cfg
  = FormFoothill

  -- 13. Hilly — moderate slope + relief
  | avgS > tfcHillSlope cfg && r > tfcHillRelief cfg
  = FormHilly

  -- 14. Rolling — slope above rolling threshold
  | avgS > tfcRollingSlope cfg
  = FormRolling

  -- 15. Flat — everything else
  | otherwise
  = FormFlat

  where
    !avgS = dsAvgSlope ds
    !maxS = dsMaxSlope ds
    !asym = dsAsymmetry ds

    -- | Check if any opposite-direction pair both exceed a threshold.
    {-# INLINE hassteepOppPair #-}
    hassteepOppPair :: Float -> Bool
    hassteepOppPair thr =
      any (\d -> let opp = hexOpposite d
                     !sD   = abs (dsSlopeIn d ds)
                     !sOpp = abs (dsSlopeIn opp ds)
                 in sD >= thr && sOpp >= thr
          ) [HexE, HexNE, HexNW]  -- only 3 unique pairs

    -- | Ridge: at least one opposite pair has both sides steep
    -- AND the perpendicular axis (other 4 directions) is relatively flat.
    {-# INLINE hasRidgeProfile #-}
    hasRidgeProfile :: Bool
    hasRidgeProfile =
      any (\d ->
        let opp = hexOpposite d
            !sD   = abs (dsSlopeIn d ds)
            !sOpp = abs (dsSlopeIn opp ds)
            -- Axis directions: the 4 directions that are NOT d or opp
            !axisMax = maxExcludingPair d opp
        in sD >= tfcRidgeMinSlope cfg
           && sOpp >= tfcRidgeMinSlope cfg
           && axisMax <= tfcRidgeMaxAxisSlope cfg
           && (sD + sOpp) / 2 - axisMax >= tfcRidgeMinAsymmetry cfg
      ) [HexE, HexNE, HexNW]

    -- | Pass: cross-axis slopes are steep, but axis slopes are gentle.
    {-# INLINE hasPassProfile #-}
    hasPassProfile :: Bool
    hasPassProfile =
      any (\d ->
        let opp = hexOpposite d
            !sD   = abs (dsSlopeIn d ds)
            !sOpp = abs (dsSlopeIn opp ds)
            !crossAvg = (sD + sOpp) / 2
        in crossAvg >= tfcPassMinCrossSlope cfg
           && maxExcludingPair d opp <= tfcPassMaxAxisSlope cfg
      ) [HexE, HexNE, HexNW]

    -- | Escarpment: one side steep, opposite side gentle.
    {-# INLINE hasEscarpmentProfile #-}
    hasEscarpmentProfile :: Bool
    hasEscarpmentProfile =
      any (\d ->
        let opp = hexOpposite d
            !sD   = abs (dsSlopeIn d ds)
            !sOpp = abs (dsSlopeIn opp ds)
        in sD >= tfcEscarpmentMinSlope cfg
           && sOpp <= tfcEscarpmentMaxOppSlope cfg
      ) allHexDirections

    -- | Maximum absolute slope excluding a given opposite pair.
    {-# INLINE maxExcludingPair #-}
    maxExcludingPair :: HexDirection -> HexDirection -> Float
    maxExcludingPair d1 d2 =
      foldl' (\acc d ->
        if d == d1 || d == d2 then acc
        else max acc (abs (dsSlopeIn d ds))
      ) 0.0 allHexDirections

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

