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

-- | Terrain form classification thresholds.
--
-- All slope thresholds are in *physical-slope* space: raw normalised
-- elevation deltas multiplied by 'tfcElevGradient' (default 0.574).
-- This converts unit-interval deltas to approximate rise/run ratios.
-- The conversion is applied once inside 'classifyTerrainForm'.
--
-- Relief thresholds for hill/mountain/canyon/mesa use multi-ring
-- neighbourhoods ('tcRelief2Ring', 'tcRelief3Ring') to capture regional
-- context beyond the 6-tile 1-ring neighbourhood.
data TerrainFormConfig = TerrainFormConfig
  { tfcElevGradient  :: !Float
  -- ^ Elevation gradient scale factor.  Raw normalised slope deltas are
  --   multiplied by this to produce physical-slope values used by all
  --   threshold comparisons (default: 0.574, derived from 'usElevGradient').
  , tfcCliffSlope      :: !Float
  -- ^ Physical slope above which terrain is classified as 'FormCliff'.
  , tfcMountainSlope   :: !Float
  -- ^ Physical top-3 slope threshold for 'FormMountainous'.
  , tfcMountainRelief  :: !Float
  -- ^ 3-ring relief threshold for 'FormMountainous'.
  , tfcValleyCurvature :: !Float
  -- ^ Curvature magnitude for 'FormValley' (positive value; compared as
  --   curvature < -this).
  , tfcHillSlope       :: !Float
  -- ^ Physical top-3 slope threshold for 'FormHilly'.
  , tfcHillRelief      :: !Float
  -- ^ 2-ring relief threshold for 'FormHilly'.
  , tfcRollingSlope    :: !Float
  -- ^ Physical average slope threshold for 'FormRolling'.
  , tfcRollingNearFactor :: !Float
  -- ^ Near-threshold factor for micro-relief-assisted rolling
  -- promotion.  Applied as @tfcRollingSlope * tfcRollingNearFactor@.

  -- Ridge thresholds
  , tfcRidgeMinSlope     :: !Float
  -- ^ Min physical slope on the steep axis for 'FormRidge'.
  , tfcRidgeMaxAxisSlope :: !Float
  -- ^ Max physical slope along the ridge axis.
  , tfcRidgeMinAsymmetry :: !Float
  -- ^ Min asymmetry between steep and axial physical slopes.

  -- Escarpment thresholds
  , tfcEscarpmentMinSlope    :: !Float
  -- ^ Min physical slope on the steep face for 'FormEscarpment'.
  , tfcEscarpmentMaxOppSlope :: !Float
  -- ^ Max physical slope on the opposite (gentle) face.

  -- Plateau thresholds
  , tfcPlateauMaxSlope       :: !Float
  -- ^ Max physical slope anywhere for 'FormPlateau'.
  , tfcPlateauMinElevASL     :: !Float
  -- ^ Min elevation above sea level for 'FormPlateau'.
  , tfcPlateauMaxRelief2Ring :: !Float
  -- ^ Max 2-ring relief for 'FormPlateau'.  Prevents gentle mountain
  --   slopes from being classified as plateau (default: 0.03).
  , tfcPlateauMaxMicroRelief :: !Float
  -- ^ Max micro-relief for 'FormPlateau'.  Inhibits over-classification
  -- of rough elevated flats as plateau.

  -- Badlands thresholds
  , tfcBadlandsMinMaxSlope  :: !Float
  -- ^ Min physical 'dsMaxSlope' for 'FormBadlands'.
  , tfcBadlandsMaxHardness  :: !Float
  -- ^ Max substrate hardness — badlands form in soft rock.
  , tfcBadlandsMinAsymmetry :: !Float
  -- ^ Min slope asymmetry for 'FormBadlands'.

  -- Pass thresholds
  , tfcPassMaxAxisSlope  :: !Float
  -- ^ Max physical slope along the pass axis.
  , tfcPassMinCrossSlope :: !Float
  -- ^ Min physical slope perpendicular to the axis.

  -- Canyon thresholds
  , tfcCanyonMinRelief    :: !Float
  -- ^ Min 3-ring relief for 'FormCanyon'.
  , tfcCanyonMinWallSlope :: !Float
  -- ^ Min physical slope on steep walls.
  , tfcCanyonMinHardness  :: !Float
  -- ^ Min hardness — canyons carve through hard rock.

  -- Mesa thresholds
  , tfcMesaMaxTopSlope   :: !Float
  -- ^ Max physical slope on the mesa top.
  , tfcMesaMinEdgeRelief :: !Float
  -- ^ Min 2-ring relief at mesa edges.
  , tfcMesaMinHardness   :: !Float
  -- ^ Min hardness — cap rock.
  , tfcMesaMinElevASL    :: !Float
  -- ^ Min elevation above sea level.

  -- Foothill thresholds
  , tfcFoothillMinSlope  :: !Float
  -- ^ Min physical top-3 slope for 'FormFoothill'.
  , tfcFoothillMaxSlope  :: !Float
  -- ^ Max physical top-3 slope for 'FormFoothill'.
  , tfcFoothillMinElevASL :: !Float
  -- ^ Min elevation above sea level.
  , tfcFoothillMaxElevASL :: !Float
  -- ^ Max elevation — above this is mountain, not foothill.
  , tfcMicroReliefRollingMin :: !Float
  -- ^ Minimum micro-relief needed for low-slope tiles near the rolling
  -- threshold to classify as 'FormRolling'.
  , tfcMicroReliefHillyMin :: !Float
  -- ^ Minimum micro-relief needed for micro-relief-assisted hilly
  -- classification.
  , tfcMicroReliefHillySlopeScale :: !Float
  -- ^ Scale applied to the hilly slope threshold when micro-relief is high.
  , tfcMicroReliefHillyReliefScale :: !Float
  -- ^ Scale applied to the hilly 2-ring relief threshold when micro-relief
  -- is high.
  , tfcMicroReliefSoftHardnessThreshold :: !Float
  -- ^ If hardness is below this threshold, micro-relief contribution is
  -- attenuated in near-threshold rolling/hilly promotion.
  , tfcMicroReliefSoftAttenuation :: !Float
  -- ^ Multiplicative attenuation applied to micro-relief when hardness is
  -- below 'tfcMicroReliefSoftHardnessThreshold'.
  } deriving (Eq, Show, Generic)

instance ToJSON TerrainFormConfig where
  toJSON = genericToJSON (configOptions "tfc")

instance FromJSON TerrainFormConfig where
  parseJSON v = genericParseJSON (configOptions "tfc")
                  (mergeDefaults (toJSON defaultTerrainFormConfig) v)

-- | Default terrain form thresholds.
--
-- All slope values are in physical-slope space (raw delta × 0.574).
-- Relief values are in normalised elevation units [0,1].
defaultTerrainFormConfig :: TerrainFormConfig
defaultTerrainFormConfig = TerrainFormConfig
  { tfcElevGradient    = 0.574
  , tfcCliffSlope      = 0.15
  , tfcMountainSlope   = 0.06
  , tfcMountainRelief  = 0.08
  , tfcValleyCurvature = 0.15
  , tfcHillSlope       = 0.025
  , tfcHillRelief      = 0.04
  , tfcRollingSlope    = 0.008
  , tfcRollingNearFactor = 0.85
  -- Ridge
  , tfcRidgeMinSlope     = 0.07
  , tfcRidgeMaxAxisSlope = 0.035
  , tfcRidgeMinAsymmetry = 0.045
  -- Escarpment
  , tfcEscarpmentMinSlope    = 0.085
  , tfcEscarpmentMaxOppSlope = 0.023
  -- Plateau
  , tfcPlateauMaxSlope       = 0.015
  , tfcPlateauMinElevASL     = 0.08
  , tfcPlateauMaxRelief2Ring = 0.03
  , tfcPlateauMaxMicroRelief = 0.5
  -- Badlands
  , tfcBadlandsMinMaxSlope  = 0.085
  , tfcBadlandsMaxHardness  = 0.35
  , tfcBadlandsMinAsymmetry = 0.035
  -- Pass
  , tfcPassMaxAxisSlope  = 0.023
  , tfcPassMinCrossSlope = 0.057
  -- Canyon
  , tfcCanyonMinRelief    = 0.08
  , tfcCanyonMinWallSlope = 0.10
  , tfcCanyonMinHardness  = 0.40
  -- Mesa
  , tfcMesaMaxTopSlope   = 0.015
  , tfcMesaMinEdgeRelief = 0.05
  , tfcMesaMinHardness   = 0.45
  , tfcMesaMinElevASL    = 0.05
  -- Foothill
  , tfcFoothillMinSlope   = 0.015
  , tfcFoothillMaxSlope   = 0.055
  , tfcFoothillMinElevASL = 0.02
  , tfcFoothillMaxElevASL = 0.12
  , tfcMicroReliefRollingMin = 0.62
  , tfcMicroReliefHillyMin = 0.70
  , tfcMicroReliefHillySlopeScale = 0.85
  , tfcMicroReliefHillyReliefScale = 0.85
  , tfcMicroReliefSoftHardnessThreshold = 0.35
  , tfcMicroReliefSoftAttenuation = 0.75
  }

-- | Compute the fused sub-tile relief index in @[0,1]@.
--
-- The ring-relief term is always available as the base signal.  Optional
-- noise and erosion terms are fused with explicit non-negative weights.
--
-- If both optional weights are zero (or both optional terms are absent),
-- this function returns the ring-only fallback used by pre-erosion
-- classification.
computeReliefIndex
  :: Float         -- ^ 1-ring relief
  -> Float         -- ^ 2-ring relief
  -> Float         -- ^ 3-ring relief
  -> Maybe Float   -- ^ optional normalized noise term [0,1]
  -> Maybe Float   -- ^ optional normalized erosion term [0,1]
  -> Float         -- ^ noise weight (>= 0)
  -> Float         -- ^ erosion weight (>= 0)
  -> Float
computeReliefIndex relief1 relief2 relief3 noiseTerm erosionTerm noiseWeight erosionWeight =
  let !ringTerm = clamp01 ((relief1 + relief2 + relief3) / 3)
      !wNoise = max 0 noiseWeight
      !wErosion = max 0 erosionWeight
      !weightedSum =
        maybe 0 (\n -> wNoise * clamp01 n) noiseTerm
          + maybe 0 (\e -> wErosion * clamp01 e) erosionTerm
      !weightTotal =
        maybe 0 (const wNoise) noiseTerm
          + maybe 0 (const wErosion) erosionTerm
  in if weightTotal <= 0
       then ringTerm
       else clamp01 ((ringTerm + weightedSum) / (1 + weightTotal))

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

-- | Classify terrain form from directional slope, multi-ring relief,
--   curvature, local-minimum flag, substrate properties, and elevation
--   above sea level.
--
-- All slope comparisons use physical-slope values obtained by scaling the
-- raw normalised 'DirectionalSlope' by 'tfcElevGradient'.
--
-- Decision cascade (first match wins):
--
--    1. 'FormCliff'       — @physMaxS > cliffThreshold@
--    2. 'FormCanyon'      — valley curvature + high 3-ring relief + steep
--                           opposite walls + hard rock
--    3. 'FormBadlands'    — high max physical slope + soft substrate + high
--                           asymmetry
--    4. 'FormMountainous' — high top-3 physical slope OR high 3-ring relief
--    5. 'FormPass'        — ridge-like cross-axis profile + local minimum
--    6. 'FormRidge'       — steep on opposite pair, flat along axis
--    7. 'FormEscarpment'  — steep face on one side, gentle on opposite
--    8. 'FormMesa'        — flat top + steep 2-ring relief + hard cap + elevated
--    9. 'FormPlateau'     — low physical slope + high elevation + low 2-ring relief
--   10. 'FormValley'      — strongly negative curvature
--   11. 'FormDepression'  — local minimum
--   12. 'FormFoothill'    — moderate top-3 slope + moderate elevation
--   13. 'FormHilly'       — moderate top-3 slope + 2-ring relief
--   14. 'FormRolling'     — physical avg slope above rolling threshold
--   15. 'FormFlat'        — otherwise
{-# INLINE classifyTerrainForm #-}
classifyTerrainForm
  :: TerrainFormConfig
  -> DirectionalSlope   -- ^ raw per-direction slopes (normalised)
  -> Float              -- ^ 1-ring relief (max − min in 6 neighbours + center)
  -> Float              -- ^ 2-ring relief (NEW)
  -> Float              -- ^ 3-ring relief (NEW)
  -> Float              -- ^ curvature (Laplacian)
  -> Bool               -- ^ local minimum
  -> Float              -- ^ hardness [0,1]
  -> Float              -- ^ micro-relief [0,1]
  -> Float              -- ^ elevation above sea level
  -> TerrainForm
classifyTerrainForm cfg ds _r1 relief2 relief3 c localMin hardness microRelief elevASL
  -- 1. Cliff — sheer face dominates
  | maxS > tfcCliffSlope cfg
  = FormCliff

  -- 2. Canyon — valley + high 3-ring relief + steep opposite walls + hard rock
  | c < negate (tfcValleyCurvature cfg)
    && relief3 > tfcCanyonMinRelief cfg
    && hassteepOppPair (tfcCanyonMinWallSlope cfg)
    && hardness >= tfcCanyonMinHardness cfg
  = FormCanyon

  -- 3. Badlands — steep + soft substrate + high asymmetry
  | maxS > tfcBadlandsMinMaxSlope cfg
    && hardness <= tfcBadlandsMaxHardness cfg
    && asym > tfcBadlandsMinAsymmetry cfg
  = FormBadlands

  -- 4. Mountainous — high top-3 slope or high 3-ring relief
  | top3S > tfcMountainSlope cfg || relief3 > tfcMountainRelief cfg
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

  -- 8. Mesa — flat top + steep 2-ring relief + hard cap rock + elevated
  | maxS <= tfcMesaMaxTopSlope cfg
    && relief2 > tfcMesaMinEdgeRelief cfg
    && hardness >= tfcMesaMinHardness cfg
    && elevASL >= tfcMesaMinElevASL cfg
  = FormMesa

  -- 9. Plateau — low slope + high elevation + low regional relief
  | maxS <= tfcPlateauMaxSlope cfg
    && elevASL >= tfcPlateauMinElevASL cfg
    && relief2 <= tfcPlateauMaxRelief2Ring cfg
    && microRelief <= tfcPlateauMaxMicroRelief cfg
  = FormPlateau

  -- 10. Valley — strongly negative curvature
  | c < negate (tfcValleyCurvature cfg)
  = FormValley

  -- 11. Depression — local minimum
  | localMin
  = FormDepression

  -- 12. Foothill — moderate top-3 slope + moderate elevation
  | top3S >= tfcFoothillMinSlope cfg && top3S <= tfcFoothillMaxSlope cfg
    && elevASL >= tfcFoothillMinElevASL cfg
    && elevASL <= tfcFoothillMaxElevASL cfg
  = FormFoothill

  -- 13. Hilly — moderate top-3 slope + 2-ring relief (with conservative
  -- micro-relief assist near threshold)
  | isHilly
  = FormHilly

  -- 14. Rolling — physical avg slope above rolling threshold (plus
  -- micro-relief support near threshold)
  | isRolling
  = FormRolling

  -- 15. Flat — everything else
  | otherwise
  = FormFlat

  where
    -- Physical-slope conversion: scale raw DS by elevGradient once.
    !grad = tfcElevGradient cfg
    !physDS = DirectionalSlope
                (dsSlopeE ds * grad) (dsSlopeNE ds * grad) (dsSlopeNW ds * grad)
                (dsSlopeW ds * grad) (dsSlopeSW ds * grad) (dsSlopeSE ds * grad)
    !avgS  = dsAvgSlope physDS
    !maxS  = dsMaxSlope physDS
    !top3S = dsTop3Slope physDS
    !asym  = dsAsymmetry physDS
    !microReliefEff =
      if hardness < tfcMicroReliefSoftHardnessThreshold cfg
        then clamp01 (microRelief * tfcMicroReliefSoftAttenuation cfg)
        else microRelief
    !rollingSlopeNear = tfcRollingSlope cfg * tfcRollingNearFactor cfg
    !isRolling =
      avgS > tfcRollingSlope cfg
      || (avgS > rollingSlopeNear && microReliefEff >= tfcMicroReliefRollingMin cfg)
    !isHilly =
      (top3S > tfcHillSlope cfg && relief2 > tfcHillRelief cfg)
      || ( microReliefEff >= tfcMicroReliefHillyMin cfg
           && top3S > tfcHillSlope cfg * tfcMicroReliefHillySlopeScale cfg
           && relief2 > tfcHillRelief cfg * tfcMicroReliefHillyReliefScale cfg
         )

    -- | Check if any opposite-direction pair both exceed a threshold.
    {-# INLINE hassteepOppPair #-}
    hassteepOppPair :: Float -> Bool
    hassteepOppPair thr =
      any (\d -> let opp = hexOpposite d
                     !sD   = abs (dsSlopeIn d physDS)
                     !sOpp = abs (dsSlopeIn opp physDS)
                 in sD >= thr && sOpp >= thr
          ) [HexE, HexNE, HexNW]  -- only 3 unique pairs

    -- | Ridge: at least one opposite pair has both sides steep
    -- AND the perpendicular axis (other 4 directions) is relatively flat.
    {-# INLINE hasRidgeProfile #-}
    hasRidgeProfile :: Bool
    hasRidgeProfile =
      any (\d ->
        let opp = hexOpposite d
            !sD   = abs (dsSlopeIn d physDS)
            !sOpp = abs (dsSlopeIn opp physDS)
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
            !sD   = abs (dsSlopeIn d physDS)
            !sOpp = abs (dsSlopeIn opp physDS)
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
            !sD   = abs (dsSlopeIn d physDS)
            !sOpp = abs (dsSlopeIn opp physDS)
        in sD >= tfcEscarpmentMinSlope cfg
           && sOpp <= tfcEscarpmentMaxOppSlope cfg
      ) allHexDirections

    -- | Maximum absolute slope excluding a given opposite pair.
    {-# INLINE maxExcludingPair #-}
    maxExcludingPair :: HexDirection -> HexDirection -> Float
    maxExcludingPair d1 d2 =
      foldl' (\acc d ->
        if d == d1 || d == d2 then acc
        else max acc (abs (dsSlopeIn d physDS))
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

