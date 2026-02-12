{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Simple ocean surface current model (Phase 7.4).
--
-- Modifies climate temperatures for ocean tiles based on their
-- proximity to coastlines and latitude.  Western boundary currents
-- (ocean tiles with land to the east) receive warming; eastern
-- boundary currents (land to the west) receive cooling.
--
-- The magnitude follows a Gaussian in latitude, peaking at
-- 'occLatPeakDeg' degrees and vanishing near the equator and poles.
-- This produces climate patterns analogous to the Gulf Stream (warm
-- western boundary) and Humboldt Current (cold eastern boundary).
module Topo.OceanCurrent
  ( OceanCurrentConfig(..)
  , defaultOceanCurrentConfig
  , applyOceanCurrentsStage
  -- * Pure helpers (exported for testing)
  , oceanCurrentOffset
  ) where

import qualified Data.IntMap.Strict as IntMap
import Topo.Math (clamp01)
import Topo.Pipeline (PipelineStage(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), hexesPerDegreeLatitude)
import Topo.Plugin (logInfo, modifyWorldP)
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

---------------------------------------------------------------------------
-- Configuration
---------------------------------------------------------------------------

-- | Configuration for ocean surface current SST modifications.
data OceanCurrentConfig = OceanCurrentConfig
  { occWarmScale :: !Float
    -- ^ Maximum temperature boost for warm western boundary currents
    -- (default 0.06).
  , occColdScale :: !Float
    -- ^ Maximum temperature reduction for cold eastern boundary
    -- currents (default 0.04).
  , occLatPeakDeg :: !Float
    -- ^ Latitude (degrees) at which current strength peaks
    -- (default 35).
  , occLatWidthDeg :: !Float
    -- ^ Half-width (degrees) of the latitude response Gaussian
    -- (default 25).
  } deriving (Eq, Show)

-- | Sensible Earth-like defaults.
defaultOceanCurrentConfig :: OceanCurrentConfig
defaultOceanCurrentConfig = OceanCurrentConfig
  { occWarmScale   = 0.06
  , occColdScale   = 0.04
  , occLatPeakDeg  = 35.0
  , occLatWidthDeg = 25.0
  }

---------------------------------------------------------------------------
-- Pipeline stage
---------------------------------------------------------------------------

-- | Modify climate temperatures for ocean tiles near coastlines.
--
-- Must run after 'Topo.Climate.generateClimateStage' (needs
-- 'twClimate') and after terrain generation (needs 'tcElevation').
-- Runs before biome classification so that coastal biomes reflect
-- the modified SST.
applyOceanCurrentsStage :: OceanCurrentConfig -> Float -> PipelineStage
applyOceanCurrentsStage cfg waterLevel =
    PipelineStage "applyOceanCurrents" "applyOceanCurrents" $ do
  logInfo "applyOceanCurrents: modifying coastal SST"
  modifyWorldP $ \world ->
    let config     = twConfig world
        planet     = twPlanet world
        slice      = twSlice world
        hpd        = hexesPerDegreeLatitude planet
        degPerTile = 1.0 / max 0.001 hpd
        cs         = wcChunkSize config
        latBiasDeg = wsLatCenter slice
                   - fromIntegral (cs `div` 2) * degPerTile
        climateMap = twClimate world
        terrainMap = twTerrain world
        climateMap' = IntMap.mapWithKey
          (\k cc -> case IntMap.lookup k terrainMap of
            Nothing -> cc
            Just tc -> applyCurrentsChunk cfg waterLevel cs
                         degPerTile latBiasDeg tc cc
          ) climateMap
    in world { twClimate = climateMap' }

---------------------------------------------------------------------------
-- Per-chunk computation
---------------------------------------------------------------------------

-- | Apply ocean current temperature offsets to a single chunk.
applyCurrentsChunk
  :: OceanCurrentConfig
  -> Float          -- ^ waterLevel
  -> Int            -- ^ chunkSize
  -> Float          -- ^ degPerTile
  -> Float          -- ^ latBiasDeg
  -> TerrainChunk
  -> ClimateChunk
  -> ClimateChunk
applyCurrentsChunk cfg waterLevel cs degPerTile latBiasDeg tc cc =
  let elev    = tcElevation tc
      tempAvg = ccTempAvg cc
      n       = U.length tempAvg
      tempAvg' = U.generate n $ \i ->
        let h = elev U.! i
        in if h >= waterLevel
           then tempAvg U.! i          -- land: no modification
           else
             let y = i `div` cs
                 x = i `mod` cs
                 latDeg  = latBiasDeg + fromIntegral y * degPerTile
                 latRad  = latDeg * (pi / 180)
                 -- Check east/west neighbours for land (up to 2 tiles)
                 landE   = anyLandInDir cs elev waterLevel x y 1 0
                 landW   = anyLandInDir cs elev waterLevel x y (-1) 0
                 offset  = oceanCurrentOffset cfg latRad landE landW
             in clamp01 (tempAvg U.! i + offset)
  in cc { ccTempAvg = tempAvg' }

-- | Check if there is land within 2 tiles in the given direction.
anyLandInDir
  :: Int            -- ^ chunkSize
  -> U.Vector Float -- ^ elevation
  -> Float          -- ^ waterLevel
  -> Int -> Int     -- ^ (x, y)
  -> Int -> Int     -- ^ (dx, dy)
  -> Bool
anyLandInDir cs elev waterLevel x y dx dy =
  let inBounds bx by = bx >= 0 && bx < cs && by >= 0 && by < cs
      isLand   bx by = elev U.! (by * cs + bx) >= waterLevel
      x1 = x + dx;     y1 = y + dy
      x2 = x + 2 * dx; y2 = y + 2 * dy
  in (inBounds x1 y1 && isLand x1 y1)
  || (inBounds x2 y2 && isLand x2 y2)

---------------------------------------------------------------------------
-- Pure helper
---------------------------------------------------------------------------

-- | Temperature offset from ocean boundary currents.
--
-- Western boundary (land to the east) produces warming; eastern
-- boundary (land to the west) produces cooling.  The magnitude
-- follows a Gaussian in |latitude| centered on 'occLatPeakDeg',
-- vanishing near the equator and poles.
--
-- If a tile has land on both sides (narrow strait), the effects
-- partially cancel, which is physically reasonable.
oceanCurrentOffset
  :: OceanCurrentConfig
  -> Float  -- ^ latitude in radians
  -> Bool   -- ^ has land to the east (western boundary)
  -> Bool   -- ^ has land to the west (eastern boundary)
  -> Float  -- ^ temperature offset (positive = warming)
oceanCurrentOffset cfg latRad hasLandEast hasLandWest =
  let latDeg = abs (latRad * (180 / pi))
      peak   = occLatPeakDeg cfg
      w      = max 0.01 (occLatWidthDeg cfg)
      d      = (latDeg - peak) / w
      latFac = exp (negate (d * d))
      warm   = if hasLandEast then occWarmScale cfg * latFac else 0
      cold   = if hasLandWest then occColdScale cfg * latFac else 0
  in warm - cold
