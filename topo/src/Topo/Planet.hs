{-# LANGUAGE DeriveGeneric #-}

-- | Planetary parameters, world slices, and hex-to-geographic coordinate mapping.
--
-- One hex = 13 miles flat-to-flat. This is the universal physical scale constant.
-- Default behavior: the generated region is centered on the equator (@latCenter=0@).
-- Users never need to generate a whole planet — every world is a "slice" of a planet.
module Topo.Planet
  ( -- * Hex scale constant
    hexScaleMiles
    -- * Planet configuration
  , PlanetConfig(..)
  , PlanetConfigError(..)
  , mkPlanetConfig
  , defaultPlanetConfig
    -- * World slice
  , WorldSlice(..)
  , WorldSliceError(..)
  , mkWorldSlice
  , defaultWorldSlice
    -- * Derived helpers
  , planetCircumferenceMiles
  , hexesPerDegreeLatitude
  , hexesPerDegreeLongitude
  , tileLatitude
  , tileLongitude
  , tileYToLatDeg
    -- * Latitude mapping
  , LatitudeMapping(..)
  , mkLatitudeMapping
    -- * Extent from slice
  , sliceToWorldExtent
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (TileCoord(..), WorldConfig(..), WorldExtent, WorldExtentError, mkWorldExtent)

-- ---------------------------------------------------------------------------
-- Hex scale constant
-- ---------------------------------------------------------------------------

-- | Universal physical scale: one hex is 13 miles flat-to-flat.
hexScaleMiles :: Float
hexScaleMiles = 13.0

-- ---------------------------------------------------------------------------
-- PlanetConfig
-- ---------------------------------------------------------------------------

-- | Planetary parameters that govern climate, latitude mapping, and scale.
--
-- * @pcRadius@ — planet radius in km (Earth default: 6371).
-- * @pcAxialTilt@ — axial tilt in degrees (Earth default: 23.44).
-- * @pcInsolation@ — relative insolation (Earth = 1.0).
data PlanetConfig = PlanetConfig
  { pcRadius    :: !Float
  , pcAxialTilt :: !Float
  , pcInsolation :: !Float
  } deriving (Eq, Show, Generic)

instance ToJSON PlanetConfig where
  toJSON = genericToJSON (configOptions "pc")

instance FromJSON PlanetConfig where
  parseJSON v = genericParseJSON (configOptions "pc")
                  (mergeDefaults (toJSON defaultPlanetConfig) v)

-- | Errors produced when constructing an invalid 'PlanetConfig'.
data PlanetConfigError
  = PlanetRadiusOutOfRange !Float
    -- ^ Radius must be in [4778..9557] km.
  | PlanetTiltOutOfRange !Float
    -- ^ Axial tilt must be in [0..45] degrees.
  | PlanetInsolationOutOfRange !Float
    -- ^ Insolation must be in [0.7..1.3].
  deriving (Eq, Show)

-- | Default Earth-like planet configuration.
defaultPlanetConfig :: PlanetConfig
defaultPlanetConfig = PlanetConfig
  { pcRadius     = 6371
  , pcAxialTilt  = 23.44
  , pcInsolation = 1.0
  }

-- | Construct a validated 'PlanetConfig'.
--
-- Constraints:
--
-- * radius ∈ [4778..9557] km (0.75× – 1.5× Earth)
-- * axialTilt ∈ [0..45]°
-- * insolation ∈ [0.7..1.3]
mkPlanetConfig
  :: Float  -- ^ Planet radius (km)
  -> Float  -- ^ Axial tilt (degrees)
  -> Float  -- ^ Insolation (Earth = 1.0)
  -> Either PlanetConfigError PlanetConfig
mkPlanetConfig radius tilt insol
  | radius < 4778 || radius > 9557 = Left (PlanetRadiusOutOfRange radius)
  | tilt < 0 || tilt > 45          = Left (PlanetTiltOutOfRange tilt)
  | insol < 0.7 || insol > 1.3     = Left (PlanetInsolationOutOfRange insol)
  | otherwise = Right PlanetConfig
      { pcRadius     = radius
      , pcAxialTilt  = tilt
      , pcInsolation = insol
      }

-- ---------------------------------------------------------------------------
-- WorldSlice
-- ---------------------------------------------------------------------------

-- | Defines the geographic window (slice) of a planet that is being generated.
--
-- * @wsLatCenter@ — latitude of the slice center (degrees, − = south, + = north).
-- * @wsLatExtent@ — total latitude span in degrees (must be > 0).
-- * @wsLonCenter@ — longitude of the slice center (degrees).
-- * @wsLonExtent@ — total longitude span in degrees (must be > 0).
data WorldSlice = WorldSlice
  { wsLatCenter :: !Float
  , wsLatExtent :: !Float
  , wsLonCenter :: !Float
  , wsLonExtent :: !Float
  } deriving (Eq, Show, Generic)

instance ToJSON WorldSlice where
  toJSON = genericToJSON (configOptions "ws")

instance FromJSON WorldSlice where
  parseJSON v = genericParseJSON (configOptions "ws")
                  (mergeDefaults (toJSON defaultWorldSlice) v)

-- | Errors produced when constructing an invalid 'WorldSlice'.
data WorldSliceError
  = SliceLatCenterOutOfRange !Float
    -- ^ Latitude center must be in [-90..90].
  | SliceLatExtentNonPositive !Float
    -- ^ Latitude extent must be > 0.
  | SliceLonCenterOutOfRange !Float
    -- ^ Longitude center must be in [-180..180].
  | SliceLonExtentNonPositive !Float
    -- ^ Longitude extent must be > 0.
  deriving (Eq, Show)

-- | Default world slice: 40° latitude × 60° longitude centered on the equator.
defaultWorldSlice :: WorldSlice
defaultWorldSlice = WorldSlice
  { wsLatCenter = 0
  , wsLatExtent = 40
  , wsLonCenter = 0
  , wsLonExtent = 60
  }

-- | Construct a validated 'WorldSlice'.
--
-- Constraints:
--
-- * latCenter ∈ [-90..90]
-- * latExtent > 0
-- * lonCenter ∈ [-180..180]
-- * lonExtent > 0
mkWorldSlice
  :: Float  -- ^ Latitude center (degrees)
  -> Float  -- ^ Latitude extent (degrees)
  -> Float  -- ^ Longitude center (degrees)
  -> Float  -- ^ Longitude extent (degrees)
  -> Either WorldSliceError WorldSlice
mkWorldSlice latC latE lonC lonE
  | latC < (-90) || latC > 90 = Left (SliceLatCenterOutOfRange latC)
  | latE <= 0                 = Left (SliceLatExtentNonPositive latE)
  | lonC < (-180) || lonC > 180 = Left (SliceLonCenterOutOfRange lonC)
  | lonE <= 0                 = Left (SliceLonExtentNonPositive lonE)
  | otherwise = Right WorldSlice
      { wsLatCenter = latC
      , wsLatExtent = latE
      , wsLonCenter = lonC
      , wsLonExtent = lonE
      }

-- ---------------------------------------------------------------------------
-- Derived helpers
-- ---------------------------------------------------------------------------

-- | Planet circumference in miles, derived from radius (km → miles, C = 2πr).
planetCircumferenceMiles :: PlanetConfig -> Float
planetCircumferenceMiles pc =
  let radiusMiles = pcRadius pc * kmToMiles
  in 2 * pi * radiusMiles

-- | Number of hexes per degree of latitude (constant for a given planet).
--
-- @hexesPerDegLat = circumference / 360 / hexScaleMiles@
hexesPerDegreeLatitude :: PlanetConfig -> Float
hexesPerDegreeLatitude pc =
  planetCircumferenceMiles pc / 360.0 / hexScaleMiles

-- | Number of hexes per degree of longitude at a given latitude.
--
-- Longitude degrees shrink toward the poles by @cos(latitude)@.
-- Returns @max 0.001@ to avoid division by zero at the poles.
hexesPerDegreeLongitude :: PlanetConfig -> Float -> Float
hexesPerDegreeLongitude pc latDeg =
  let baseLon = hexesPerDegreeLatitude pc
      latRad  = latDeg * degToRad
  in baseLon * max 0.001 (cos latRad)

-- | Convert a tile Y coordinate to geographic latitude in degrees.
--
-- The tile grid center maps to @wsLatCenter@. Chunks are symmetric
-- around chunk (0,0), so the center tile Y is @chunkSize \`div\` 2@.
-- Each tile offset corresponds to @1 / hexesPerDegreeLatitude@ degrees.
--
-- Latitude increases northward (positive Y offset → higher latitude).
tileLatitude
  :: PlanetConfig
  -> WorldSlice
  -> WorldConfig
  -> TileCoord
  -> Float
tileLatitude planet slice config (TileCoord _tx ty) =
  let hpd    = hexesPerDegreeLatitude planet
      cs     = wcChunkSize config
      -- Chunks range from -ry to ry; chunk (0,0) origin is tile (0,0).
      -- The center of the grid is at the middle of chunk (0,0).
      centerTileY = cs `div` 2
      offsetTiles = ty - centerTileY
      offsetDeg   = fromIntegral offsetTiles / hpd
  in wsLatCenter slice + offsetDeg

-- | Convert a tile X coordinate to geographic longitude in degrees.
--
-- Analogous to 'tileLatitude' but for the X axis.
-- Longitude is corrected for the latitude at the tile's Y position.
tileLongitude
  :: PlanetConfig
  -> WorldSlice
  -> WorldConfig
  -> TileCoord
  -> Float
tileLongitude planet slice config coord@(TileCoord tx _ty) =
  let lat    = tileLatitude planet slice config coord
      hpdLon = hexesPerDegreeLongitude planet lat
      cs     = wcChunkSize config
      centerTileX = cs `div` 2
      offsetTiles = tx - centerTileX
      offsetDeg   = fromIntegral offsetTiles / hpdLon
  in wsLonCenter slice + offsetDeg

-- | Compute latitude in degrees from a global tile Y coordinate.
--
-- Convenience wrapper: @tileYToLatDeg planet slice config gy@
-- is equivalent to @tileLatitude planet slice config (TileCoord 0 gy)@.
tileYToLatDeg :: PlanetConfig -> WorldSlice -> WorldConfig -> Int -> Float
tileYToLatDeg planet slice config gy =
  tileLatitude planet slice config (TileCoord 0 gy)

-- ---------------------------------------------------------------------------
-- Latitude mapping
-- ---------------------------------------------------------------------------

-- | Pre-computed latitude mapping derived from planet, slice, and world
-- configuration.  Stored on 'Topo.World.TerrainWorld' so every pipeline
-- stage can access it without recomputation or formula duplication.
--
-- The mapping converts tile-grid Y coordinates to geographic latitude.
-- @latDeg(ty) = lmBiasDeg + ty * lmDegPerTile@, and analogously for
-- radians.
data LatitudeMapping = LatitudeMapping
  { lmDegPerTile  :: !Float
    -- ^ Degrees of latitude per tile-Y step.
  , lmRadPerTile  :: !Float
    -- ^ Radians of latitude per tile-Y step.
  , lmBiasDeg     :: !Float
    -- ^ Latitude of tile Y = 0 in degrees.
  , lmBiasRad     :: !Float
    -- ^ Latitude of tile Y = 0 in radians.
  , lmLatExtent   :: !Float
    -- ^ Slice latitude extent in degrees (from 'WorldSlice').
  , lmTiltScale   :: !Float
    -- ^ Axial-tilt scaling factor: @pcAxialTilt / 23.44@.
  , lmInsolation  :: !Float
    -- ^ Solar insolation multiplier (from 'PlanetConfig').
  } deriving (Eq, Show)

-- | Construct a 'LatitudeMapping' from the planet, slice, and world
-- configuration.  Consolidates the latitude derivation formula that
-- was previously duplicated across Tectonics, Climate, Weather,
-- OceanCurrent, and Vegetation modules.
mkLatitudeMapping :: PlanetConfig -> WorldSlice -> WorldConfig -> LatitudeMapping
mkLatitudeMapping planet slice wc =
  let hpd        = hexesPerDegreeLatitude planet
      degPerTile = 1.0 / max 0.001 hpd
      cs         = wcChunkSize wc
      latBiasDeg = wsLatCenter slice
                 - fromIntegral (cs `div` 2) * degPerTile
      radPerTile = degPerTile * (pi / 180.0)
      latBiasRad = latBiasDeg * (pi / 180.0)
  in LatitudeMapping
      { lmDegPerTile = degPerTile
      , lmRadPerTile = radPerTile
      , lmBiasDeg    = latBiasDeg
      , lmBiasRad    = latBiasRad
      , lmLatExtent  = wsLatExtent slice
      , lmTiltScale  = pcAxialTilt planet / 23.44
      , lmInsolation = pcInsolation planet
      }

-- ---------------------------------------------------------------------------
-- Internal constants
-- ---------------------------------------------------------------------------

-- | Conversion factor: kilometres to miles.
kmToMiles :: Float
kmToMiles = 0.621371

-- | Conversion factor: degrees to radians.
degToRad :: Float
degToRad = pi / 180.0

-- ---------------------------------------------------------------------------
-- Extent from slice
-- ---------------------------------------------------------------------------

-- | Compute the chunk radii needed to cover a 'WorldSlice' given a
-- 'PlanetConfig' and 'WorldConfig'.
--
-- @radiusY = ceiling (sliceLatExtent * hexesPerDegLat / 2 / chunkSize)@
--
-- @radiusX = ceiling (sliceLonExtent * hexesPerDegLon(latCenter) / 2 / chunkSize)@
--
-- Returns 'Left' if the computed radii are negative (should not happen
-- for valid inputs, but the validated constructor is used for safety).
sliceToWorldExtent
  :: PlanetConfig
  -> WorldSlice
  -> WorldConfig
  -> Either WorldExtentError WorldExtent
sliceToWorldExtent planet slice config =
  let hpdLat = hexesPerDegreeLatitude planet
      hpdLon = hexesPerDegreeLongitude planet (wsLatCenter slice)
      cs     = max 1 (wcChunkSize config)
      tilesY = wsLatExtent slice * hpdLat
      tilesX = wsLonExtent slice * hpdLon
      ry = max 1 (ceiling (tilesY / (2.0 * fromIntegral cs)))
      rx = max 1 (ceiling (tilesX / (2.0 * fromIntegral cs)))
  in mkWorldExtent rx ry
