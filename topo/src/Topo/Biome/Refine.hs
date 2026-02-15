{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Biome refinement dispatcher.
--
-- After the primary Whittaker-style classification assigns a family-level
-- biome, a second pass runs per-tile to refine it into a specific sub-biome
-- using richer discriminators (terrain form, moisture, hardness, discharge,
-- glacier thickness, volcanic potential, etc.).
--
-- Each biome family has its own refinement module ('Topo.Biome.Refine.*')
-- with a config record and a pure per-tile function.  This module bundles
-- the configs and provides the chunk-level dispatcher.
module Topo.Biome.Refine
  ( RefinementConfig(..)
  , defaultRefinementConfig
  , aridRefinementConfig
  , lushRefinementConfig
  , refineBiomesChunk
  ) where

import GHC.Generics (Generic)
import qualified Data.Vector.Unboxed as U
import Topo.Config.JSON (configOptions, mergeDefaults, ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, Value)
import Topo.Biome.Refine.Alpine   (AlpineConfig, defaultAlpineConfig, refineAlpine)
import Topo.Biome.Refine.Coastal  (CoastalConfig(..), defaultCoastalConfig, refineCoastal)
import Topo.Biome.Refine.Desert   (DesertConfig(..), defaultDesertConfig, refineDesert)
import Topo.Biome.Refine.Forest   (ForestConfig(..), defaultForestConfig, refineForest)
import Topo.Biome.Refine.Grassland (GrasslandConfig(..), defaultGrasslandConfig, refineGrassland)
import Topo.Biome.Refine.Ocean    (OceanConfig, defaultOceanConfig, refineOcean)
import Topo.Biome.Refine.Rainforest (RainforestConfig(..), defaultRainforestConfig, refineRainforest)
import Topo.Biome.Refine.Savanna  (SavannaConfig(..), defaultSavannaConfig, refineSavanna)
import Topo.Biome.Refine.Shrubland (ShrublandConfig(..), defaultShrublandConfig, refineShrubland)
import Topo.Biome.Refine.Snow     (SnowConfig, defaultSnowConfig, refineSnow)
import Topo.Biome.Refine.Swamp    (SwampConfig(..), defaultSwampConfig, refineSwamp)
import Topo.Biome.Refine.Taiga    (TaigaConfig, defaultTaigaConfig, refineTaiga)
import Topo.Biome.Refine.Tundra   (TundraConfig, defaultTundraConfig, refineTundra)
import Topo.Biome.Refine.Volcanic (VolcanicConfig, defaultVolcanicConfig, refineVolcanic)
import Topo.Types

-- | Bundle of all per-family refinement configurations.
--
-- Each field corresponds to one biome family's refinement module
-- under @Topo.Biome.Refine.*@.  Named variants 'aridRefinementConfig'
-- and 'lushRefinementConfig' shift thresholds for dry and wet worlds.
data RefinementConfig = RefinementConfig
  { rcOcean      :: !OceanConfig
    -- ^ Ocean sub-biome refinement (deep, coral, etc.).
  , rcCoastal    :: !CoastalConfig
    -- ^ Coastal sub-biome refinement (mangrove, dunes, rocky, etc.).
  , rcDesert     :: !DesertConfig
    -- ^ Desert sub-biome refinement (hot, cold, rocky, sand, salt flat).
  , rcGrassland  :: !GrasslandConfig
    -- ^ Grassland sub-biome refinement (steppe, prairie, meadow, etc.).
  , rcForest     :: !ForestConfig
    -- ^ Forest sub-biome refinement (deciduous, coniferous, montane, etc.).
  , rcTundra     :: !TundraConfig
    -- ^ Tundra sub-biome refinement (arctic, alpine, polar desert).
  , rcRainforest :: !RainforestConfig
    -- ^ Rainforest sub-biome refinement (tropical, temperate).
  , rcShrubland  :: !ShrublandConfig
    -- ^ Shrubland sub-biome refinement (Mediterranean, xeric, moorland).
  , rcSavanna    :: !SavannaConfig
    -- ^ Savanna sub-biome refinement (woodland, tropical, grassland).
  , rcTaiga      :: !TaigaConfig
    -- ^ Taiga sub-biome refinement (boreal forest, bog).
  , rcSwamp      :: !SwampConfig
    -- ^ Swamp sub-biome refinement (marsh, bog, fen, floodplain, etc.).
  , rcSnow       :: !SnowConfig
    -- ^ Snow sub-biome refinement (ice cap, glacier, snowfield).
  , rcAlpine     :: !AlpineConfig
    -- ^ Alpine sub-biome refinement (scree, meadow, tundra).
  , rcVolcanic   :: !VolcanicConfig
    -- ^ Volcanic sub-biome refinement (lava field, ash plain).
  } deriving (Eq, Show, Generic)

-- | Serialise with two-letter @rc@ prefix stripped from field names.
instance ToJSON RefinementConfig where
  toJSON = genericToJSON (configOptions "rc")

-- | Deserialise with defaults for any missing field.
instance FromJSON RefinementConfig where
  parseJSON v = genericParseJSON (configOptions "rc") (mergeDefaults (toJSON defaultRefinementConfig) v)

-- | Default refinement configuration.
defaultRefinementConfig :: RefinementConfig
defaultRefinementConfig = RefinementConfig
  { rcOcean      = defaultOceanConfig
  , rcCoastal    = defaultCoastalConfig
  , rcDesert     = defaultDesertConfig
  , rcGrassland  = defaultGrasslandConfig
  , rcForest     = defaultForestConfig
  , rcTundra     = defaultTundraConfig
  , rcRainforest = defaultRainforestConfig
  , rcShrubland  = defaultShrublandConfig
  , rcSavanna    = defaultSavannaConfig
  , rcTaiga      = defaultTaigaConfig
  , rcSwamp      = defaultSwampConfig
  , rcSnow       = defaultSnowConfig
  , rcAlpine     = defaultAlpineConfig
  , rcVolcanic   = defaultVolcanicConfig
  }

-- | Arid-biased refinement configuration.
--
-- Shifts desert / steppe / xeric sub-biome thresholds to expand arid
-- classifications:  higher moisture tolerance for salt flats, lower
-- precipitation thresholds for steppe, and wider desert temperature
-- bands.  Wetland and forest sub-biome thresholds are tightened.
aridRefinementConfig :: RefinementConfig
aridRefinementConfig = defaultRefinementConfig
  { rcDesert    = (defaultDesertConfig)
      { dcHotMinTemp       = 0.60   -- easier to classify as hot desert
      , dcColdMaxTemp      = 0.45   -- wider cold-desert band
      , dcSaltFlatMaxMoist = 0.05   -- more salt flats in mildly moist basins
      , dcFogDesertMinHumidity = 0.25  -- easier fog-desert detection
      }
  , rcGrassland = (defaultGrasslandConfig)
      { grcSteppeMaxPrecip      = 0.30  -- steppe eats into grassland range
      , grcPrairieMinSoilDepth  = 0.70  -- harder to reach prairie
      , grcPrairieMinMoisture   = 0.40
      }
  , rcShrubland = (defaultShrublandConfig)
      { scXericMaxMoisture       = 0.20  -- wider xeric band
      , scMediterraneanMaxPrecip = 0.40  -- wider mediterranean
      , scMediterraneanMinPrecipSeason = 0.25  -- easier mediterranean
      }
  , rcSavanna   = (defaultSavannaConfig)
      { saGrasslandMaxPrecip   = 0.30  -- drier savanna → grassland savanna
      , saGrasslandMaxHumidity = 0.35  -- wider grassland savanna
      , saWoodlandMinPrecip    = 0.40  -- harder to reach woodland
      , saWoodlandMinHumidity  = 0.40  -- harder to reach woodland
      }
  , rcForest    = (defaultForestConfig)
      { fcDeciduousMinPrecip        = 0.55  -- tighter forest requirements
      , fcTempRainforestMinPrecip   = 0.85
      , fcCloudForestMinPrecip      = 0.75
      , fcCloudForestMinHumidity    = 0.70  -- cloud forest needs humidity
      , fcTempRainforestMinHumidity = 0.75  -- temperate rainforest needs humidity
      , fcSeasonalForestMinSeason   = 0.35  -- easier seasonal forest in arid
      }
  , rcRainforest = (defaultRainforestConfig)
      { rfTempRainforestMinPrecip   = 0.80  -- harder to form temperate rainforest
      , rfTempRainforestMaxTemp     = 0.68  -- narrower temperate rainforest band
      }
  , rcSwamp     = (defaultSwampConfig)
      { swMarshMinMoisture         = 0.80  -- harder to form wetlands
      , swFloodplainMinDischarge   = 0.50
      }
  , rcCoastal   = (defaultCoastalConfig)
      { cstDunesMaxMoisture = 0.30  -- more coastal dunes
      , cstDunesMaxPrecip   = 0.25
      }
  }

-- | Lush-biased refinement configuration.
--
-- Shifts forest / wetland / rainforest sub-biome thresholds to expand
-- verdant classifications:  lower precipitation requirements for
-- deciduous and montane forests, wider wetland ranges, easier
-- floodplain detection.  Desert sub-biomes are tightened.
lushRefinementConfig :: RefinementConfig
lushRefinementConfig = defaultRefinementConfig
  { rcForest    = (defaultForestConfig)
      { fcDeciduousMinPrecip        = 0.35  -- easier deciduous formation
      , fcConiferousMaxTemp         = 0.62  -- wider coniferous band
      , fcCloudForestMinPrecip      = 0.65  -- easier cloud forest
      , fcCloudForestMinHumidity    = 0.55  -- easier cloud forest
      , fcTempRainforestMinPrecip   = 0.55  -- easier temperate rainforest
      , fcTempRainforestMinHumidity = 0.60  -- easier temperate rainforest
      , fcSeasonalForestMinSeason   = 0.55  -- harder seasonal forest in lush
      }
  , rcRainforest = (defaultRainforestConfig)
      { rfTropicalMinTemp           = 0.70  -- wider tropical rainforest band
      , rfTempRainforestMinPrecip   = 0.55  -- easier temperate rainforest
      }
  , rcSwamp     = (defaultSwampConfig)
      { swMarshMinMoisture         = 0.60  -- easier wetland formation
      , swFloodplainMinDischarge   = 0.30  -- easier floodplain forests
      , swFenMinGwDischarge        = 0.25  -- easier fen
      }
  , rcGrassland = (defaultGrasslandConfig)
      { grcSteppeMaxPrecip      = 0.20  -- less steppe (more lush grassland)
      , grcPrairieMinSoilDepth  = 0.50  -- easier prairie
      , grcPrairieMinMoisture   = 0.30
      , grcFloodplainMinDischarge = 0.20  -- easier floodplain grassland
      }
  , rcDesert    = (defaultDesertConfig)
      { dcHotMinTemp       = 0.75   -- harder to classify as hot desert
      , dcColdMaxTemp      = 0.35   -- narrower cold-desert band
      , dcSaltFlatMaxMoist = 0.01   -- fewer salt flats
      }
  , rcSavanna   = (defaultSavannaConfig)
      { saWoodlandMinPrecip    = 0.30  -- easier woodland savanna
      , saWoodlandMinFertility = 0.35
      , saWoodlandMinHumidity  = 0.30  -- easier woodland savanna
      }
  , rcCoastal   = (defaultCoastalConfig)
      { cstMangroveMinTemp    = 0.60  -- wider mangrove band
      , cstMangroveMinPrecip  = 0.45
      , cstSaltMarshMinMoist  = 0.50  -- easier salt marsh
      }
  }

-- | Refine family-level biomes into sub-biomes for an entire chunk.
--
-- Iterates over all tiles, dispatches to the matching family refiner
-- based on the primary biome, and assembles the output vector.
-- Non-matching tiles pass through unchanged.
--
-- The volcanic overlay runs last and can override any biome.
refineBiomesChunk
  :: RefinementConfig
  -> Float                      -- ^ waterLevel
  -> U.Vector BiomeId           -- ^ primary biomes (from classifyBiome)
  -> TerrainChunk
  -> ClimateChunk
  -> WeatherChunk
  -> Maybe RiverChunk
  -> Maybe GroundwaterChunk
  -> Maybe VolcanismChunk
  -> Maybe GlacierChunk
  -> Maybe WaterBodyChunk
  -> U.Vector BiomeId           -- ^ refined biomes
refineBiomesChunk cfg wl primary tc cc _wc mRiver mGw mVolc mGlac mWb =
  let n = U.length primary

      -- Helper: safe index with default 0
      atF :: U.Vector Float -> Int -> Float
      atF v i = if i < U.length v then v U.! i else 0
      {-# INLINE atF #-}

      atTF :: U.Vector TerrainForm -> Int -> TerrainForm
      atTF v i = if i < U.length v then v U.! i else FormFlat
      {-# INLINE atTF #-}

      -- Terrain vectors
      elev       = tcElevation tc
      slope      = tcSlope tc
      moisture   = tcMoisture tc
      hardness   = tcHardness tc
      soilDepth  = tcSoilDepth tc
      fertility  = tcFertility tc
      ruggedness = tcRuggedness tc
      tform      = tcTerrainForm tc

      -- Climate vectors
      temp  = ccTempAvg cc
      prec  = ccPrecipAvg cc
      humAvg = ccHumidityAvg cc
      tempRng = ccTempRange cc
      precipSeason = ccPrecipSeasonality cc

      -- Optional chunk vectors
      discharge = fmap rcDischarge mRiver
      gwDisch   = fmap gwDischarge mGw
      lavaPot   = fmap vcLavaPotential mVolc
      ashPot    = fmap vcAshPotential mVolc
      iceThick  = fmap glIceThickness mGlac
      snowpack  = fmap glSnowpack mGlac
      wbTypes   = fmap wbType mWb

      maybeAt :: Maybe (U.Vector Float) -> Int -> Float
      maybeAt Nothing  _ = 0
      maybeAt (Just v) i = atF v i
      {-# INLINE maybeAt #-}

      maybeWbt :: Maybe (U.Vector WaterBodyType) -> Int -> WaterBodyType
      maybeWbt Nothing  _ = WaterDry
      maybeWbt (Just v) i = if i < U.length v then v U.! i else WaterDry
      {-# INLINE maybeWbt #-}

      -- Per-tile refinement
      refineAt :: Int -> BiomeId
      refineAt i =
        let bid = primary U.! i
            e   = atF elev i
            t   = atF temp i
            p   = atF prec i
            s   = atF slope i
            m   = atF moisture i
            h   = atF hardness i
            sd  = atF soilDepth i
            f   = atF fertility i
            rug = atF ruggedness i
            tf  = atTF tform i
            dis = maybeAt discharge i
            gwd = maybeAt gwDisch i
            lav = maybeAt lavaPot i
            ash = maybeAt ashPot i
            ice = maybeAt iceThick i
            snp = maybeAt snowpack i
            wbt = maybeWbt wbTypes i
            hum = atF humAvg i
            tRng = atF tempRng i
            pSeason = atF precipSeason i

            -- Family dispatch
            refined
              | bid == BiomeOcean     = refineOcean     (rcOcean cfg)     wbt wl e t s h
              | bid == BiomeLake      = bid  -- lakes pass through (no sub-biomes)
              | bid == BiomeInlandSea = bid  -- inland seas pass through
              | bid == BiomeCoastal   = refineCoastal   (rcCoastal cfg)   t p m h sd dis
              | bid == BiomeDesert    = refineDesert     (rcDesert cfg)    t m h sd tf hum pSeason
              | bid == BiomeGrassland = refineGrassland  (rcGrassland cfg) e p sd m dis tRng t
              | bid == BiomeForest    = refineForest     (rcForest cfg)    e t p h hum s tf pSeason
              | bid == BiomeTundra    = refineTundra     (rcTundra cfg)    e t p tf
              | bid == BiomeRainforest= refineRainforest (rcRainforest cfg) t p
              | bid == BiomeShrubland = refineShrubland  (rcShrubland cfg) t p m f tf pSeason
              | bid == BiomeSavanna   = refineSavanna    (rcSavanna cfg)   t p f hum pSeason
              | bid == BiomeTaiga     = refineTaiga      (rcTaiga cfg)     t p m tf tRng hum
              | bid == BiomeSwamp     = refineSwamp      (rcSwamp cfg)     t m f dis gwd tf
              | bid == BiomeSnow      = refineSnow       (rcSnow cfg)     t ice snp s
              | bid == BiomeAlpine    = refineAlpine     (rcAlpine cfg)    e t p s rug sd hum
              | otherwise             = bid

            -- Volcanic overlay (runs last, can override any biome)
            withVolcanic = refineVolcanic (rcVolcanic cfg) lav ash f refined
        in withVolcanic
      {-# INLINE refineAt #-}

  in U.generate n refineAt
