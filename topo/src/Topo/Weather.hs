-- | Public weather API façade.
--
-- This module keeps the stable user-facing surface while the weather
-- implementation is organised across focused submodules.
module Topo.Weather
  ( WeatherConfig(..)
  , defaultWeatherConfig
  , weatherScheduleDecl
  , weatherScheduleIntervalTicks
  , weatherSeasonalPhase
  , deriveWeatherSeed
  , initWeatherStage
  , weatherSimNode
  , weatherOverlaySchema
  , weatherChunkToOverlay
  , overlayToWeatherChunk
  , getWeatherFromOverlay
  , getWeatherChunk
  , weatherFieldCount
  , WeatherNormalsChunk(..)
  , weatherNormalsOverlayName
  , weatherNormalsFieldCount
  , weatherNormalsOverlaySchema
  , weatherNormalsChunkFromClimate
  , weatherNormalsChunkToOverlay
  , overlayToWeatherNormalsChunk
  , weatherNormalsOverlayFromClimate
  , weatherNormalsOverlayForWorld
  , getWeatherNormalsFromStore
  , getWeatherNormalsFromOverlay
  , getWeatherNormalsChunkFromStore
  , getWeatherNormalsChunk
  , cloudFraction
  , seasonalITCZLatitude
  ) where

import Data.Word (Word64)
import Topo.Weather.Config
  ( WeatherConfig(..)
  , defaultWeatherConfig
  , weatherScheduleDecl
  , weatherScheduleIntervalTicks
  , weatherSeasonalPhase
  )
import Topo.Seed (deriveOverlaySeed)
import Topo.Weather.Grid
  ( weatherOverlaySchema
  , weatherChunkToOverlay
  , overlayToWeatherChunk
  , getWeatherFromOverlay
  , getWeatherChunk
  , weatherFieldCount
  )
import Topo.Weather.Init (initWeatherStage, seasonalITCZLatitude)
import Topo.Weather.Normals
  ( WeatherNormalsChunk(..)
  , weatherNormalsOverlayName
  , weatherNormalsFieldCount
  , weatherNormalsOverlaySchema
  , weatherNormalsChunkFromClimate
  , weatherNormalsChunkToOverlay
  , overlayToWeatherNormalsChunk
  , weatherNormalsOverlayFromClimate
  , weatherNormalsOverlayForWorld
  , getWeatherNormalsFromStore
  , getWeatherNormalsFromOverlay
  , getWeatherNormalsChunkFromStore
  , getWeatherNormalsChunk
  )
import Topo.Weather.Tick (weatherSimNode, cloudFraction)

-- | Weather seed derivation alias.
deriveWeatherSeed :: Word64 -> Word64 -> Word64
deriveWeatherSeed = deriveOverlaySeed
