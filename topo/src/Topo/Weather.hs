-- | Public weather API façade.
--
-- This module keeps the stable user-facing surface while the weather
-- implementation is organised across focused submodules.
module Topo.Weather
  ( WeatherConfig(..)
  , defaultWeatherConfig
  , deriveWeatherSeed
  , initWeatherStage
  , weatherSimNode
  , weatherOverlaySchema
  , weatherChunkToOverlay
  , overlayToWeatherChunk
  , getWeatherFromOverlay
  , getWeatherChunk
  , weatherFieldCount
  , cloudFraction
  , seasonalITCZLatitude
  ) where

import Data.Word (Word64)
import Topo.Weather.Config (WeatherConfig(..), defaultWeatherConfig)
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
import Topo.Weather.Tick (weatherSimNode, cloudFraction)

-- | Weather seed derivation alias.
deriveWeatherSeed :: Word64 -> Word64 -> Word64
deriveWeatherSeed = deriveOverlaySeed
