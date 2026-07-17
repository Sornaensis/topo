{-# LANGUAGE OverloadedStrings #-}

module Seer.Config.SliderConfig
  ( applySliderConfig
  ) where

import Actor.UI (UiState(..))
import Data.List (foldl')
import Seer.Config.SliderConfig.Data
  ( lookupSliderConfigUpdate
  , updateTerrainGen
  , updateWorldSlice
  )
import Seer.Config.SliderRegistry (SliderDef(..), SliderId(SliderExtentX, SliderExtentY), allSliderDefs)
import Seer.Config.SliderConversion (sliderToDomainInt)
import Topo.BaseHeight (GenConfig(..))
import Topo.Planet
  ( PlanetConfig(..)
  , WorldSlice(..)
  , hexesPerDegreeLatitude
  , hexesPerDegreeLongitude
  )
import Topo.Soil (SoilConfig(..))
import Topo.Types (worldExtentOrDefault, worldExtentRadiusX, worldExtentRadiusY)
import Topo.WorldGen (TerrainConfig(..), WorldGenConfig(..))

applySliderConfig :: UiState -> WorldGenConfig -> WorldGenConfig
applySliderConfig ui cfg =
  applyDerivedUiConfig ui (foldl' (applySliderUpdate ui) cfg allSliderDefs)
    where
      applySliderUpdate :: UiState -> WorldGenConfig -> SliderDef -> WorldGenConfig
      applySliderUpdate currentUi cfg sliderDef =
        maybe cfg (\updateSlider -> updateSlider currentUi cfg) (lookupSliderConfigUpdate (sliderId sliderDef))

applyDerivedUiConfig :: UiState -> WorldGenConfig -> WorldGenConfig
applyDerivedUiConfig ui = applyWorldSliceExtents ui . applyWorldExtent ui

applyWorldExtent :: UiState -> WorldGenConfig -> WorldGenConfig
applyWorldExtent ui =
  updateTerrainGen $ \terrainGenConfig ->
    terrainGenConfig
      { gcWorldExtent = worldExtentOrDefault extentX extentY
      }
  where
    extentX = sliderToDomainInt SliderExtentX (uiWorldExtentX ui)
    extentY = sliderToDomainInt SliderExtentY (uiWorldExtentY ui)

applyWorldSliceExtents :: UiState -> WorldGenConfig -> WorldGenConfig
applyWorldSliceExtents ui cfg =
  updateWorldSlice
    (\worldSliceConfig ->
      worldSliceConfig
        { wsLatExtent = max 0.1 (fromIntegral (worldExtentRadiusY extent * 2 * chunkSize) / hexesPerDegreeLatitude planet hex)
        , wsLonExtent = max 0.1 (fromIntegral (worldExtentRadiusX extent * 2 * chunkSize) / hexesPerDegreeLongitude planet hex (wsLatCenter worldSliceConfig))
        }
    )
    cfg
  where
    terrain = worldTerrain cfg
    extent = gcWorldExtent (terrainGen terrain)
    planet = worldPlanet cfg
    hex = worldHexGrid cfg
    chunkSize = max 1 (uiChunkSize ui)

