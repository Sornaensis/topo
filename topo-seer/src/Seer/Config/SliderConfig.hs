{-# LANGUAGE OverloadedStrings #-}

module Seer.Config.SliderConfig
  ( applySliderConfig
  ) where

import Actor.UI (UiState(..))
import Data.List (foldl')
import Seer.Config.Range (mapRange)
import Seer.Config.SliderConfig.Data
  ( lookupSliderConfigUpdate
  , updateClimateBoundary
  , updateTerrainGen
  , updateWorldSlice
  )
import Seer.Config.SliderRegistry (SliderDef(..), SliderId(..), allSliderDefs)
import Seer.Config.SliderConversion (sliderToDomainInt)
import Topo.BaseHeight (GenConfig(..))
import Topo.Climate (BoundaryConfig(..))
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
applyDerivedUiConfig ui = applyBoundaryBiases ui . applyWorldSliceExtents ui . applyWorldExtent ui

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
        { wsLatExtent = max 0.1 (fromIntegral (worldExtentRadiusY extent * 2 * chunkSize) / hexesPerDegreeLatitude planet)
        , wsLonExtent = max 0.1 (fromIntegral (worldExtentRadiusX extent * 2 * chunkSize) / hexesPerDegreeLongitude planet (wsLatCenter worldSliceConfig))
        }
    )
    cfg
  where
    terrain = worldTerrain cfg
    extent = gcWorldExtent (terrainGen terrain)
    planet = worldPlanet cfg
    chunkSize = max 1 (uiChunkSize ui)

applyBoundaryBiases :: UiState -> WorldGenConfig -> WorldGenConfig
applyBoundaryBiases ui =
  updateClimateBoundary $ \boundary ->
    boundary
      { bndTempConvergent = mapRange (-0.2) 0.1 (uiBndTempConvergent ui)
      , bndTempDivergent = mapRange (-0.1) 0.2 (uiBndTempDivergent ui)
      , bndTempTransform = mapRange (-0.1) 0.1 (uiBndTempTransform ui)
      , bndPrecipConvergent = mapRange (-0.1) 0.2 (uiBndPrecipConvergent ui)
      , bndPrecipDivergent = mapRange (-0.2) 0.1 (uiBndPrecipDivergent ui)
      , bndPrecipTransform = mapRange (-0.1) 0.1 (uiBndPrecipTransform ui)
      }
