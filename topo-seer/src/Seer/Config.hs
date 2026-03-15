{-# LANGUAGE OverloadedStrings #-}

module Seer.Config
  ( applyUiConfig
  , configFromUi
  , configSummary
  , mapRange
  , mapIntRange
  , unmapRange
  , unmapIntRange
  ) where

import Actor.UI (UiState(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Seer.Config.Range (mapIntRange, mapRange, unmapIntRange, unmapRange)
import Seer.Config.SliderConfig (applySliderConfig)
import Topo.WorldGen (TerrainConfig(..), WorldGenConfig(..), defaultWorldGenConfig)

-- | Apply the current UI slider state to an existing 'WorldGenConfig'.
--
-- All slider-backed writes are routed through the shared
-- registry-driven config update table in 'Seer.Config.SliderConfig',
-- including the coupled world-extent and slice-extent derivations.
applyUiConfig :: UiState -> WorldGenConfig -> WorldGenConfig
applyUiConfig ui = applySliderConfig ui

-- | Produce a diagnostic summary of all slider fields for logging.
configSummary :: UiState -> Text
configSummary ui =
  Text.intercalate " | "
    [ "Config seed=" <> Text.pack (show (uiSeed ui)) <> " chunk=" <> Text.pack (show (uiChunkSize ui))
    , kv "water" uiWaterLevel <> " " <> kv "oroLift" uiOrographicLift <> " " <> kv "rShadow" uiRainShadowLoss
    ]
  where
    kv :: Show a => Text -> (UiState -> a) -> Text
    kv label f = label <> "=" <> Text.pack (show (f ui))

-- | Build a 'WorldGenConfig' from the current slider state.
--
-- This is a thin wrapper around 'applyUiConfig' applied to the library
-- default.  Every slider value in 'UiState' is mapped to its real domain
-- value inside 'WorldGenConfig'.
configFromUi :: UiState -> WorldGenConfig
configFromUi ui = applyUiConfig ui defaultWorldGenConfig
