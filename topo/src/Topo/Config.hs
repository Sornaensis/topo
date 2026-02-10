-- | Deprecated configuration module (compatibility shim).
--
-- Prefer the dedicated config modules (e.g. Topo.BaseHeight, Topo.WorldGen)
-- in new code.
module Topo.Config
  ( module Topo.BaseHeight
  , module Topo.Tectonics
  , module Topo.Erosion
  , module Topo.Hydrology
  , module Topo.Parameters
  , module Topo.Climate
  , module Topo.BiomeConfig
  , module Topo.Weather
  , module Topo.WorldGen
  ) where

import Topo.BaseHeight
import Topo.BiomeConfig
import Topo.Climate
import Topo.Erosion
import Topo.Hydrology
import Topo.Parameters
import Topo.Tectonics
import Topo.Weather
import Topo.WorldGen
