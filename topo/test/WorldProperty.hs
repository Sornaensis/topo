module WorldProperty (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Either (isRight)
import Topo

spec :: Spec
spec = describe "World properties" $ do
  prop "mkWorldConfig accepts positive chunk sizes" $ \(Positive size) ->
    mkWorldConfig size == Right (WorldConfig { wcChunkSize = size })

  prop "validateWorldGenConfig accepts non-negative iterations" $
    \(NonNegative h)
     (NonNegative t)
     (NonNegative w)
     (NonNegative m)
     (NonNegative c)
     (NonNegative b)
     (NonNegative g) ->
      let cfg = defaultWorldGenConfig
            { worldTerrain = defaultTerrainConfig
                { terrainErosion = defaultErosionConfig
                    { ecHydraulicIterations = h
                    , ecThermalIterations = t
                    }
                , terrainGlacier = defaultGlacierConfig
                    { gcFlowIterations = g
                    }
                }
            , worldClimate = defaultClimateConfig
                { ccWind = (ccWind defaultClimateConfig)
                    { windIterations = w }
                , ccMoisture = (ccMoisture defaultClimateConfig)
                    { moistIterations = m }
                , ccPrecipitation = (ccPrecipitation defaultClimateConfig)
                    { precCoastalIterations = c }
                }
            , worldBiome = defaultBiomeConfig
                { bcSmoothingIterations = b
                }
            }
      in isRight (validateWorldGenConfig cfg)

  prop "set/get elevation roundtrip" $ \x y value ->
    let size = 8
        config = WorldConfig { wcChunkSize = size }
        world = emptyWorld config defaultHexGridMeta
        coord = TileCoord (abs x `mod` size) (abs y `mod` size)
        world' = setElevationAt (ChunkId 0) coord value world
    in getElevationAt (ChunkId 0) coord world' == Just value
