module Topo.Biome
  ( BiomeRule(..)
  , BiomeMap
  , BiomeThresholds(..)
  , defaultBiomeRules
  , defaultBiomeThresholds
  , classifyBiome
  , classifyBiomesChunk
  , smoothBiomesChunk
  , VegetationConfig(..)
  , defaultVegetationConfig
  , vegetationDensityChunk
  ) where

import qualified Data.Vector.Unboxed as U
import Topo.Types

newtype BiomeRule = BiomeRule
  { brTable :: [(BiomeId, (Float, Float), (Float, Float))]
  } deriving (Eq, Show)

newtype BiomeMap = BiomeMap (U.Vector BiomeId)
  deriving (Eq, Show)

data BiomeThresholds = BiomeThresholds
  { btBeachBand :: !Float
  , btSnowElevation :: !Float
  , btAlpineElevation :: !Float
  , btFallbackBiome :: !BiomeId
  } deriving (Eq, Show)

data VegetationConfig = VegetationConfig
  { vcBaseDensity :: !Float
  , vcBiomeBoost :: !Float
  , vcTempWeight :: !Float
  , vcPrecipWeight :: !Float
  , vcBoostBiomes :: ![BiomeId]
  , vcMinWeightSum :: !Float
  } deriving (Eq, Show)

defaultVegetationConfig :: VegetationConfig
defaultVegetationConfig = VegetationConfig
  { vcBaseDensity = 0.2
  , vcBiomeBoost = 0.6
  , vcTempWeight = 0.6
  , vcPrecipWeight = 0.4
  , vcBoostBiomes = [BiomeForest, BiomeRainforest, BiomeSwamp]
  , vcMinWeightSum = 0.0001
  }

defaultBiomeRules :: BiomeRule
defaultBiomeRules = BiomeRule
  [ (BiomeDesert, (0.2, 0.6), (0.0, 0.2))  -- desert
  , (BiomeGrassland, (0.35, 0.7), (0.2, 0.6)) -- grassland
  , (BiomeForest, (0.25, 0.6), (0.6, 0.95)) -- forest
  , (BiomeTundra, (0.0, 0.3), (0.3, 0.7)) -- tundra
  , (BiomeRainforest, (0.7, 1.0), (0.5, 1.0)) -- rainforest
  , (BiomeShrubland, (0.2, 0.45), (0.2, 0.4)) -- shrubland
  , (BiomeSavanna, (0.6, 0.9), (0.2, 0.5)) -- savanna
  , (BiomeTaiga, (0.1, 0.4), (0.4, 0.7)) -- taiga
  , (BiomeSwamp, (0.35, 0.7), (0.7, 1.0)) -- swamp
  ]

defaultBiomeThresholds :: BiomeThresholds
defaultBiomeThresholds = BiomeThresholds
  { btBeachBand = 0.03
  , btSnowElevation = 0.9
  , btAlpineElevation = 0.75
  , btFallbackBiome = BiomeGrassland
  }

classifyBiome :: BiomeRule -> BiomeThresholds -> Float -> Float -> Float -> Float -> BiomeId
classifyBiome (BiomeRule rules) thresholds waterLevel temp precip elev
  | elev < waterLevel = BiomeOcean
  | elev < waterLevel + btBeachBand thresholds = BiomeBeach
  | elev > btSnowElevation thresholds = BiomeSnow
  | elev > btAlpineElevation thresholds = BiomeAlpine
  | otherwise =
      case match rules of
        Just bid -> bid
        Nothing -> nearest rules
  where
    match [] = Nothing
    match ((bid, (t0, t1), (p0, p1)) : rest)
      | temp >= t0 && temp < t1 && precip >= p0 && precip < p1 = Just bid
      | otherwise = match rest
    nearest [] = btFallbackBiome thresholds
    nearest ((bid, tRange, pRange) : rest) =
      let (bestBid, bestDist) = foldl pick (bid, dist tRange pRange) rest
      in bestBid
    dist (t0, t1) (p0, p1) =
      let tc = (t0 + t1) / 2
          pc = (p0 + p1) / 2
          dt = temp - tc
          dp = precip - pc
      in dt * dt + dp * dp
    pick (bidA, distA) (bidB, tRange, pRange) =
      let distB = dist tRange pRange
      in if distB < distA then (bidB, distB) else (bidA, distA)

classifyBiomesChunk :: WorldConfig -> BiomeRule -> BiomeThresholds -> Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector BiomeId
classifyBiomesChunk _ rules thresholds waterLevel temp precip elev =
  U.generate (U.length temp) (\i -> classifyBiome rules thresholds waterLevel (temp U.! i) (precip U.! i) (elev U.! i))

smoothBiomesChunk :: WorldConfig -> Int -> U.Vector BiomeId -> U.Vector BiomeId
smoothBiomesChunk config iterations biomes
  | iterations <= 0 = biomes
  | otherwise = smoothBiomesChunk config (iterations - 1) (smoothOnce config biomes)

smoothOnce :: WorldConfig -> U.Vector BiomeId -> U.Vector BiomeId
smoothOnce config biomes =
  let size = wcChunkSize config
      n = U.length biomes
  in U.generate n (smoothAt size biomes)

smoothAt :: Int -> U.Vector BiomeId -> Int -> BiomeId
smoothAt size biomes i =
  let x = i `mod` size
      y = i `div` size
      b0 = biomes U.! i
      neighbors =
        [ b0
        , if x > 0 then biomes U.! (i - 1) else b0
        , if x + 1 < size then biomes U.! (i + 1) else b0
        , if y > 0 then biomes U.! (i - size) else b0
        , if y + 1 < size then biomes U.! (i + size) else b0
        ]
  in majority neighbors

majority :: [BiomeId] -> BiomeId
majority xs =
  let counts = foldl (\acc v -> insertCount v acc) [] xs
  in fst (foldl1 (\a b -> if snd a >= snd b then a else b) counts)
  where
    insertCount v [] = [(v, 1)]
    insertCount v ((k, c):rest)
      | v == k = (k, c + 1) : rest
      | otherwise = (k, c) : insertCount v rest

vegetationDensityChunk :: VegetationConfig -> U.Vector BiomeId -> U.Vector Float -> U.Vector Float -> U.Vector Float
vegetationDensityChunk cfg biomes temp precip =
  U.generate (U.length biomes) (\i ->
    let biome = biomes U.! i
        base = vcBaseDensity cfg
        boost = if biome `elem` vcBoostBiomes cfg then vcBiomeBoost cfg else 0
        tw = max 0 (vcTempWeight cfg)
        pw = max 0 (vcPrecipWeight cfg)
        denom = max (vcMinWeightSum cfg) (tw + pw)
        climate = clamp01 ((temp U.! i * tw + precip U.! i * pw) / denom)
    in clamp01 (base + boost * climate))

clamp01 :: Float -> Float
clamp01 v
  | v < 0 = 0
  | v > 1 = 1
  | otherwise = v
