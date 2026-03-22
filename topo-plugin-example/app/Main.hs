{-# LANGUAGE OverloadedStrings #-}

-- | Example topo plugin: terrain roughening.
--
-- Demonstrates a generator plugin that modifies terrain data using
-- the topo Plugin SDK.  The plugin applies pseudo-random elevation
-- perturbations scaled by the user-configurable @roughness@ parameter,
-- repeated for the configured number of @iterations@.
--
-- To use: build and install the executable, then place a symlink or
-- copy in @~\/.topo\/plugins\/terrain-roughen\/@ alongside the
-- generated @manifest.json@.
module Main (main) where

import Data.Aeson (Value(..))
import Data.Bits (shiftR, xor)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word64)
import Topo.Plugin.SDK

-- | Plugin definition.
--
-- Registers a generator stage inserted after the erosion stage.
-- The generator logs a message and completes successfully.
terrainRoughenPlugin :: PluginDef
terrainRoughenPlugin = defaultPluginDef
  { pdName    = "terrain-roughen"
  , pdVersion = "0.1.0"
  , pdParams  =
      [ ParamDef
          { paramName    = "roughness"
          , paramLabel   = "Roughness Amount"
          , paramType    = PFloat
          , paramDefault = Number 0.3
          , paramMin     = Just (Number 0.0)
          , paramMax     = Just (Number 1.0)
          , paramTooltip = "How much roughness to add to terrain"
          }
      , ParamDef
          { paramName    = "iterations"
          , paramLabel   = "Roughen Passes"
          , paramType    = PInt
          , paramDefault = Number 2
          , paramMin     = Just (Number 1)
          , paramMax     = Just (Number 10)
          , paramTooltip = "Number of roughening passes"
          }
      ]
  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "erosion"
      , gdRequires    = ["erosion"]
      , gdRun         = runRoughenGenerator
      }
  }

------------------------------------------------------------------------
-- Generator: roughen terrain
------------------------------------------------------------------------

-- | Apply pseudo-random elevation perturbations to the terrain.
--
-- For each iteration, every tile's elevation is perturbed by a
-- deterministic hash of (seed, iteration, tile-index), scaled by
-- the @roughness@ parameter.  The result is clamped to [0, 1].
runRoughenGenerator :: PluginContext -> IO (Either Text GeneratorTickResult)
runRoughenGenerator ctx = do
  pcLog ctx "terrain-roughen: generator invoked"
  let params    = pcParams ctx
      roughness = paramFloat params "roughness" 0.3
      iters     = paramInt params "iterations" 2
      seed      = pcSeed ctx
  pcLog ctx ("terrain-roughen: roughness = " <> pack (show roughness)
          <> ", iterations = " <> pack (show iters))
  case decodeTerrainPayload (pcTerrain ctx) of
    Left decodeErr ->
      pure (Left ("terrain-roughen: failed to decode terrain payload: " <> decodeErr))
    Right terrainWorld -> do
      let terrainWorld' = applyRoughening seed roughness iters terrainWorld
      pcLog ctx "terrain-roughen: terrain modified"
      case generatorResultFromTerrain terrainWorld' of
        Left encodeErr ->
          pure (Left ("terrain-roughen: failed to encode terrain payload: " <> encodeErr))
        Right result -> do
          pcLog ctx "terrain-roughen: generator complete"
          pure (Right result)

-- | Apply @n@ roughening passes to all terrain chunks.
applyRoughening :: Word64 -> Double -> Int -> TerrainWorld -> TerrainWorld
applyRoughening seed roughness iters world =
  foldl (\w i -> mapChunks (roughenChunk seed roughness i) w) world [0 .. iters - 1]

-- | Roughen a single terrain chunk by perturbing its elevation vector.
roughenChunk :: Word64 -> Double -> Int -> TerrainChunk -> TerrainChunk
roughenChunk seed roughness iter chunk =
  let elev  = tcElevation chunk
      elev' = U.imap (perturbTile seed roughness iter) elev
  in chunk { tcElevation = elev' }

-- | Perturb a single tile's elevation using a simple hash.
--
-- The perturbation is in the range @[-roughness, +roughness]@ and the
-- result is clamped to @[0, 1]@.
perturbTile :: Word64 -> Double -> Int -> Int -> Float -> Float
perturbTile seed roughness iter tileIdx elev =
  let h     = simpleHash seed (fromIntegral iter) (fromIntegral tileIdx)
      delta = realToFrac roughness * (h - 0.5) * 2.0
  in clamp01 (elev + delta)

-- | Deterministic hash returning a value in @[0, 1]@.
--
-- Uses a simple multiplicative hash (Knuth) to produce a
-- pseudo-random float from three integer inputs.
simpleHash :: Word64 -> Word64 -> Word64 -> Float
simpleHash seed a b =
  let mixed = seed * 6364136223846793005 + a * 1442695040888963407 + b * 2862933555777941757
      -- Fold upper and lower 32 bits
      folded = fromIntegral (mixed `xor` (mixed `shiftR` 32)) :: Word32
  in fromIntegral folded / fromIntegral (maxBound :: Word32)

-- | Clamp a float to @[0, 1]@.
clamp01 :: Float -> Float
clamp01 x
  | x < 0    = 0
  | x > 1    = 1
  | otherwise = x

------------------------------------------------------------------------
-- Parameter helpers
------------------------------------------------------------------------

-- | Extract a numeric parameter as 'Double' with a default fallback.
paramFloat :: Map Text Value -> Text -> Double -> Double
paramFloat params key def = case Map.lookup key params of
  Just (Number n) -> realToFrac n
  _               -> def

-- | Extract a numeric parameter as 'Int' with a default fallback.
paramInt :: Map Text Value -> Text -> Int -> Int
paramInt params key def = case Map.lookup key params of
  Just (Number n) -> round n
  _               -> def

-- | Look up a parameter value and show it, with a fallback.
showParam :: Map Text Value -> Text -> Text
showParam params key = case Map.lookup key params of
  Just (Number n) -> pack (show n)
  Just (String s) -> s
  Just (Bool b)   -> pack (show b)
  Just _          -> "<complex>"
  Nothing         -> "<not set>"

main :: IO ()
main = runPlugin terrainRoughenPlugin
