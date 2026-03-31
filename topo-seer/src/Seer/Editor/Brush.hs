{-# LANGUAGE BangPatterns #-}

-- | Brush-stroke application for the terrain editor.
--
-- Applies a circular brush centred on a hex tile, modifying
-- 'TerrainChunk' elevation vectors in-place using the configured
-- 'BrushSettings' and 'EditorTool'.
module Seer.Editor.Brush
  ( applyBrushStroke
  , applySmoothStroke
  , applyFlattenStroke
  , applyNoiseStroke
  , applyPaintBiomeStroke
  , applyPaintFormStroke
  , applySetHardnessStroke
  , applyErodeStroke
  , brushWeight
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Topo (ChunkId(..), TileCoord(..), WorldConfig(..), chunkCoordFromTile, chunkIdFromCoord)
import Topo.Erosion (ErosionConfig(..), hydraulicStepGrid, thermalStepGrid)
import Topo.Hex (hexDisc, hexDistance, hexNeighbors)
import Topo.Math (clamp01, iterateN)
import Topo.Noise (fbm2D, hashSeed)
import Topo.Parameters (TerrainFormConfig)
import Topo.TerrainForm.Modifiers (defaultTerrainFormModifiers, tfmDepositSuppression, tfmErosionRate, tfmHardnessBonus)
import Topo.TerrainGrid (classifyTerrainFormGrid)
import Topo.Types (HexCoord(..), TerrainChunk(..), BiomeId, TerrainForm)
import Seer.Editor.Types (BrushSettings(..), EditorTool(..), Falloff(..))

-- | Compute the brush weight for a tile at @dist@ hexes from center,
-- given a brush @radius@ and 'Falloff' function.
--
-- Returns 0 when @dist > radius@.
brushWeight :: Falloff -> Int -> Int -> Float
brushWeight _falloff radius dist
  | dist > radius = 0
  | radius == 0   = 1
brushWeight falloff radius dist =
  let t = fromIntegral dist / fromIntegral radius
  in case falloff of
       FalloffConstant -> 1
       FalloffLinear   -> 1 - t
       FalloffSmooth   -> 0.5 * (1 + cos (pi * t))
{-# INLINE brushWeight #-}

-- | Apply a single brush stroke at @(q, r)@ to the terrain chunks.
--
-- For each tile in the brush disc, the elevation is adjusted by
-- @± strength × weight@ depending on the 'EditorTool'.  The result
-- is clamped to @[0, 1]@.
--
-- Returns the modified 'IntMap' of terrain chunks (only touched
-- chunks are copied).
applyBrushStroke
  :: WorldConfig
  -> EditorTool
  -> BrushSettings
  -> (Int, Int)
     -- ^ Center hex @(q, r)@.
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applyBrushStroke cfg tool brush (cq, cr) chunks =
  let !center = HexAxial cq cr
      !radius = brushRadius brush
      !strength = brushStrength brush
      !falloff = brushFalloff brush
      disc = hexDisc center radius
  in case tool of
       ToolRaise -> foldl' (applyToTile cfg 1 strength falloff center radius) chunks disc
       ToolLower -> foldl' (applyToTile cfg (-1) strength falloff center radius) chunks disc
       _ -> chunks

applyToTile
  :: WorldConfig
  -> Float         -- ^ sign (+1 raise, −1 lower)
  -> Float         -- ^ brush strength
  -> Falloff
  -> HexCoord      -- ^ brush center
  -> Int           -- ^ brush radius
  -> IntMap TerrainChunk
  -> HexCoord      -- ^ tile to modify
  -> IntMap TerrainChunk
applyToTile cfg sign strength falloff center radius chunks tile =
  let HexAxial tq tr = tile
      (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> chunks
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx = ly * csize + lx
             elev = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then chunks
              else
                let !dist = hexDistance center tile
                    !w = brushWeight falloff radius dist
                    !delta = sign * strength * w
                    !old = elev `U.unsafeIndex` idx
                    !new = clamp01 (old + delta)
                    !elev' = U.modify (\mv -> MU.write mv idx new) elev
                    !chunk' = chunk { tcElevation = elev' }
                in IntMap.insert key chunk' chunks

-- ---------------------------------------------------------------------------
-- Smooth tool
-- ---------------------------------------------------------------------------

-- | Apply a smooth stroke: each affected tile's elevation moves toward
-- the weighted average of its 6 hex neighbours.  Iterated @passes@
-- times per invocation.
--
-- @newElev = lerp (strength × weight) currentElev neighbourAvg@
applySmoothStroke
  :: WorldConfig
  -> BrushSettings
  -> Int
     -- ^ Number of smooth passes (1–5).
  -> (Int, Int)
     -- ^ Center hex @(q, r)@.
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applySmoothStroke cfg brush passes (cq, cr) chunks0 =
  let !center = HexAxial cq cr
      !radius = brushRadius brush
      disc = hexDisc center radius
  in iterate (smoothPass cfg brush center radius disc) chunks0 !! clampPasses passes

-- | Clamp pass count to [1, 5].
clampPasses :: Int -> Int
clampPasses n
  | n < 1     = 1
  | n > 5     = 5
  | otherwise = n
{-# INLINE clampPasses #-}

smoothPass
  :: WorldConfig
  -> BrushSettings
  -> HexCoord -> Int
  -> [HexCoord]
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
smoothPass cfg brush center radius disc chunks =
  foldl' (smoothTile cfg brush center radius chunks) chunks disc

smoothTile
  :: WorldConfig
  -> BrushSettings
  -> HexCoord -> Int
  -> IntMap TerrainChunk  -- ^ snapshot (read from)
  -> IntMap TerrainChunk  -- ^ accumulator (write to)
  -> HexCoord
  -> IntMap TerrainChunk
smoothTile cfg brush center radius snapshot acc tile =
  let HexAxial tq tr = tile
      (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key acc of
       Nothing -> acc
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx = ly * csize + lx
             elev = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then acc
              else
                let !dist   = hexDistance center tile
                    !w      = brushWeight (brushFalloff brush) radius dist
                    !str    = brushStrength brush
                    !old    = elev `U.unsafeIndex` idx
                    !avg    = neighbourAverage cfg snapshot tile
                    !new    = clamp01 (old + (avg - old) * str * w)
                    !elev'  = U.modify (\mv -> MU.write mv idx new) elev
                    !chunk' = chunk { tcElevation = elev' }
                in IntMap.insert key chunk' acc

-- | Average elevation of the 6 hex neighbours.  Missing neighbours
-- (out of bounds / unloaded chunks) are excluded from the average.
neighbourAverage :: WorldConfig -> IntMap TerrainChunk -> HexCoord -> Float
neighbourAverage cfg chunks tile =
  let nbrs = hexNeighbors tile
      (total, count) = foldl' accumNeighbour (0, 0 :: Int) nbrs
  in if count == 0 then 0 else total / fromIntegral count
  where
    accumNeighbour (!s, !n) nbr =
      case lookupTileElevation cfg chunks nbr of
        Nothing -> (s, n)
        Just e  -> (s + e, n + 1)

-- | Look up the elevation of a single hex tile in the chunk map.
lookupTileElevation :: WorldConfig -> IntMap TerrainChunk -> HexCoord -> Maybe Float
lookupTileElevation cfg chunks (HexAxial tq tr) =
  let (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> Nothing
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx = ly * csize + lx
             elev = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then Nothing
              else Just (elev `U.unsafeIndex` idx)
lookupTileElevation _ _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Flatten tool
-- ---------------------------------------------------------------------------

-- | Apply a flatten stroke: blend elevation toward @refElev@ (the
-- reference height captured at the start of the stroke).
applyFlattenStroke
  :: WorldConfig
  -> BrushSettings
  -> Float
     -- ^ Reference elevation captured at stroke start.
  -> (Int, Int)
     -- ^ Center hex @(q, r)@.
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applyFlattenStroke cfg brush refElev (cq, cr) chunks =
  let !center = HexAxial cq cr
      !radius = brushRadius brush
      disc = hexDisc center radius
  in foldl' (flattenTile cfg brush center radius refElev) chunks disc

flattenTile
  :: WorldConfig
  -> BrushSettings
  -> HexCoord -> Int -> Float
  -> IntMap TerrainChunk
  -> HexCoord
  -> IntMap TerrainChunk
flattenTile cfg brush center radius refElev chunks tile =
  let HexAxial tq tr = tile
      (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> chunks
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx   = ly * csize + lx
             elev   = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then chunks
              else
                let !dist   = hexDistance center tile
                    !w      = brushWeight (brushFalloff brush) radius dist
                    !str    = brushStrength brush
                    !old    = elev `U.unsafeIndex` idx
                    !new    = clamp01 (old + (refElev - old) * str * w)
                    !elev'  = U.modify (\mv -> MU.write mv idx new) elev
                    !chunk' = chunk { tcElevation = elev' }
                in IntMap.insert key chunk' chunks

-- ---------------------------------------------------------------------------
-- Noise tool
-- ---------------------------------------------------------------------------

-- | Apply a noise stroke: add coherent noise perturbation to elevation.
--
-- Uses 'fbm2D' with the world seed mixed with @strokeId@ so each
-- stroke produces a unique but deterministic noise pattern.
applyNoiseStroke
  :: WorldConfig
  -> BrushSettings
  -> Word64
     -- ^ World seed.
  -> Word64
     -- ^ Stroke ID (monotonically increasing).
  -> Float
     -- ^ Noise frequency (0.5–4.0).
  -> (Int, Int)
     -- ^ Center hex @(q, r)@.
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applyNoiseStroke cfg brush worldSeed strokeId freq (cq, cr) chunks =
  let !center   = HexAxial cq cr
      !radius   = brushRadius brush
      !noiseSeed = hashSeed worldSeed strokeId
      disc      = hexDisc center radius
  in foldl' (noiseTile cfg brush center radius noiseSeed freq) chunks disc

noiseTile
  :: WorldConfig
  -> BrushSettings
  -> HexCoord -> Int -> Word64 -> Float
  -> IntMap TerrainChunk
  -> HexCoord
  -> IntMap TerrainChunk
noiseTile cfg brush center radius noiseSeed freq chunks tile =
  let HexAxial tq tr = tile
      (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> chunks
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx   = ly * csize + lx
             elev   = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then chunks
              else
                let !dist   = hexDistance center tile
                    !w      = brushWeight (brushFalloff brush) radius dist
                    !str    = brushStrength brush
                    -- Scale tile coords by frequency for noise sampling
                    !nx     = fromIntegral tq * freq * 0.1
                    !ny     = fromIntegral tr * freq * 0.1
                    -- 4-octave FBM, lacunarity 2.0, gain 0.5
                    !noise  = fbm2D noiseSeed 4 2.0 0.5 nx ny
                    -- Centre noise around 0: fbm2D returns [−range,+range]
                    !delta  = noise * str * w
                    !old    = elev `U.unsafeIndex` idx
                    !new    = clamp01 (old + delta)
                    !elev'  = U.modify (\mv -> MU.write mv idx new) elev
                    !chunk' = chunk { tcElevation = elev' }
                in IntMap.insert key chunk' chunks

-- ---------------------------------------------------------------------------
-- Biome paint tool
-- ---------------------------------------------------------------------------

-- | Paint a 'BiomeId' onto all tiles in the brush disc.
--
-- Unlike elevation tools, biome painting is binary: each tile within
-- the brush radius gets the target biome weighted by brush falloff —
-- tiles with weight above 0.5 receive the new biome.
applyPaintBiomeStroke
  :: WorldConfig
  -> BrushSettings
  -> BiomeId
     -- ^ Target biome.
  -> (Int, Int)
     -- ^ Center hex @(q, r)@.
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applyPaintBiomeStroke cfg brush biomeId (cq, cr) chunks =
  let !center = HexAxial cq cr
      !radius = brushRadius brush
      disc = hexDisc center radius
  in foldl' (paintBiomeTile cfg brush center radius biomeId) chunks disc

paintBiomeTile
  :: WorldConfig
  -> BrushSettings
  -> HexCoord -> Int -> BiomeId
  -> IntMap TerrainChunk
  -> HexCoord
  -> IntMap TerrainChunk
paintBiomeTile cfg brush center radius biomeId chunks tile =
  let HexAxial tq tr = tile
      (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> chunks
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx   = ly * csize + lx
             flags  = tcFlags chunk
         in if idx < 0 || idx >= U.length flags
              then chunks
              else
                let !dist = hexDistance center tile
                    !w    = brushWeight (brushFalloff brush) radius dist
                in if w > 0.5
                     then let !flags' = U.modify (\mv -> MU.write mv idx biomeId) flags
                              !chunk' = chunk { tcFlags = flags' }
                          in IntMap.insert key chunk' chunks
                     else chunks

-- ---------------------------------------------------------------------------
-- Terrain form paint tool
-- ---------------------------------------------------------------------------

-- | Paint a 'TerrainForm' onto all tiles in the brush disc.
applyPaintFormStroke
  :: WorldConfig
  -> BrushSettings
  -> TerrainForm
     -- ^ Target terrain form.
  -> (Int, Int)
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applyPaintFormStroke cfg brush form (cq, cr) chunks =
  let !center = HexAxial cq cr
      !radius = brushRadius brush
      disc = hexDisc center radius
  in foldl' (paintFormTile cfg brush center radius form) chunks disc

paintFormTile
  :: WorldConfig
  -> BrushSettings
  -> HexCoord -> Int -> TerrainForm
  -> IntMap TerrainChunk
  -> HexCoord
  -> IntMap TerrainChunk
paintFormTile cfg brush center radius form chunks tile =
  let HexAxial tq tr = tile
      (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> chunks
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx   = ly * csize + lx
             forms  = tcTerrainForm chunk
         in if idx < 0 || idx >= U.length forms
              then chunks
              else
                let !dist = hexDistance center tile
                    !w    = brushWeight (brushFalloff brush) radius dist
                in if w > 0.5
                     then let !forms' = U.modify (\mv -> MU.write mv idx form) forms
                              !chunk' = chunk { tcTerrainForm = forms' }
                          in IntMap.insert key chunk' chunks
                     else chunks

-- ---------------------------------------------------------------------------
-- Hardness tool
-- ---------------------------------------------------------------------------

-- | Set rock hardness on tiles in the brush disc.
--
-- Blends the current hardness toward @targetHardness@ using
-- @strength × weight@, identically to the flatten tool but on
-- 'tcHardness'.
applySetHardnessStroke
  :: WorldConfig
  -> BrushSettings
  -> Float
     -- ^ Target hardness (0–1).
  -> (Int, Int)
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applySetHardnessStroke cfg brush targetH (cq, cr) chunks =
  let !center = HexAxial cq cr
      !radius = brushRadius brush
      disc = hexDisc center radius
  in foldl' (hardnessTile cfg brush center radius targetH) chunks disc

hardnessTile
  :: WorldConfig
  -> BrushSettings
  -> HexCoord -> Int -> Float
  -> IntMap TerrainChunk
  -> HexCoord
  -> IntMap TerrainChunk
hardnessTile cfg brush center radius targetH chunks tile =
  let HexAxial tq tr = tile
      (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> chunks
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx   = ly * csize + lx
             hard   = tcHardness chunk
         in if idx < 0 || idx >= U.length hard
              then chunks
              else
                let !dist  = hexDistance center tile
                    !w     = brushWeight (brushFalloff brush) radius dist
                    !str   = brushStrength brush
                    !old   = hard `U.unsafeIndex` idx
                    !new   = clamp01 (old + (targetH - old) * str * w)
                    !hard' = U.modify (\mv -> MU.write mv idx new) hard
                    !chunk' = chunk { tcHardness = hard' }
                in IntMap.insert key chunk' chunks

-- ---------------------------------------------------------------------------
-- Erosion tool
-- ---------------------------------------------------------------------------

-- | Apply local hydraulic + thermal erosion inside the brush disc.
--
-- A small axial sub-grid covering the brush radius plus a one-ring
-- border is extracted from the current terrain chunks. The existing
-- erosion kernels run on that local grid, then the eroded elevations
-- are blended back into the brush disc using the current falloff.
applyErodeStroke
  :: WorldConfig
  -> BrushSettings
  -> Int
     -- ^ Hydraulic and thermal pass count.
  -> ErosionConfig
  -> TerrainFormConfig
  -> Float
     -- ^ Water level.
  -> (Int, Int)
     -- ^ Center hex @(q, r)@.
  -> IntMap TerrainChunk
  -> IntMap TerrainChunk
applyErodeStroke cfg brush passes erosionCfg formCfg waterLevel (cq, cr) chunks
  | IntMap.null chunks = chunks
  | strength <= 0 = chunks
  | otherwise =
      let borderRadius = radius + 1
          minQ = cq - borderRadius
          maxQ = cq + borderRadius
          minR = cr - borderRadius
          maxR = cr + borderRadius
          gridW = maxQ - minQ + 1
          gridH = maxR - minR + 1
          center = HexAxial cq cr
          disc = hexDisc center radius
          elev0 = buildLocalFloatGrid cfg 0 tcElevation chunks minQ minR gridW gridH
          hard0 = buildLocalFloatGrid cfg 0.5 tcHardness chunks minQ minR gridW gridH
          formGrid = classifyTerrainFormGrid formCfg waterLevel gridW gridH elev0 hard0
          modLookup = defaultTerrainFormModifiers
          erosionMult = U.map ((strength *) . tfmErosionRate . modLookup) formGrid
          adjHardness = U.zipWith
            (\hard form -> clamp01 (hard + tfmHardnessBonus (modLookup form)))
            hard0
            formGrid
          depositFactor = U.map (\form -> 1 - tfmDepositSuppression (modLookup form)) formGrid
          scaledCfg = erosionCfg
            { ecRainRate = ecRainRate erosionCfg * strength
            , ecThermalStrength = ecThermalStrength erosionCfg * strength
            }
          passCount = clampErodePasses passes
          elev1 = iterateN passCount
            (hydraulicStepGrid gridW gridH waterLevel scaledCfg adjHardness erosionMult depositFactor)
            elev0
          elev2 = iterateN passCount
            (thermalStepGrid gridW gridH waterLevel scaledCfg adjHardness erosionMult depositFactor)
            elev1
      in foldl' (erodeTile cfg brush center radius minQ minR gridW elev0 elev2) chunks disc
  where
    radius = max 0 (brushRadius brush)
    strength = max 0 (brushStrength brush)

clampErodePasses :: Int -> Int
clampErodePasses n
  | n < 1 = 1
  | n > 20 = 20
  | otherwise = n
{-# INLINE clampErodePasses #-}

buildLocalFloatGrid
  :: WorldConfig
  -> Float
  -> (TerrainChunk -> U.Vector Float)
  -> IntMap TerrainChunk
  -> Int
  -> Int
  -> Int
  -> Int
  -> U.Vector Float
buildLocalFloatGrid cfg fallback field chunks minQ minR gridW gridH =
  U.generate (gridW * gridH) $ \i ->
    let q = minQ + (i `mod` gridW)
        r = minR + (i `div` gridW)
    in case lookupTileValue cfg field chunks (HexAxial q r) of
         Just value -> value
         Nothing -> fallback

lookupTileValue
  :: U.Unbox a
  => WorldConfig
  -> (TerrainChunk -> U.Vector a)
  -> IntMap TerrainChunk
  -> HexCoord
  -> Maybe a
lookupTileValue cfg field chunks (HexAxial tq tr) =
  let (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> Nothing
       Just chunk ->
         let csize = wcChunkSize cfg
             idx = ly * csize + lx
             values = field chunk
         in if idx < 0 || idx >= U.length values
              then Nothing
              else Just (values `U.unsafeIndex` idx)

erodeTile
  :: WorldConfig
  -> BrushSettings
  -> HexCoord
  -> Int
  -> Int
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> IntMap TerrainChunk
  -> HexCoord
  -> IntMap TerrainChunk
erodeTile cfg brush center radius minQ minR gridW source eroded chunks tile@(HexAxial tq tr) =
  let (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord tq tr)
      ChunkId key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> chunks
       Just chunk ->
         let csize = wcChunkSize cfg
             idx = ly * csize + lx
             elev = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then chunks
              else
                let localIdx = (tr - minR) * gridW + (tq - minQ)
                    old = source `U.unsafeIndex` localIdx
                    erodedVal = eroded `U.unsafeIndex` localIdx
                    dist = hexDistance center tile
                    weight = brushWeight (brushFalloff brush) radius dist
                    new = clamp01 (old + (erodedVal - old) * weight)
                    elev' = U.modify (\mv -> MU.write mv idx new) elev
                    chunk' = chunk { tcElevation = elev' }
                in IntMap.insert key chunk' chunks
