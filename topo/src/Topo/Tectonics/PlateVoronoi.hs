-- | Plate Voronoi lookup and warped-cell helpers for tectonics.
module Topo.Tectonics.PlateVoronoi
  ( PlateInfo(..)
  , plateInfoAtXY
  , plateInfoForCell
  , plateDistancePair
  , plateNearestPairAtXY
  , plateDistSq
  , plateCellActive
  , plateWarpXY
  , warpOctaves
  , chooseNearest
  ) where

import Data.Bits (xor)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Data.Word (Word16, Word64)
import Topo.Math (clamp01)
import Topo.Noise (domainWarp2D, noise2DContinuous)
import Topo.Tectonics.Config (TectonicsConfig(..))
import Topo.Types (PlateCrust(..))

-- | Per-plate metadata at a grid cell: identity, Voronoi centre,
-- velocity vector, base height/hardness, age, and crust type.
data PlateInfo = PlateInfo
  { plateInfoId :: !Word16
  , plateInfoCenter :: !(Float, Float)
  , plateInfoVelocity :: !(Float, Float)
  , plateInfoBaseHeight :: !Float
  , plateInfoBaseHardness :: !Float
  , plateInfoAge :: !Float
  , plateInfoCrust :: !PlateCrust
  }

plateDistancePair :: Word64 -> TectonicsConfig -> Int -> Int -> (Float, Float)
plateDistancePair seed tcfg x y =
  let size = max 1 (tcPlateSize tcfg)
      (wx, wy) = plateWarpXY seed tcfg x y
      fx = wx / fromIntegral size
      fy = wy / fromIntegral size
      cx = floor fx
      cy = floor fy
      cells = [ (ix, iy) | iy <- [cy - 1 .. cy + 1], ix <- [cx - 1 .. cx + 1] ]
      activeCells = filter (uncurry (plateCellActive seed tcfg)) cells
      cells' = if null activeCells then cells else activeCells
      dists = map (plateDistSq seed tcfg (wx, wy)) cells'
      sorted = sortBy (comparing fst) dists
  in case sorted of
      (d0, _) : (d1, _) : _ -> (sqrt d0, sqrt d1)
      (d0, _) : _ -> (sqrt d0, sqrt d0 + fromIntegral size)
      [] -> (0, fromIntegral size)

plateNearestPairAtXY :: Word64 -> TectonicsConfig -> Int -> Int -> (PlateInfo, PlateInfo, Float, Float)
plateNearestPairAtXY seed tcfg x y =
  let size = max 1 (tcPlateSize tcfg)
      (wx, wy) = plateWarpXY seed tcfg x y
      fx = wx / fromIntegral size
      fy = wy / fromIntegral size
      cx = floor fx
      cy = floor fy
      cells = [ (ix, iy) | iy <- [cy - 1 .. cy + 1], ix <- [cx - 1 .. cx + 1] ]
      activeCells = filter (uncurry (plateCellActive seed tcfg)) cells
      cells' = if null activeCells then cells else activeCells
      dists = sortBy (comparing fst) (map (plateDistSq seed tcfg (wx, wy)) cells')
  in case dists of
      (d0, info0) : (d1, info1) : _ -> (info0, info1, sqrt d0, sqrt d1)
      (d0, info0) : _ -> (info0, info0, sqrt d0, sqrt d0 + fromIntegral size)
      [] ->
        let info0 = plateInfoForCell seed tcfg cx cy
        in (info0, info0, 0, fromIntegral size)

plateDistSq :: Word64 -> TectonicsConfig -> (Float, Float) -> (Int, Int) -> (Float, PlateInfo)
plateDistSq seed tcfg (x, y) (cx, cy) =
  let info = plateInfoForCell seed tcfg cx cy
      (px, py) = plateInfoCenter info
      dx = x - px
      dy = y - py
  in (dx * dx + dy * dy, info)

plateInfoAtXY :: Word64 -> TectonicsConfig -> Int -> Int -> PlateInfo
plateInfoAtXY seed tcfg x y =
  let (wx, wy) = plateWarpXY seed tcfg x y
      size = max 1 (tcPlateSize tcfg)
      fx = wx / fromIntegral size
      fy = wy / fromIntegral size
      cx = floor fx
      cy = floor fy
      cells = [ (ix, iy) | iy <- [cy - 1 .. cy + 1], ix <- [cx - 1 .. cx + 1] ]
      activeCells = filter (uncurry (plateCellActive seed tcfg)) cells
      cells' = if null activeCells then cells else activeCells
      pickNearest = foldl' (chooseNearest (wx, wy) seed tcfg) Nothing cells'
  in case pickNearest of
      Just (_, info) -> info
      Nothing -> plateInfoForCell seed tcfg cx cy

chooseNearest :: (Float, Float) -> Word64 -> TectonicsConfig -> Maybe (Float, PlateInfo) -> (Int, Int) -> Maybe (Float, PlateInfo)
chooseNearest (x, y) seed tcfg best (cx, cy) =
  let info = plateInfoForCell seed tcfg cx cy
      (px, py) = plateInfoCenter info
      dx = x - px
      dy = y - py
      d2 = dx * dx + dy * dy
  in case best of
      Nothing -> Just (d2, info)
      Just (d2Best, bestInfo) -> if d2 < d2Best then Just (d2, info) else Just (d2Best, bestInfo)

plateInfoForCell :: Word64 -> TectonicsConfig -> Int -> Int -> PlateInfo
plateInfoForCell seed tcfg cx cy =
  let size = fromIntegral (max 1 (tcPlateSize tcfg))
      jx = noise2DContinuous (seed + 1001) (fromIntegral cx * 0.9) (fromIntegral cy * 0.9)
      jy = noise2DContinuous (seed + 2003) (fromIntegral cx * 0.9 + 11.7) (fromIntegral cy * 0.9 - 3.4)
      jitterN = noise2DContinuous (seed + 11001) (fromIntegral cx * 0.43) (fromIntegral cy * 0.43)
      jitter = min 1.1 (0.6 + jitterN * 0.6)
      (wcx, wcy) = plateWarpXY (seed + 12345) tcfg (round (fromIntegral cx * size)) (round (fromIntegral cy * size))
      baseCx = wcx / size
      baseCy = wcy / size
      center = ((baseCx + (jx - 0.5) * jitter) * size, (baseCy + (jy - 0.5) * jitter) * size)
      angle = noise2DContinuous (seed + 3001) (fromIntegral cx + 2.3) (fromIntegral cy - 9.1) * 2 * pi
      speed = tcPlateSpeed tcfg
      velocity = (cos angle * speed, sin angle * speed)
      pid = hashPlateId seed cx cy
      pid' = mergePlateId seed tcfg cx cy pid
      baseHeight = plateBaseHeight seed tcfg cx cy
      baseHardness = plateBaseHardness seed tcfg cx cy
      age = plateAge seed cx cy
      crust = plateCrust seed cx cy baseHeight
  in PlateInfo
      { plateInfoId = pid'
      , plateInfoCenter = center
      , plateInfoVelocity = velocity
      , plateInfoBaseHeight = baseHeight
      , plateInfoBaseHardness = baseHardness
      , plateInfoAge = age
      , plateInfoCrust = crust
      }

plateCellActive :: Word64 -> TectonicsConfig -> Int -> Int -> Bool
plateCellActive seed tcfg cx cy =
  let scale = max 0.01 (tcPlateMergeScale tcfg)
      n0 = noise2DContinuous (seed + 91011) (fromIntegral cx * scale) (fromIntegral cy * scale)
  in n0 >= tcPlateMergeBias tcfg

plateWarpXY :: Word64 -> TectonicsConfig -> Int -> Int -> (Float, Float)
plateWarpXY seed tcfg x y =
  let scale = max 0.0001 (tcBoundaryNoiseScale tcfg)
      strength = tcBoundaryNoiseStrength tcfg
      octaves = max 1 (tcBoundaryWarpOctaves tcfg)
      lac = max 1.2 (tcBoundaryWarpLacunarity tcfg)
      gain = clamp01 (tcBoundaryWarpGain tcfg)
  in warpOctaves octaves lac gain (seed + 7777) scale strength (fromIntegral x) (fromIntegral y)

warpOctaves :: Int -> Float -> Float -> Word64 -> Float -> Float -> Float -> Float -> (Float, Float)
warpOctaves octaves lac gain seed scale strength x y =
  let step 0 _ _ accX accY = (x + accX * strength, y + accY * strength)
      step o freq amp accX accY =
        let (wx, wy) = domainWarp2D (seed + fromIntegral o) (scale * freq) 1 x y
            dx = (wx - x) * amp
            dy = (wy - y) * amp
        in step (o - 1) (freq * lac) (amp * gain) (accX + dx) (accY + dy)
  in step octaves 1 1 0 0

mergePlateId :: Word64 -> TectonicsConfig -> Int -> Int -> Word16 -> Word16
mergePlateId seed tcfg cx cy pid =
  let scale = max 0.01 (tcPlateMergeScale tcfg)
      bias = clamp01 (tcPlateMergeBias tcfg)
      n0 = noise2DContinuous (seed + 91011) (fromIntegral cx * scale) (fromIntegral cy * scale)
      n1 = noise2DContinuous (seed + 92021) (fromIntegral cx * scale + 7.7) (fromIntegral cy * scale - 2.1)
      bucket = floor (n0 * 8) :: Int
      salt = fromIntegral ((bucket `mod` 8) * 8191) :: Word16
  in if n1 > bias then pid else pid `xor` salt

plateBaseHeight :: Word64 -> TectonicsConfig -> Int -> Int -> Float
plateBaseHeight seed tcfg cx cy =
  let n0 = noise2DContinuous (seed + 5001) (fromIntegral cx * 0.37) (fromIntegral cy * 0.37) * 2 - 1
  in tcPlateHeightBase tcfg + n0 * tcPlateHeightVariance tcfg

plateBaseHardness :: Word64 -> TectonicsConfig -> Int -> Int -> Float
plateBaseHardness seed tcfg cx cy =
  let n0 = noise2DContinuous (seed + 6001) (fromIntegral cx * 0.29) (fromIntegral cy * 0.29) * 2 - 1
  in clamp01 (tcPlateHardnessBase tcfg + n0 * tcPlateHardnessVariance tcfg)

plateAge :: Word64 -> Int -> Int -> Float
plateAge seed cx cy =
  noise2DContinuous (seed + 7001) (fromIntegral cx * 0.19) (fromIntegral cy * 0.19)

plateCrust :: Word64 -> Int -> Int -> Float -> PlateCrust
plateCrust seed cx cy baseHeight =
  let n0 = noise2DContinuous (seed + 8001) (fromIntegral cx * 0.23) (fromIntegral cy * 0.23)
  in if baseHeight + (n0 - 0.5) * 0.2 >= 0 then PlateContinental else PlateOceanic

hashPlateId :: Word64 -> Int -> Int -> Word16
hashPlateId seed x y =
  let n0 = noise2DContinuous (seed + 424242) (fromIntegral x * 0.13) (fromIntegral y * 0.13)
  in fromIntegral (floor (n0 * 65535))