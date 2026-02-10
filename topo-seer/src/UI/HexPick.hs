module UI.HexPick
  ( axialToScreen
  , screenToAxial
  , pointInHex
  ) where

axialToScreen :: Int -> Int -> Int -> (Int, Int)
axialToScreen size q r =
  let s = fromIntegral size :: Float
      xf = s * sqrt 3 * (fromIntegral q + fromIntegral r / 2)
      yf = s * 1.5 * fromIntegral r
  in (round xf + 40, round yf + 80)

screenToAxial :: Int -> Int -> Int -> (Int, Int)
screenToAxial size sx sy =
  let (q0, r0) = screenToAxialRaw size sx sy
      candidates = (q0, r0) : axialNeighbors (q0, r0)
  in case filter (pointInHex size (sx, sy)) candidates of
       (hit:_) -> hit
       [] -> (q0, r0)

pointInHex :: Int -> (Int, Int) -> (Int, Int) -> Bool
pointInHex size (sx, sy) (q, r) =
  let (cx, cy) = axialToScreen size q r
      s = fromIntegral size :: Float
      x = (fromIntegral sx + 0.5 - fromIntegral cx) / s
      y = (fromIntegral sy + 0.5 - fromIntegral cy) / s
      qf = (sqrt 3 / 3 * x - 1 / 3 * y)
      rf = (2 / 3 * y)
      cfX = qf
      cfZ = rf
      cfY = -cfX - cfZ
  in maximum [abs cfX, abs cfY, abs cfZ] <= 1.0

screenToAxialRaw :: Int -> Int -> Int -> (Int, Int)
screenToAxialRaw size sx sy =
  let s = fromIntegral size :: Float
      x = (fromIntegral sx + 0.5 - 40) / s
      y = (fromIntegral sy + 0.5 - 80) / s
      qf = (sqrt 3 / 3 * x - 1 / 3 * y)
      rf = (2 / 3 * y)
  in cubeRound qf rf

axialNeighbors :: (Int, Int) -> [(Int, Int)]
axialNeighbors (q, r) =
  [ (q + 1, r)
  , (q + 1, r - 1)
  , (q, r - 1)
  , (q - 1, r)
  , (q - 1, r + 1)
  , (q, r + 1)
  ]

cubeRound :: Float -> Float -> (Int, Int)
cubeRound q r =
  let x = q
      z = r
      y = -x - z
      rx = fromIntegral (round x)
      ry = fromIntegral (round y)
      rz = fromIntegral (round z)
      dx = abs (rx - x)
      dy = abs (ry - y)
      dz = abs (rz - z)
      (fx, fz)
        | dx > dy && dx > dz = (-(ry + rz), rz)
        | dy > dz = (rx, rz)
        | otherwise = (rx, -(rx + ry))
  in (round fx, round fz)
