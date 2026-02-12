{-# LANGUAGE BangPatterns #-}

module Topo.Hex
  ( HexGridMeta(..)
  , HexLayout(..)
  , defaultHexGridMeta
  , worldToHex
  , hexToWorld
  , axialToCube
  , cubeToAxial
  , hexDistance
  , hexNeighbors
    -- * Grid-index hex neighbours
  , hexNeighborIndices
  ) where

import Topo.Types (HexCoord(..), WorldPos(..))

newtype HexGridMeta = HexGridMeta
  { hexSize :: Float
  } deriving (Eq, Show)

data HexLayout = HexPointy | HexFlat
  deriving (Eq, Show)

defaultHexGridMeta :: HexGridMeta
defaultHexGridMeta = HexGridMeta { hexSize = 1 }

worldToHex :: HexGridMeta -> WorldPos -> HexCoord
worldToHex _ (WorldPos x y) = HexAxial (round x) (round y)

hexToWorld :: HexGridMeta -> HexCoord -> WorldPos
hexToWorld _ (HexAxial q r) = WorldPos (fromIntegral q) (fromIntegral r)
hexToWorld _ (HexCube x y _) = WorldPos (fromIntegral x) (fromIntegral y)

axialToCube :: HexCoord -> HexCoord
axialToCube (HexAxial q r) = HexCube q (-q - r) r
axialToCube cube@(HexCube _ _ _) = cube

cubeToAxial :: HexCoord -> HexCoord
cubeToAxial (HexCube x _ z) = HexAxial x z
cubeToAxial axial@(HexAxial _ _) = axial

hexDistance :: HexCoord -> HexCoord -> Int
hexDistance a b =
  let HexCube ax ay az = axialToCube a
      HexCube bx by bz = axialToCube b
      dx = abs (ax - bx)
      dy = abs (ay - by)
      dz = abs (az - bz)
  in max dx (max dy dz)

hexNeighbors :: HexCoord -> [HexCoord]
hexNeighbors coord =
  let HexAxial q r = cubeToAxial coord
      dirs =
        [ (1, 0)
        , (1, -1)
        , (0, -1)
        , (-1, 0)
        , (-1, 1)
        , (0, 1)
        ]
  in [HexAxial (q + dq) (r + dr) | (dq, dr) <- dirs]

-- | The 6 axial hex direction offsets @(dq, dr)@.
--
-- Order matches the standard hex edge convention:
-- E(0), NE(1), NW(2), W(3), SW(4), SE(5).
hexDirs :: [(Int, Int)]
hexDirs =
  [ ( 1,  0)   -- E
  , ( 1, -1)   -- NE
  , ( 0, -1)   -- NW
  , (-1,  0)   -- W
  , (-1,  1)   -- SW
  , ( 0,  1)   -- SE
  ]

-- | Return the flat-array indices of the 6 hex neighbours of tile @i@
-- in a @gridW * gridH@ axial grid.  Only indices that fall within
-- bounds are returned.
--
-- The grid stores tiles in row-major order: @index = y * gridW + x@
-- where @(x, y)@ corresponds to axial coordinates @(q, r)@.
hexNeighborIndices :: Int -> Int -> Int -> [Int]
hexNeighborIndices gridW gridH i =
  let !x = i `mod` gridW
      !y = i `div` gridW
  in [ nx + ny * gridW
     | (dx, dy) <- hexDirs
     , let !nx = x + dx
           !ny = y + dy
     , nx >= 0, nx < gridW, ny >= 0, ny < gridH
     ]
