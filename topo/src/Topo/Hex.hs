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
