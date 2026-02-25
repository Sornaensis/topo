{-# LANGUAGE BangPatterns #-}

-- | Hex grid topology and coordinate utilities.
--
-- Provides axial\/cube coordinate conversions, hex distance, neighbour
-- lookup, the 'HexDirection' ADT representing the six cardinal hex
-- directions, and directional slope query functions ('dsSlopeIn',
-- 'dsSteepestDescent') that bridge 'HexDirection' with
-- 'Topo.Types.DirectionalSlope'.
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
    -- * Hex directions
  , HexDirection(..)
  , hexDirectionCount
  , allHexDirections
  , hexOpposite
  , hexDirOffset
  , hexNeighborInDirection
    -- * Grid-index hex neighbours
  , hexNeighborIndices
  , hexNeighborIndexInDirection
    -- * Low-level offset list
  , hexDirs
    -- * Directional slope queries
  , dsSlopeIn
  , dsSteepestDescent
  ) where

import Data.List (foldl')
import Data.Ord (comparing)
import Topo.Types (DirectionalSlope(..), HexCoord(..), WorldPos(..))

newtype HexGridMeta = HexGridMeta
  { hexSize :: Float
  } deriving (Eq, Show)

data HexLayout = HexPointy | HexFlat
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Hex directions
-- ---------------------------------------------------------------------------

-- | The six cardinal hex directions in axial coordinates.
--
-- Order matches 'hexDirs': E(0), NE(1), NW(2), W(3), SW(4), SE(5).
data HexDirection
  = HexE   -- ^ East:      @(+1,  0)@
  | HexNE  -- ^ Northeast: @(+1, −1)@
  | HexNW  -- ^ Northwest: @( 0, −1)@
  | HexW   -- ^ West:      @(−1,  0)@
  | HexSW  -- ^ Southwest: @(−1, +1)@
  | HexSE  -- ^ Southeast: @( 0, +1)@
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Total number of hex directions (always 6).
hexDirectionCount :: Int
hexDirectionCount = 6

-- | All hex directions in canonical order (E, NE, NW, W, SW, SE).
allHexDirections :: [HexDirection]
allHexDirections = [minBound .. maxBound]

-- | The opposite direction on the hex grid.
--
-- @hexOpposite (hexOpposite d) == d@ for all @d@.
hexOpposite :: HexDirection -> HexDirection
hexOpposite HexE  = HexW
hexOpposite HexNE = HexSW
hexOpposite HexNW = HexSE
hexOpposite HexW  = HexE
hexOpposite HexSW = HexNE
hexOpposite HexSE = HexNW

-- | Axial offset @(dq, dr)@ for a direction.
hexDirOffset :: HexDirection -> (Int, Int)
hexDirOffset HexE  = ( 1,  0)
hexDirOffset HexNE = ( 1, -1)
hexDirOffset HexNW = ( 0, -1)
hexDirOffset HexW  = (-1,  0)
hexDirOffset HexSW = (-1,  1)
hexDirOffset HexSE = ( 0,  1)

-- | Compute the hex neighbor of a coordinate in a given direction.
hexNeighborInDirection :: HexDirection -> HexCoord -> HexCoord
hexNeighborInDirection dir coord =
  let HexAxial q r = cubeToAxial coord
      (dq, dr) = hexDirOffset dir
  in HexAxial (q + dq) (r + dr)

-- ---------------------------------------------------------------------------
-- Grid-level helpers
-- ---------------------------------------------------------------------------

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
hexNeighbors coord = map (`hexNeighborInDirection` coord) allHexDirections

-- | The 6 axial hex direction offsets @(dq, dr)@.
--
-- Order matches the standard hex edge convention:
-- E(0), NE(1), NW(2), W(3), SW(4), SE(5).
--
-- Prefer 'hexDirOffset' and 'HexDirection' for new code.
hexDirs :: [(Int, Int)]
hexDirs = map hexDirOffset allHexDirections

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

-- | Return the flat-array index of the hex neighbour of tile @i@ in
-- direction @dir@ in a @gridW × gridH@ grid.  Returns 'Nothing' for
-- off-grid neighbours.
hexNeighborIndexInDirection :: Int -> Int -> HexDirection -> Int -> Maybe Int
hexNeighborIndexInDirection gridW gridH dir i =
  let !x  = i `mod` gridW
      !y  = i `div` gridW
      !(dx, dy) = hexDirOffset dir
      !nx = x + dx
      !ny = y + dy
  in if nx >= 0 && nx < gridW && ny >= 0 && ny < gridH
       then Just (nx + ny * gridW)
       else Nothing

---------------------------------------------------------------------------
-- Directional slope queries
---------------------------------------------------------------------------

-- | Look up the slope component in a given 'HexDirection'.
dsSlopeIn :: HexDirection -> DirectionalSlope -> Float
dsSlopeIn HexE  ds = dsSlopeE  ds
dsSlopeIn HexNE ds = dsSlopeNE ds
dsSlopeIn HexNW ds = dsSlopeNW ds
dsSlopeIn HexW  ds = dsSlopeW  ds
dsSlopeIn HexSW ds = dsSlopeSW ds
dsSlopeIn HexSE ds = dsSlopeSE ds
{-# INLINE dsSlopeIn #-}

-- | Direction of steepest descent (most negative slope).
--   Returns 'Nothing' when the tile is flat or a local minimum
--   (no neighbour is lower).
dsSteepestDescent :: DirectionalSlope -> Maybe HexDirection
dsSteepestDescent ds =
  let pairs = map (\d -> (d, dsSlopeIn d ds)) allHexDirections
      negatives = filter (\(_, s) -> s < 0) pairs
  in case negatives of
       [] -> Nothing
       _  -> Just (fst (minimumBy (comparing snd) negatives))
  where
    minimumBy :: (a -> a -> Ordering) -> [a] -> a
    minimumBy _ [x]    = x
    minimumBy f (x:xs) = foldl' (\a b -> if f a b == GT then b else a) x xs
    minimumBy _ []     = error "dsSteepestDescent: impossible empty list"
    {-# INLINE minimumBy #-}