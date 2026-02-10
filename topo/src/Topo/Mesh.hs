module Topo.Mesh
  ( Mesh(..)
  , meshPatch
  ) where

import Topo.Sample (sampleHeight)
import Topo.Types
import Topo.World (TerrainWorld)

data Mesh = Mesh
  { meshVertices :: ![Vec3]
  , meshIndices  :: ![Int]
  } deriving (Eq, Show)

meshPatch :: TerrainWorld -> Region -> Mesh
meshPatch world region =
  let (w, h) = regionSize region
      verts = buildVertices region world
      indices = buildIndices w h
  in Mesh
      { meshVertices = verts
      , meshIndices = indices
      }

buildVertices :: Region -> TerrainWorld -> [Vec3]
buildVertices (RegionRect (TileCoord x0 y0) (TileCoord x1 y1)) world =
  let xs = if x0 <= x1 then [x0..x1 + 1] else reverse [x1..x0 + 1]
      ys = if y0 <= y1 then [y0..y1 + 1] else reverse [y1..y0 + 1]
  in [ Vec3 (fromIntegral x) (fromIntegral y) (heightAt x y)
     | y <- ys
     , x <- xs
     ]
  where
    heightAt x y =
      case sampleHeight world (WorldPos (fromIntegral x) (fromIntegral y)) of
        Nothing -> 0
        Just h -> h

buildIndices :: Int -> Int -> [Int]
buildIndices w h =
  let row = w + 1
      idx x y = y * row + x
      cells = [ (x, y) | y <- [0..h - 1], x <- [0..w - 1] ]
  in concatMap (cellTriangles idx) cells

cellTriangles :: (Int -> Int -> Int) -> (Int, Int) -> [Int]
cellTriangles idx (x, y) =
  let i0 = idx x y
      i1 = idx (x + 1) y
      i2 = idx x (y + 1)
      i3 = idx (x + 1) (y + 1)
  in [i0, i2, i1, i1, i2, i3]
