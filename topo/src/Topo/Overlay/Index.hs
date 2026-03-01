{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

-- | Secondary field indices for overlay data.
--
-- An indexed field builds a lookup structure on load and after each
-- simulation tick that modifies the overlay.  Indices are /not/
-- persisted to disk — the @.topolay@ file stores only raw records.
--
-- = Supported index types
--
-- * __Int fields__: @IntMap [Int]@ — value → list of tile indices.
-- * __Float fields__: sorted @Vector (Float, Int)@ — enables range queries.
-- * __Bool fields__: two 'IntSet's (true-set / false-set).
-- * __Text fields__: not indexable (too variable).
module Topo.Overlay.Index
  ( -- * Index types
    OverlayIndex(..)
  , FieldIndex(..)
    -- * Building
  , buildIndex
  , buildIndices
    -- * Querying
  , lookupIntIndex
  , queryFloatRange
  , lookupBoolIndex
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Topo.Overlay
  ( OverlayChunk(..)
  , OverlayRecord(..)
  , OverlayValue(..)
  )
import Topo.Overlay.Schema
  ( OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  )

------------------------------------------------------------------------
-- Index types
------------------------------------------------------------------------

-- | Index for a single field.
data FieldIndex
  = IntFieldIndex    !(IntMap [Int])
  -- ^ Int-value → list of tile indices.
  | FloatFieldIndex  !(Vector (Float, Int))
  -- ^ Sorted by float value, pairs of (value, tileIdx).
  | BoolFieldIndex   !IntSet !IntSet
  -- ^ (true-set, false-set) of tile indices.
  | NoIndex
  -- ^ Field is not indexed (text fields, or not declared indexed).
  deriving (Show)

-- | All field indices for one overlay, keyed by field name.
newtype OverlayIndex = OverlayIndex (Map Text FieldIndex)
  deriving (Show)

------------------------------------------------------------------------
-- Building
------------------------------------------------------------------------

-- | Build indices for all indexed fields in a sparse overlay.
--
-- Non-indexed fields and text fields yield 'NoIndex'.
buildIndices :: OverlaySchema -> IntMap OverlayChunk -> OverlayIndex
buildIndices schema chunks =
  let fields = osFields schema
      idxMap = Map.fromList
        [ (ofdName fd, buildIndex fd fieldPos chunks)
        | (fieldPos, fd) <- zip [0..] fields
        ]
  in  OverlayIndex idxMap

-- | Build an index for a single field across all chunks.
buildIndex :: OverlayFieldDef -> Int -> IntMap OverlayChunk -> FieldIndex
buildIndex fd fieldPos chunks
  | not (ofdIndexed fd) = NoIndex
  | otherwise = case ofdType fd of
      OFInt   -> buildIntIndex fieldPos chunks
      OFFloat -> buildFloatIndex fieldPos chunks
      OFBool  -> buildBoolIndex fieldPos chunks
      OFText  -> NoIndex  -- text fields are not indexable

-- | Build an int field index: value → [tileIdx].
buildIntIndex :: Int -> IntMap OverlayChunk -> FieldIndex
buildIntIndex fieldPos chunks =
  let allPairs =
        [ (val, tileIdx)
        | (_chunkId, OverlayChunk m) <- IntMap.toList chunks
        , (tileIdx, OverlayRecord v) <- IntMap.toList m
        , fieldPos < V.length v
        , let val = case v V.! fieldPos of
                OVInt i -> i
                _       -> 0
        ]
      grouped = IntMap.fromListWith (++) [ (v, [t]) | (v, t) <- allPairs ]
  in  IntFieldIndex grouped

-- | Build a float field index: sorted vector of (value, tileIdx).
buildFloatIndex :: Int -> IntMap OverlayChunk -> FieldIndex
buildFloatIndex fieldPos chunks =
  let allPairs =
        [ (val, tileIdx)
        | (_chunkId, OverlayChunk m) <- IntMap.toList chunks
        , (tileIdx, OverlayRecord v) <- IntMap.toList m
        , fieldPos < V.length v
        , let val = case v V.! fieldPos of
                OVFloat f -> f
                _         -> 0.0
        ]
      sorted = V.fromList (sortBy (comparing fst) allPairs)
  in  FloatFieldIndex sorted

-- | Build a bool field index: (true-set, false-set).
buildBoolIndex :: Int -> IntMap OverlayChunk -> FieldIndex
buildBoolIndex fieldPos chunks =
  let (trues, falses) = foldl go (IntSet.empty, IntSet.empty) allPairs
      allPairs =
        [ (val, tileIdx)
        | (_chunkId, OverlayChunk m) <- IntMap.toList chunks
        , (tileIdx, OverlayRecord v) <- IntMap.toList m
        , fieldPos < V.length v
        , let val = case v V.! fieldPos of
                OVBool b -> b
                _        -> False
        ]
      go (!ts, !fs) (b, t)
        | b         = (IntSet.insert t ts, fs)
        | otherwise = (ts, IntSet.insert t fs)
  in  BoolFieldIndex trues falses

------------------------------------------------------------------------
-- Querying
------------------------------------------------------------------------

-- | Look up all tile indices with a specific int value.
lookupIntIndex :: Int -> FieldIndex -> [Int]
lookupIntIndex val (IntFieldIndex m) = maybe [] id (IntMap.lookup val m)
lookupIntIndex _   _                 = []

-- | Query tile indices whose float value falls within @[lo, hi]@.
--
-- Uses binary search on the sorted vector for O(log n + k) where k
-- is the result count.
queryFloatRange :: Float -> Float -> FieldIndex -> [(Float, Int)]
queryFloatRange lo hi (FloatFieldIndex sorted) =
  let n = V.length sorted
      -- Find first index >= lo via binary search
      loIdx = bsearchLower lo sorted 0 n
      -- Collect while <= hi
      collect i acc
        | i >= n    = reverse acc
        | otherwise =
            let (v, t) = sorted V.! i
            in  if v > hi then reverse acc
                else collect (i + 1) ((v, t) : acc)
  in  collect loIdx []
queryFloatRange _ _ _ = []

-- | Binary search: find the smallest index where value >= target.
bsearchLower :: Float -> Vector (Float, Int) -> Int -> Int -> Int
bsearchLower target vec low high
  | low >= high = low
  | otherwise   =
      let mid = low + (high - low) `div` 2
          (v, _) = vec V.! mid
      in  if v < target
            then bsearchLower target vec (mid + 1) high
            else bsearchLower target vec low mid

-- | Look up all tile indices where the bool field is 'True'.
lookupBoolIndex :: Bool -> FieldIndex -> IntSet
lookupBoolIndex True  (BoolFieldIndex ts _) = ts
lookupBoolIndex False (BoolFieldIndex _ fs) = fs
lookupBoolIndex _     _                     = IntSet.empty
