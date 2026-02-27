{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

-- | Index-aware overlay wrapper that keeps indices in sync with data.
--
-- An 'IndexedOverlay' pairs an 'Overlay' with an optional cached
-- 'OverlayIndex'.  The index is lazily rebuilt on first query after a
-- mutation (dirty-flag strategy).  Dense overlays carry no index —
-- their positional structure already supports direct access.
--
-- = Usage
--
-- @
-- let io  = mkIndexed myOverlay
--     io' = insertSparseRecord 0 42 myRecord io
--     (io'', hits) = queryIntField "population" 100 io'
-- @
--
-- The returned 'IndexedOverlay' from query functions caches the freshly
-- built index; threading it through subsequent queries avoids redundant
-- rebuilds.
module Topo.Overlay.Indexed
  ( -- * Type
    IndexedOverlay(..)
    -- * Construction
  , mkIndexed
  , mkIndexedFresh
    -- * Access
  , getOverlay
  , getSchema
  , getData
    -- * Index management
  , ensureIndex
  , invalidateIndex
    -- * Sparse data mutations (invalidate index)
  , insertSparseRecord
  , deleteSparseRecord
  , updateSparseChunk
    -- * Queries (rebuild index on demand)
  , queryIntField
  , queryFloatRange
  , queryBoolField
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Topo.Overlay
  ( Overlay(..)
  , OverlayData(..)
  , OverlayChunk(..)
  , OverlayRecord
  , chunkInsert
  , chunkDelete
  )
import Topo.Overlay.Index
  ( OverlayIndex(..)
  , FieldIndex
  , buildIndices
  , lookupIntIndex
  , lookupBoolIndex
  )
import qualified Topo.Overlay.Index as Idx
import Topo.Overlay.Schema (OverlaySchema)

------------------------------------------------------------------------
-- Type
------------------------------------------------------------------------

-- | An overlay paired with a lazily-maintained index.
--
-- When 'ioIndex' is 'Nothing' the index is /dirty/ — the next query
-- will trigger a full rebuild via 'buildIndices'.  Mutations always
-- set 'ioIndex' to 'Nothing'.
data IndexedOverlay = IndexedOverlay
  { ioOverlay :: !Overlay
  , ioIndex   :: !(Maybe OverlayIndex)
    -- ^ 'Nothing' means dirty (needs rebuild on next query).
  } deriving (Show)

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | Wrap an overlay with a dirty (unbuilt) index.
mkIndexed :: Overlay -> IndexedOverlay
mkIndexed ov = IndexedOverlay ov Nothing

-- | Wrap an overlay and immediately build its index.
--
-- Useful when the overlay has just been loaded or migrated and queries
-- are expected right away.
mkIndexedFresh :: Overlay -> IndexedOverlay
mkIndexedFresh ov = IndexedOverlay ov (Just (buildOverlayIndex ov))

------------------------------------------------------------------------
-- Access
------------------------------------------------------------------------

-- | Extract the underlying 'Overlay'.
getOverlay :: IndexedOverlay -> Overlay
getOverlay = ioOverlay

-- | Extract the schema from the wrapped overlay.
getSchema :: IndexedOverlay -> OverlaySchema
getSchema = ovSchema . ioOverlay

-- | Extract the data payload from the wrapped overlay.
getData :: IndexedOverlay -> OverlayData
getData = ovData . ioOverlay

------------------------------------------------------------------------
-- Index management
------------------------------------------------------------------------

-- | Ensure the index is built, returning both the (possibly updated)
-- 'IndexedOverlay' and the 'OverlayIndex'.
--
-- If the index is already cached this is O(1).  Otherwise a full
-- rebuild is triggered.
ensureIndex :: IndexedOverlay -> (IndexedOverlay, OverlayIndex)
ensureIndex io@(IndexedOverlay _ (Just idx)) = (io, idx)
ensureIndex (IndexedOverlay ov Nothing) =
  let !idx = buildOverlayIndex ov
  in  (IndexedOverlay ov (Just idx), idx)

-- | Mark the cached index as dirty, forcing a rebuild on next query.
invalidateIndex :: IndexedOverlay -> IndexedOverlay
invalidateIndex (IndexedOverlay ov _) = IndexedOverlay ov Nothing

------------------------------------------------------------------------
-- Sparse data mutations
------------------------------------------------------------------------

-- | Insert or replace a record in a sparse overlay chunk.
--
-- No-op when the overlay uses dense storage.
-- Invalidates the cached index.
insertSparseRecord
  :: Int              -- ^ Chunk ID
  -> Int              -- ^ Tile index within chunk
  -> OverlayRecord
  -> IndexedOverlay
  -> IndexedOverlay
insertSparseRecord chunkId tileIdx rec (IndexedOverlay ov _) =
  case ovData ov of
    SparseData chunks ->
      let chunk  = IntMap.findWithDefault (OverlayChunk IntMap.empty) chunkId chunks
          chunk' = chunkInsert tileIdx rec chunk
          ov'    = ov { ovData = SparseData (IntMap.insert chunkId chunk' chunks) }
      in  IndexedOverlay ov' Nothing
    DenseData _ ->
      -- Dense overlays: mutation through this API is not supported;
      -- return unchanged (index was already Nothing or irrelevant).
      IndexedOverlay ov Nothing

-- | Delete a record from a sparse overlay chunk.
--
-- No-op when the overlay uses dense storage.
-- Invalidates the cached index.
deleteSparseRecord
  :: Int              -- ^ Chunk ID
  -> Int              -- ^ Tile index within chunk
  -> IndexedOverlay
  -> IndexedOverlay
deleteSparseRecord chunkId tileIdx (IndexedOverlay ov _) =
  case ovData ov of
    SparseData chunks ->
      case IntMap.lookup chunkId chunks of
        Nothing -> IndexedOverlay ov Nothing
        Just chunk ->
          let chunk' = chunkDelete tileIdx chunk
              ov'    = ov { ovData = SparseData (IntMap.insert chunkId chunk' chunks) }
          in  IndexedOverlay ov' Nothing
    DenseData _ ->
      IndexedOverlay ov Nothing

-- | Replace an entire chunk in a sparse overlay.
--
-- No-op when the overlay uses dense storage.
-- Invalidates the cached index.
updateSparseChunk
  :: Int              -- ^ Chunk ID
  -> OverlayChunk
  -> IndexedOverlay
  -> IndexedOverlay
updateSparseChunk chunkId chunk (IndexedOverlay ov _) =
  case ovData ov of
    SparseData chunks ->
      let ov' = ov { ovData = SparseData (IntMap.insert chunkId chunk chunks) }
      in  IndexedOverlay ov' Nothing
    DenseData _ ->
      IndexedOverlay ov Nothing

------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------

-- | Query an int field index for all tile indices matching a value.
--
-- Rebuilds the index if dirty, returning the updated 'IndexedOverlay'
-- so callers can thread the cached index through subsequent queries.
queryIntField
  :: Text             -- ^ Field name
  -> Int              -- ^ Value to look up
  -> IndexedOverlay
  -> (IndexedOverlay, [Int])
queryIntField fieldName val io =
  let (io', OverlayIndex m) = ensureIndex io
      hits = case Map.lookup fieldName m of
        Just fi -> lookupIntIndex val fi
        Nothing -> []
  in  (io', hits)

-- | Query a float field index for tile indices within @[lo, hi]@.
--
-- Rebuilds the index if dirty.
queryFloatRange
  :: Text             -- ^ Field name
  -> Float            -- ^ Lower bound (inclusive)
  -> Float            -- ^ Upper bound (inclusive)
  -> IndexedOverlay
  -> (IndexedOverlay, [(Float, Int)])
queryFloatRange fieldName lo hi io =
  let (io', OverlayIndex m) = ensureIndex io
      hits = case Map.lookup fieldName m of
        Just fi -> Idx.queryFloatRange lo hi fi
        Nothing -> []
  in  (io', hits)

-- | Query a bool field index for tile indices where the field is 'True'
-- or 'False'.
--
-- Rebuilds the index if dirty.
queryBoolField
  :: Text             -- ^ Field name
  -> Bool             -- ^ Value to match
  -> IndexedOverlay
  -> (IndexedOverlay, IntSet)
queryBoolField fieldName val io =
  let (io', OverlayIndex m) = ensureIndex io
      hits = case Map.lookup fieldName m of
        Just fi -> lookupBoolIndex val fi
        Nothing -> mempty
  in  (io', hits)

------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------

-- | Build an 'OverlayIndex' from an 'Overlay'.
--
-- * Sparse overlays: delegates to 'buildIndices'.
-- * Dense overlays: returns an empty index (no indexing for dense).
buildOverlayIndex :: Overlay -> OverlayIndex
buildOverlayIndex ov =
  case ovData ov of
    SparseData chunks -> buildIndices (ovSchema ov) chunks
    DenseData  _      -> OverlayIndex Map.empty
