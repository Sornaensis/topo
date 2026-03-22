-- | Read-only overlay access for topo plugins.
--
-- This module re-exports the subset of @Topo.Overlay@ and
-- @Topo.Overlay.Schema@ that plugins need to inspect overlay data
-- received from the host.  Plugins should import this module (or
-- the top-level "Topo.Plugin.SDK") rather than depending on
-- @topo@ directly for overlay types.
--
-- === Typical usage
--
-- @
-- import Topo.Plugin.SDK
--
-- myTick :: PluginContext -> IO (Either Text SimulationTickResult)
-- myTick ctx = do
--   case decodeOwnOverlay schema ctx of
--     Left err -> pure (Left err)
--     Right ov -> do
--       let mVal = lookupFieldAt ov 0 42 "population"
--       ...
-- @
module Topo.Plugin.SDK.Overlay
  ( -- * Core types
    Overlay(..)
  , OverlayData(..)
  , OverlayValue(..)
  , OverlayRecord(..)
  , OverlayChunk(..)
  , OverlayProvenance(..)
    -- * Schema types
  , OverlaySchema(..)
  , OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlayStorage(..)
    -- * Schema queries
  , fieldIndex
  , parseOverlaySchema
    -- * Record read access
  , recordField
  , defaultRecord
  , overlayValueToFloat
    -- * Record write access
  , setRecordField
    -- * Chunk access
  , chunkLookup
  , chunkInsert
    -- * Overlay queries
  , overlayName
  , emptyOverlay
    -- * Convenience
  , lookupFieldAt
  , lookupRecordAt
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Topo.Overlay
  ( Overlay(..)
  , OverlayData(..)
  , OverlayValue(..)
  , OverlayRecord(..)
  , OverlayChunk(..)
  , OverlayProvenance(..)
  , chunkInsert
  , chunkLookup
  , defaultRecord
  , emptyOverlay
  , overlayName
  , overlayValueToFloat
  , recordField
  , setRecordField
  )
import Topo.Overlay.Schema
  ( OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  , fieldIndex
  , parseOverlaySchema
  )

-- | Look up a record at a given chunk and tile index.
--
-- Returns 'Nothing' when the chunk or tile is unpopulated (sparse)
-- or when the chunk ID is out of range (dense).
lookupRecordAt
  :: Overlay
  -> Int        -- ^ Chunk ID
  -> Int        -- ^ Tile index within the chunk
  -> Maybe OverlayRecord
lookupRecordAt ov chunkId tileIdx =
  case ovData ov of
    SparseData sm ->
      case IntMap.lookup chunkId sm of
        Nothing -> Nothing
        Just chunk -> chunkLookup tileIdx chunk
    DenseData _ ->
      -- Dense overlays store raw float vectors, not records.
      -- Per-field access is available via lookupFieldAt instead.
      Nothing

-- | Look up a single field value at a given chunk, tile, and field name.
--
-- Combines chunk navigation, tile lookup, schema field-index
-- resolution, and record field access into one call.
--
-- Returns 'Nothing' when:
--
-- * The field name is not declared in the overlay schema
-- * The chunk or tile is unpopulated
-- * The field index is out of range in the record
lookupFieldAt
  :: Overlay
  -> Int        -- ^ Chunk ID
  -> Int        -- ^ Tile index within the chunk
  -> Text       -- ^ Field name
  -> Maybe OverlayValue
lookupFieldAt ov chunkId tileIdx fieldName = do
  fIdx <- fieldIndex (ovSchema ov) fieldName
  case ovData ov of
    SparseData sm -> do
      chunk <- IntMap.lookup chunkId sm
      record <- chunkLookup tileIdx chunk
      recordField fIdx record
    DenseData dm -> do
      chunkVecs <- IntMap.lookup chunkId dm
      lookupDenseField chunkVecs fIdx tileIdx

-- | Read a single float value from a dense chunk's field vectors.
lookupDenseField
  :: V.Vector (U.Vector Float)
  -> Int        -- ^ Field index
  -> Int        -- ^ Tile index
  -> Maybe OverlayValue
lookupDenseField chunkVecs fIdx tileIdx
  | fIdx >= 0 && fIdx < V.length chunkVecs =
      let vec = chunkVecs V.! fIdx
      in if tileIdx >= 0 && tileIdx < U.length vec
           then Just (OVFloat (vec U.! tileIdx))
           else Nothing
  | otherwise = Nothing
