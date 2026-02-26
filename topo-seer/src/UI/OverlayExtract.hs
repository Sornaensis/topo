{-# LANGUAGE StrictData #-}

-- | Extract per-chunk overlay field data for visualization.
--
-- Given an overlay name and field index, produces an @IntMap (U.Vector Float)@
-- keyed by chunk ID — one float per hex for color mapping.  Both sparse and
-- dense overlay layouts are supported.
module UI.OverlayExtract
  ( extractOverlayField
  , overlayFieldInfo
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Topo.Overlay
  ( Overlay(..)
  , OverlayData(..)
  , OverlayStore
  , lookupOverlay
  , overlayValueToFloat
  , OverlayChunk(..)
  , OverlayRecord(..)
  , defaultValue
  )
import Topo.Overlay.Schema
  ( OverlayFieldDef(..)
  , OverlayFieldType
  , OverlaySchema(..)
  )

-- | Extract a single field's float data for every chunk in the named overlay.
--
-- Returns @Nothing@ when the overlay or field index is out of range.
-- For dense overlays the data is already a @U.Vector Float@ per chunk.
-- For sparse overlays each populated hex's record is read; unpopulated
-- hexes fall back to the field's schema-declared default.
extractOverlayField
  :: Text
  -- ^ Overlay name.
  -> Int
  -- ^ Field index within the overlay schema.
  -> Int
  -- ^ Chunk tile count (used for sparse → dense expansion).
  -> OverlayStore
  -> Maybe (IntMap (U.Vector Float))
extractOverlayField name fieldIdx chunkTileCount store = do
  overlay <- lookupOverlay name store
  let schema = ovSchema overlay
      fields = osFields schema
  -- Validate field index
  if fieldIdx < 0 || fieldIdx >= length fields
    then Nothing
    else Just $ case ovData overlay of
      DenseData dm ->
        -- Each chunk is Vector (U.Vector Float), indexed by field position.
        IntMap.mapMaybe (extractDenseField fieldIdx) dm
      SparseData sm ->
        -- Build a full-coverage float vector per chunk.
        let defVal = overlayValueToFloat (defaultValue (fields !! fieldIdx))
        in IntMap.map (sparseToDense fieldIdx defVal chunkTileCount) sm

-- | Extract a single field's vector from dense per-chunk data.
extractDenseField :: Int -> Vector (U.Vector Float) -> Maybe (U.Vector Float)
extractDenseField fieldIdx vec
  | fieldIdx >= 0 && fieldIdx < V.length vec = Just (vec V.! fieldIdx)
  | otherwise = Nothing

-- | Expand a sparse overlay chunk to a full-coverage float vector.
sparseToDense :: Int -> Float -> Int -> OverlayChunk -> U.Vector Float
sparseToDense fieldIdx defVal tileCount (OverlayChunk m) =
  U.generate tileCount $ \i ->
    case IntMap.lookup i m of
      Nothing -> defVal
      Just (OverlayRecord vals)
        | fieldIdx < V.length vals -> overlayValueToFloat (vals V.! fieldIdx)
        | otherwise -> defVal

-- | Query overlay field metadata (name and type) for UI display.
--
-- Returns the list of @(fieldName, fieldType)@ pairs for the named overlay.
overlayFieldInfo :: Text -> OverlayStore -> [(Text, OverlayFieldType)]
overlayFieldInfo name store =
  case lookupOverlay name store of
    Nothing -> []
    Just overlay ->
      let fields = osFields (ovSchema overlay)
      in map (\fd -> (ofdName fd, ofdType fd)) fields
