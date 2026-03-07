{-# LANGUAGE OverloadedStrings #-}

module Spec.Support.OverlayFixtures
  ( sparseFloatOverlaySchema
  , denseFloatOverlaySchema
  , mkSparseFloatOverlay
  , mkDenseFloatOverlay
  , overlayProvenanceFixture
  ) where

import Data.Aeson (Value(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Topo.Overlay
  ( Overlay(..)
  , OverlayChunk(..)
  , OverlayData(..)
  , OverlayProvenance(..)
  , OverlayRecord(..)
  , OverlayValue(..)
  , emptyOverlayProvenance
  )
import Topo.Overlay.Schema
  ( OverlayDeps(..)
  , OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  )

-- | Builds a sparse float overlay schema with a single @value@ field.
sparseFloatOverlaySchema :: Text.Text -> Text.Text -> OverlaySchema
sparseFloatOverlaySchema name description = OverlaySchema
  { osName = name
  , osVersion = "1.0.0"
  , osDescription = description
  , osFields = [OverlayFieldDef "value" OFFloat (Number 0) False Nothing]
  , osStorage = StorageSparse
  , osDependencies = OverlayDeps { odTerrain = True, odOverlays = [] }
  , osFieldIndex = Map.fromList [("value", 0)]
  }

-- | Builds a dense float overlay schema with a single @value@ field.
denseFloatOverlaySchema :: Text.Text -> Text.Text -> OverlaySchema
denseFloatOverlaySchema name description = OverlaySchema
  { osName = name
  , osVersion = "1.0.0"
  , osDescription = description
  , osFields = [OverlayFieldDef "value" OFFloat (Number 0) False Nothing]
  , osStorage = StorageDense
  , osDependencies = OverlayDeps { odTerrain = True, odOverlays = [] }
  , osFieldIndex = Map.fromList [("value", 0)]
  }

-- | Creates a sparse float overlay containing one chunk and one record.
mkSparseFloatOverlay :: Text.Text -> Text.Text -> Float -> OverlayProvenance -> Overlay
mkSparseFloatOverlay name description value provenance =
  let schema = sparseFloatOverlaySchema name description
      rec = OverlayRecord (V.fromList [OVFloat value])
      chunk = OverlayChunk (IntMap.singleton 0 rec)
  in Overlay
      { ovSchema = schema
      , ovData = SparseData (IntMap.singleton 0 chunk)
      , ovProvenance = provenance
      }

-- | Creates a dense float overlay containing one chunk and one dense vector.
mkDenseFloatOverlay :: Text.Text -> Text.Text -> Float -> OverlayProvenance -> Overlay
mkDenseFloatOverlay name description value provenance =
  let schema = denseFloatOverlaySchema name description
      denseValues = V.fromList [U.singleton value]
  in Overlay
      { ovSchema = schema
      , ovData = DenseData (IntMap.singleton 0 denseValues)
      , ovProvenance = provenance
      }

-- | Deterministic provenance fixture used by persistence tests.
overlayProvenanceFixture :: OverlayProvenance
overlayProvenanceFixture =
  emptyOverlayProvenance
    { opSeed = 42
    , opVersion = 7
    , opSource = "bundle-test"
    }