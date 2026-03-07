{-# LANGUAGE OverloadedStrings #-}

module Spec.Support.OverlayFixtures
  ( mkSparseFloatOverlay
  ) where

import Data.Aeson (Value(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as V

import Topo.Overlay
  ( Overlay(..)
  , OverlayChunk(..)
  , OverlayData(..)
  , OverlayProvenance
  , OverlayRecord(..)
  , OverlayValue(..)
  )
import Topo.Overlay.Schema
  ( OverlayDeps(..)
  , OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  )

-- | Creates a sparse float overlay containing one chunk and one record.
mkSparseFloatOverlay :: Text.Text -> Text.Text -> Float -> OverlayProvenance -> Overlay
mkSparseFloatOverlay name description value provenance =
  let schema = OverlaySchema
        { osName = name
        , osVersion = "1.0.0"
        , osDescription = description
        , osFields = [OverlayFieldDef "value" OFFloat (Number 0) False Nothing]
        , osStorage = StorageSparse
        , osDependencies = OverlayDeps { odTerrain = True, odOverlays = [] }
        , osFieldIndex = Map.fromList [("value", 0)]
        }
      rec = OverlayRecord (V.fromList [OVFloat value])
      chunk = OverlayChunk (IntMap.singleton 0 rec)
  in Overlay
      { ovSchema = schema
      , ovData = SparseData (IntMap.singleton 0 chunk)
      , ovProvenance = provenance
      }
