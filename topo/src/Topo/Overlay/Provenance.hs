{-# LANGUAGE OverloadedStrings #-}

-- | Per-overlay provenance metadata.
module Topo.Overlay.Provenance
  ( OverlayProvenance(..)
  , emptyOverlayProvenance
  ) where

import Data.Text (Text)
import Data.Word (Word32, Word64)

-- | Provenance metadata for a single overlay.
data OverlayProvenance = OverlayProvenance
  { opSeed :: !Word64
  -- ^ Base world seed used to derive deterministic per-tick overlay seeds.
  , opVersion :: !Word32
  -- ^ Monotonic overlay version counter.
  , opSource :: !Text
  -- ^ Overlay source tag (plugin name, builtin tag, etc.).
  } deriving (Eq, Show)

-- | Empty provenance placeholder.
emptyOverlayProvenance :: OverlayProvenance
emptyOverlayProvenance = OverlayProvenance
  { opSeed = 0
  , opVersion = 1
  , opSource = "unknown"
  }