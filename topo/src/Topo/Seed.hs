-- | Deterministic seed derivation helpers.
module Topo.Seed
  ( deriveOverlaySeed
  ) where

import Data.Word (Word64)
import Topo.Noise (hashSeed)

-- | Derive a deterministic per-tick overlay seed from base world seed.
deriveOverlaySeed :: Word64 -> Word64 -> Word64
deriveOverlaySeed = hashSeed