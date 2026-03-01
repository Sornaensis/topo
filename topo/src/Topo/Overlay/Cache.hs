{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Overlay chunk cache API surface.
--
-- This module defines a lightweight cache abstraction for viewport-aware
-- overlay chunk access. It intentionally provides only API shape and simple
-- pure state helpers; policy implementations (e.g. LRU) are deferred.
module Topo.Overlay.Cache
  ( OverlayCacheKey(..)
  , OverlayCache(..)
  , noopOverlayCache
  , OverlayCacheState
  , emptyOverlayCacheState
  , cacheLookupState
  , cacheInsertState
  , cacheDeleteState
  , cacheClearState
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Topo.Overlay (OverlayChunk)

-- | Cache key for a single overlay chunk.
data OverlayCacheKey = OverlayCacheKey
  { ockOverlayName :: !Text
  -- ^ Overlay name (schema name).
  , ockChunkId :: !Int
  -- ^ World chunk id.
  } deriving (Eq, Ord, Show)

-- | Cache operations as a record-of-functions.
--
-- Consumers can provide their own implementation strategy (LRU, TTL,
-- viewport-windowed, etc.) by filling this record.
data OverlayCache m = OverlayCache
  { ocLookup :: !(OverlayCacheKey -> m (Maybe OverlayChunk))
  -- ^ Lookup a cached chunk.
  , ocInsert :: !(OverlayCacheKey -> OverlayChunk -> m ())
  -- ^ Insert or replace a cached chunk.
  , ocDelete :: !(OverlayCacheKey -> m ())
  -- ^ Delete a cached chunk key.
  , ocClear :: !(m ())
  -- ^ Clear all cache entries.
  }

-- | A no-op cache implementation.
noopOverlayCache :: Applicative m => OverlayCache m
noopOverlayCache = OverlayCache
  { ocLookup = \_ -> pure Nothing
  , ocInsert = \_ _ -> pure ()
  , ocDelete = \_ -> pure ()
  , ocClear = pure ()
  }

-- | Pure cache state model used for deterministic testing and adapters.
newtype OverlayCacheState = OverlayCacheState
  { unOverlayCacheState :: Map.Map OverlayCacheKey OverlayChunk
  }

-- | Empty cache state.
emptyOverlayCacheState :: OverlayCacheState
emptyOverlayCacheState = OverlayCacheState Map.empty

-- | Lookup in pure cache state.
cacheLookupState :: OverlayCacheKey -> OverlayCacheState -> Maybe OverlayChunk
cacheLookupState key (OverlayCacheState cacheMap) = Map.lookup key cacheMap

-- | Insert into pure cache state.
cacheInsertState :: OverlayCacheKey -> OverlayChunk -> OverlayCacheState -> OverlayCacheState
cacheInsertState key chunk (OverlayCacheState cacheMap) =
  OverlayCacheState (Map.insert key chunk cacheMap)

-- | Delete from pure cache state.
cacheDeleteState :: OverlayCacheKey -> OverlayCacheState -> OverlayCacheState
cacheDeleteState key (OverlayCacheState cacheMap) =
  OverlayCacheState (Map.delete key cacheMap)

-- | Clear pure cache state.
cacheClearState :: OverlayCacheState -> OverlayCacheState
cacheClearState _ = emptyOverlayCacheState
