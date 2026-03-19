{-# LANGUAGE DataKinds #-}

-- | Re-export shim for 'RenderSnapshot'.
--
-- 'RenderSnapshot' now lives in "Actor.SnapshotReceiver" which provides
-- the lock-free snapshot infrastructure.  This module re-exports the
-- type for backward-compatible imports; it may be deleted once all
-- importers are migrated.
module Actor.Render
  ( RenderSnapshot(..)
  ) where

import Actor.SnapshotReceiver (RenderSnapshot(..))
