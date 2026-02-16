{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Re-export shim for 'RenderSnapshot'.
--
-- The actor body that previously lived here is dead code (zero production
-- callers).  'RenderSnapshot' now lives in "Actor.SnapshotReceiver" which
-- is the sole runtime producer of snapshots.  This module re-exports the
-- type for backward-compatible imports; it may be deleted once all
-- importers are migrated.
module Actor.Render
  ( RenderSnapshot(..)
  ) where

import Actor.SnapshotReceiver (RenderSnapshot(..))
