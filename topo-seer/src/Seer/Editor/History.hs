-- | Undo/redo history for terrain editor operations.
--
-- Each 'EditAction' records the chunk-level before\/after state of
-- every terrain chunk modified by a single brush stroke.  This
-- captures both the user's primary edit and all derived-field
-- recomputations (slope, curvature, relief, terrain form, etc.)
-- so that undo\/redo restores the exact prior state without
-- requiring a re-derive pass.
module Seer.Editor.History
  ( -- * Action recording
    EditAction(..)
    -- * History stack
  , EditHistory(..)
  , emptyHistory
  , pushEdit
  , undoEdit
  , redoEdit
  ) where

import Data.IntMap.Strict (IntMap)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Topo.Types (TerrainChunk)

-- | A single undoable edit, storing the complete before\/after state
-- of every terrain chunk that was modified.
data EditAction = EditAction
  { eaDescription :: !Text
    -- ^ Human-readable label (e.g. @\"Raise\"@, @\"Smooth\"@).
  , eaOldChunks   :: !(IntMap TerrainChunk)
    -- ^ Chunk state /before/ the edit (only changed chunks).
  , eaNewChunks   :: !(IntMap TerrainChunk)
    -- ^ Chunk state /after/ the edit (only changed chunks).
  } deriving (Eq, Show)

-- | Bounded undo\/redo stack.  Newest actions are at the right
-- ('Seq.|>') end of each 'Seq'.
data EditHistory = EditHistory
  { ehUndo    :: !(Seq EditAction)
    -- ^ Actions that can be undone (most recent last).
  , ehRedo    :: !(Seq EditAction)
    -- ^ Actions that can be redone (most recent last).
  , ehMaxSize :: !Int
    -- ^ Maximum number of undo entries to keep.
  } deriving (Eq, Show)

-- | Empty history with the given capacity.
emptyHistory :: Int -> EditHistory
emptyHistory maxSz = EditHistory
  { ehUndo    = Seq.empty
  , ehRedo    = Seq.empty
  , ehMaxSize = max 1 maxSz
  }

-- | Record a new edit.  Pushes onto the undo stack and clears the
-- redo stack (new edits invalidate the redo branch).  Trims the
-- oldest entry when the stack exceeds 'ehMaxSize'.
pushEdit :: EditAction -> EditHistory -> EditHistory
pushEdit action hist =
  let undo' = ehUndo hist Seq.|> action
      trimmed
        | Seq.length undo' > ehMaxSize hist =
            case Seq.viewl undo' of
              Seq.EmptyL   -> undo'      -- impossible
              _ Seq.:< rest -> rest
        | otherwise = undo'
  in hist { ehUndo = trimmed, ehRedo = Seq.empty }

-- | Pop the most recent edit from the undo stack and move it to
-- redo.  Returns 'Nothing' when there is nothing to undo.
undoEdit :: EditHistory -> Maybe (EditAction, EditHistory)
undoEdit hist =
  case Seq.viewr (ehUndo hist) of
    Seq.EmptyR -> Nothing
    rest Seq.:> action ->
      Just (action, hist { ehUndo = rest
                         , ehRedo = ehRedo hist Seq.|> action })

-- | Pop the most recent edit from the redo stack and move it back
-- to undo.  Returns 'Nothing' when there is nothing to redo.
redoEdit :: EditHistory -> Maybe (EditAction, EditHistory)
redoEdit hist =
  case Seq.viewr (ehRedo hist) of
    Seq.EmptyR -> Nothing
    rest Seq.:> action ->
      Just (action, hist { ehUndo = ehUndo hist Seq.|> action
                         , ehRedo = rest })
