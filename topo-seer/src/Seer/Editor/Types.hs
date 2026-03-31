{-# LANGUAGE DeriveGeneric #-}

-- | Terrain editor types: tool modes, brush settings, and editor state.
--
-- The editor is a modal overlay activated by the @E@ key.  When active,
-- mouse clicks apply the currently selected 'EditorTool' at the cursor
-- hex using the configured 'BrushSettings'.
module Seer.Editor.Types
  ( -- * Tool modes
    EditorTool(..)
    -- * Brush configuration
  , Falloff(..)
  , BrushSettings(..)
  , defaultBrushSettings
    -- * Editor state
  , EditorState(..)
  , defaultEditorState
  ) where

import GHC.Generics (Generic)

-- | Available terrain editor tools.
data EditorTool
  = ToolRaise
    -- ^ Raise elevation within the brush radius.
  | ToolLower
    -- ^ Lower elevation within the brush radius.
  deriving (Eq, Ord, Show, Generic)

-- | Brush weight falloff function from center to edge.
data Falloff
  = FalloffLinear
    -- ^ Linear falloff: weight = 1 − (distance / radius).
  | FalloffSmooth
    -- ^ Smooth (cosine-based) falloff.
  | FalloffConstant
    -- ^ Constant weight across entire brush radius.
  deriving (Eq, Ord, Show, Generic)

-- | Per-stroke brush configuration.
data BrushSettings = BrushSettings
  { brushRadius   :: !Int
    -- ^ Brush radius in hex rings (0 = single tile).
  , brushStrength  :: !Float
    -- ^ Per-stroke elevation delta (normalised 0–1 units).
  , brushFalloff   :: !Falloff
    -- ^ Weight falloff from center to edge.
  } deriving (Eq, Show, Generic)

-- | Sensible defaults: radius 2, strength 0.02, linear falloff.
defaultBrushSettings :: BrushSettings
defaultBrushSettings = BrushSettings
  { brushRadius   = 2
  , brushStrength  = 0.02
  , brushFalloff   = FalloffLinear
  }

-- | Full editor state carried on 'UiState'.
data EditorState = EditorState
  { editorActive :: !Bool
    -- ^ Whether the editor overlay is currently active.
  , editorTool   :: !EditorTool
    -- ^ Currently selected tool.
  , editorBrush  :: !BrushSettings
    -- ^ Active brush configuration.
  } deriving (Eq, Show, Generic)

-- | Initial editor state: inactive, raise tool, default brush.
defaultEditorState :: EditorState
defaultEditorState = EditorState
  { editorActive = False
  , editorTool   = ToolRaise
  , editorBrush  = defaultBrushSettings
  }
