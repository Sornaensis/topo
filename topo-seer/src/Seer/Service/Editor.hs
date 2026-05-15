{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Editor
  ( EditorService(..)
  , editorServiceGroup
  , editorServiceOperationSpecs
  ) where

import Seer.Service.Types

data EditorService = EditorService
  { editorToggle :: !ServiceHandler
  , editorSetTool :: !ServiceHandler
  , editorSetBrush :: !ServiceHandler
  , editorBrushStroke :: !ServiceHandler
  , editorBrushLine :: !ServiceHandler
  , editorSetBiome :: !ServiceHandler
  , editorSetForm :: !ServiceHandler
  , editorSetHardness :: !ServiceHandler
  , editorUndo :: !ServiceHandler
  , editorRedo :: !ServiceHandler
  , editorGetState :: !ServiceHandler
  }

editorServiceGroup :: ServiceGroupSpec
editorServiceGroup = ServiceGroupSpec "editor" editorServiceOperationSpecs

editorServiceOperationSpecs :: [ServiceOperationSpec]
editorServiceOperationSpecs =
  [ operationSpec "editor.toggle" "editor_toggle" "Enable or disable terrain editor mode."
  , operationSpec "editor.tool.set" "editor_set_tool" "Select the active editor tool."
  , operationSpec "editor.brush.set" "editor_set_brush" "Configure brush size/strength options."
  , operationSpec "editor.brush.stroke" "editor_brush_stroke" "Apply a brush stroke."
  , operationSpec "editor.brush.line" "editor_brush_line" "Apply a brush line."
  , operationSpec "editor.biome.set" "editor_set_biome" "Select biome paint value."
  , operationSpec "editor.form.set" "editor_set_form" "Select terrain-form paint value."
  , operationSpec "editor.hardness.set" "editor_set_hardness" "Select terrain-hardness paint value."
  , operationSpec "editor.undo" "editor_undo" "Undo the latest editor mutation."
  , operationSpec "editor.redo" "editor_redo" "Redo an editor mutation."
  , operationSpec "editor.state" "editor_get_state" "Read terrain editor state."
  ]
