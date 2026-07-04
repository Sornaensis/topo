{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Editor
  ( EditorService(..)
  , EditorToggleRequest(..)
  , EditorStateResponse(..)
  , EditorSetToolRequest(..)
  , EditorSetBrushRequest(..)
  , EditorBrushStrokeRequest(..)
  , EditorBrushLineRequest(..)
  , EditorStrokeQueuedResponse(..)
  , EditorActionQueuedResponse(..)
  , EditorSetBiomeRequest(..)
  , EditorSetFormRequest(..)
  , EditorSetHardnessRequest(..)
  , EditorUndoRequest(..)
  , EditorRedoRequest(..)
  , EditorGetStateRequest(..)
  , editorToggleOperation
  , editorSetToolOperation
  , editorSetBrushOperation
  , editorBrushStrokeOperation
  , editorBrushLineOperation
  , editorSetBiomeOperation
  , editorSetFormOperation
  , editorSetHardnessOperation
  , editorUndoOperation
  , editorRedoOperation
  , editorGetStateOperation
  , editorServiceGroup
  , editorServiceOperationSpecs
  ) where

import Data.Text (Text)

import Seer.Editor.Types (EditorState, EditorTool, Falloff)
import Seer.Service.Types
import Topo.Types (BiomeId, TerrainForm, TileCoord)

data EditorService = EditorService
  { editorToggle :: !(ServiceHandler EditorToggleRequest EditorStateResponse)
  , editorSetTool :: !(ServiceHandler EditorSetToolRequest EditorStateResponse)
  , editorSetBrush :: !(ServiceHandler EditorSetBrushRequest EditorStateResponse)
  , editorBrushStroke :: !(ServiceHandler EditorBrushStrokeRequest EditorStrokeQueuedResponse)
  , editorBrushLine :: !(ServiceHandler EditorBrushLineRequest EditorStrokeQueuedResponse)
  , editorSetBiome :: !(ServiceHandler EditorSetBiomeRequest EditorStateResponse)
  , editorSetForm :: !(ServiceHandler EditorSetFormRequest EditorStateResponse)
  , editorSetHardness :: !(ServiceHandler EditorSetHardnessRequest EditorStateResponse)
  , editorUndo :: !(ServiceHandler EditorUndoRequest EditorActionQueuedResponse)
  , editorRedo :: !(ServiceHandler EditorRedoRequest EditorActionQueuedResponse)
  , editorGetState :: !(ServiceHandler EditorGetStateRequest EditorStateResponse)
  }

newtype EditorToggleRequest = EditorToggleRequest
  { editorToggleActive :: Maybe Bool
  } deriving (Eq, Show)

newtype EditorStateResponse = EditorStateResponse
  { editorState :: EditorState
  } deriving (Eq, Show)

newtype EditorSetToolRequest = EditorSetToolRequest
  { editorSetToolValue :: EditorTool
  } deriving (Eq, Show)

data EditorSetBrushRequest = EditorSetBrushRequest
  { editorSetBrushRadius :: !(Maybe Int)
  , editorSetBrushStrength :: !(Maybe Float)
  , editorSetBrushFalloff :: !(Maybe Falloff)
  , editorSetBrushSmoothPasses :: !(Maybe Int)
  , editorSetBrushNoiseFrequency :: !(Maybe Float)
  , editorSetBrushErodePasses :: !(Maybe Int)
  } deriving (Eq, Show)

newtype EditorBrushStrokeRequest = EditorBrushStrokeRequest
  { editorStrokeHex :: TileCoord
  } deriving (Eq, Show)

data EditorBrushLineRequest = EditorBrushLineRequest
  { editorBrushLineFrom :: !TileCoord
  , editorBrushLineTo :: !TileCoord
  } deriving (Eq, Show)

data EditorStrokeQueuedResponse = EditorStrokeQueuedResponse
  { editorStrokeQueuedStatus :: !Text
  , editorStrokeQueuedCount :: !Int
  } deriving (Eq, Show)

newtype EditorActionQueuedResponse = EditorActionQueuedResponse
  { editorActionQueuedStatus :: Text
  } deriving (Eq, Show)

newtype EditorSetBiomeRequest = EditorSetBiomeRequest
  { editorSetBiomeValue :: BiomeId
  } deriving (Eq, Show)

newtype EditorSetFormRequest = EditorSetFormRequest
  { editorSetFormValue :: TerrainForm
  } deriving (Eq, Show)

newtype EditorSetHardnessRequest = EditorSetHardnessRequest
  { editorSetHardnessValue :: Float
  } deriving (Eq, Show)

data EditorUndoRequest = EditorUndoRequest
  deriving (Eq, Show)

data EditorRedoRequest = EditorRedoRequest
  deriving (Eq, Show)

data EditorGetStateRequest = EditorGetStateRequest
  deriving (Eq, Show)

editorServiceGroup :: ServiceGroupSpec
editorServiceGroup = ServiceGroupSpec "editor" editorServiceOperationSpecs

editorServiceOperationSpecs :: [ServiceOperationSpec]
editorServiceOperationSpecs =
  [ typedServiceOperationSpec editorToggleOperation
  , typedServiceOperationSpec editorSetToolOperation
  , typedServiceOperationSpec editorSetBrushOperation
  , typedServiceOperationSpec editorBrushStrokeOperation
  , typedServiceOperationSpec editorBrushLineOperation
  , typedServiceOperationSpec editorSetBiomeOperation
  , typedServiceOperationSpec editorSetFormOperation
  , typedServiceOperationSpec editorSetHardnessOperation
  , typedServiceOperationSpec editorUndoOperation
  , typedServiceOperationSpec editorRedoOperation
  , typedServiceOperationSpec editorGetStateOperation
  ]

editorToggleOperation :: TypedServiceOperation EditorToggleRequest EditorStateResponse
editorToggleOperation = typedOperation $
  operationSpec "editor.toggle" "editor_toggle" "Enable or disable terrain editor mode."

editorSetToolOperation :: TypedServiceOperation EditorSetToolRequest EditorStateResponse
editorSetToolOperation = typedOperation $
  operationSpec "editor.tool.set" "editor_set_tool" "Select the active editor tool."

editorSetBrushOperation :: TypedServiceOperation EditorSetBrushRequest EditorStateResponse
editorSetBrushOperation = typedOperation $
  operationSpec "editor.brush.set" "editor_set_brush" "Configure brush size/strength options."

editorBrushStrokeOperation :: TypedServiceOperation EditorBrushStrokeRequest EditorStrokeQueuedResponse
editorBrushStrokeOperation = typedOperation $
  operationSpec "editor.brush.stroke" "editor_brush_stroke" "Apply a brush stroke."

editorBrushLineOperation :: TypedServiceOperation EditorBrushLineRequest EditorStrokeQueuedResponse
editorBrushLineOperation = typedOperation $
  operationSpec "editor.brush.line" "editor_brush_line" "Apply a brush line."

editorSetBiomeOperation :: TypedServiceOperation EditorSetBiomeRequest EditorStateResponse
editorSetBiomeOperation = typedOperation $
  operationSpec "editor.biome.set" "editor_set_biome" "Select biome paint value."

editorSetFormOperation :: TypedServiceOperation EditorSetFormRequest EditorStateResponse
editorSetFormOperation = typedOperation $
  operationSpec "editor.form.set" "editor_set_form" "Select terrain-form paint value."

editorSetHardnessOperation :: TypedServiceOperation EditorSetHardnessRequest EditorStateResponse
editorSetHardnessOperation = typedOperation $
  operationSpec "editor.hardness.set" "editor_set_hardness" "Select terrain-hardness paint value."

editorUndoOperation :: TypedServiceOperation EditorUndoRequest EditorActionQueuedResponse
editorUndoOperation = typedOperation $
  operationSpec "editor.undo" "editor_undo" "Undo the latest editor mutation."

editorRedoOperation :: TypedServiceOperation EditorRedoRequest EditorActionQueuedResponse
editorRedoOperation = typedOperation $
  operationSpec "editor.redo" "editor_redo" "Redo an editor mutation."

editorGetStateOperation :: TypedServiceOperation EditorGetStateRequest EditorStateResponse
editorGetStateOperation = typedOperation $
  operationSpec "editor.state" "editor_get_state" "Read terrain editor state."
