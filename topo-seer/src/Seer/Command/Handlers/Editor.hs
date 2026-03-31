{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Handlers for terrain editor state and stroke commands exposed over IPC.
module Seer.Command.Handlers.Editor
  ( handleEditorToggle
  , handleEditorSetTool
  , handleEditorSetBrush
  , handleEditorBrushStroke
  , handleEditorBrushLine
  , handleEditorSetBiome
  , handleEditorSetForm
  , handleEditorSetHardness
  , handleEditorUndo
  , handleEditorRedo
  , handleEditorGetState
  ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AesonTypes
import Data.Char (isAlphaNum)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word8)

import Actor.Terrain (TerrainReplyOps)
import Actor.UI (getUiSnapshot, setUiEditor, uiEditor)
import Actor.UiActions
  ( UiAction(..)
  , UiActionRequest(..)
  , submitUiAction
  )
import Actor.UiActions.Handles (ActorHandles(..))
import Hyperspace.Actor (replyTo)
import Seer.Command.Context (CommandContext(..))
import Seer.Editor.Types (BrushSettings(..), EditorState(..), EditorTool(..), Falloff(..))
import Topo.Biome.Name (biomeDisplayName)
import Topo.Command.Types (SeerResponse, errResponse, okResponse)
import Topo.Hex (axialToCube, cubeToAxial, hexDistance)
import Topo.Math (clamp01, lerp)
import Topo.Types
  ( BiomeId
  , HexCoord(..)
  , TerrainForm
  , biomeIdFromCode
  , biomeIdToCode
  , terrainFormDisplayName
  , terrainFormFromCode
  , terrainFormToCode
  )

data BrushPatch = BrushPatch
  { bpRadius         :: !(Maybe Int)
  , bpStrength       :: !(Maybe Float)
  , bpFalloff        :: !(Maybe Falloff)
  , bpSmoothPasses   :: !(Maybe Int)
  , bpNoiseFrequency :: !(Maybe Float)
  , bpErodePasses    :: !(Maybe Int)
  }

handleEditorToggle :: CommandContext -> Int -> Value -> IO SeerResponse
handleEditorToggle ctx reqId params =
  case parseOptionalBoolField "active" params of
    Left err -> pure (errResponse reqId err)
    Right mActive -> mutateEditorState ctx reqId $ \editor ->
      Right editor
        { editorActive = maybe (not (editorActive editor)) id mActive
        }

handleEditorSetTool :: CommandContext -> Int -> Value -> IO SeerResponse
handleEditorSetTool ctx reqId params =
  case parseRequiredField "tool" params >>= resolveToolValue of
    Left err -> pure (errResponse reqId err)
    Right tool -> mutateEditorState ctx reqId $ \editor ->
      Right editor
        { editorTool = tool
        , editorFlattenRef = Nothing
        }

handleEditorSetBrush :: CommandContext -> Int -> Value -> IO SeerResponse
handleEditorSetBrush ctx reqId params =
  case parseBrushPatch params of
    Left err -> pure (errResponse reqId err)
    Right patch -> mutateEditorState ctx reqId $ \editor -> Right (applyBrushPatch patch editor)

handleEditorBrushStroke :: CommandContext -> Int -> Value -> IO SeerResponse
handleEditorBrushStroke ctx reqId params =
  case parseHexParams "q" "r" params of
    Left err -> pure (errResponse reqId err)
    Right hex -> do
      enqueueEditorStrokeActions ctx [hex]
      pure $ okResponse reqId $ object
        [ "status" .= ("queued" :: Text)
        , "strokes_queued" .= (1 :: Int)
        ]

handleEditorBrushLine :: CommandContext -> Int -> Value -> IO SeerResponse
handleEditorBrushLine ctx reqId params =
  case parseLineParams params of
    Left err -> pure (errResponse reqId err)
    Right (startHex, endHex) -> do
      let path = axialLine startHex endHex
      enqueueEditorStrokeActions ctx path
      pure $ okResponse reqId $ object
        [ "status" .= ("queued" :: Text)
        , "strokes_queued" .= length path
        ]

handleEditorSetBiome :: CommandContext -> Int -> Value -> IO SeerResponse
handleEditorSetBiome ctx reqId params =
  case parseRequiredField "biome" params >>= resolveBiomeValue of
    Left err -> pure (errResponse reqId err)
    Right biomeId -> mutateEditorState ctx reqId $ \editor ->
      Right editor { editorBiomeId = biomeId }

handleEditorSetForm :: CommandContext -> Int -> Value -> IO SeerResponse
handleEditorSetForm ctx reqId params =
  case parseRequiredField "form" params >>= resolveTerrainFormValue of
    Left err -> pure (errResponse reqId err)
    Right terrainForm -> mutateEditorState ctx reqId $ \editor ->
      Right editor { editorFormOverride = terrainForm }

handleEditorSetHardness :: CommandContext -> Int -> Value -> IO SeerResponse
handleEditorSetHardness ctx reqId params =
  case parseRequiredField "hardness" params >>= valueToFloat "hardness" of
    Left err -> pure (errResponse reqId err)
    Right hardness -> mutateEditorState ctx reqId $ \editor ->
      Right editor { editorHardnessTarget = clamp01 hardness }

handleEditorUndo :: CommandContext -> Int -> Value -> IO SeerResponse
handleEditorUndo ctx reqId _params = do
  enqueueActions ctx [UiActionUndo]
  pure $ okResponse reqId $ object ["status" .= ("queued" :: Text)]

handleEditorRedo :: CommandContext -> Int -> Value -> IO SeerResponse
handleEditorRedo ctx reqId _params = do
  enqueueActions ctx [UiActionRedo]
  pure $ okResponse reqId $ object ["status" .= ("queued" :: Text)]

handleEditorGetState :: CommandContext -> Int -> Value -> IO SeerResponse
handleEditorGetState ctx reqId _params = do
  editor <- currentEditorState ctx
  pure $ okResponse reqId (editorStateValue editor)

mutateEditorState
  :: CommandContext
  -> Int
  -> (EditorState -> Either Text EditorState)
  -> IO SeerResponse
mutateEditorState ctx reqId mutate = do
  let uiHandle = ahUiHandle (ccActorHandles ctx)
  uiSnap <- getUiSnapshot uiHandle
  case mutate (uiEditor uiSnap) of
    Left err -> pure (errResponse reqId err)
    Right editor' -> do
      setUiEditor uiHandle editor'
      updated <- currentEditorState ctx
      pure $ okResponse reqId (editorStateValue updated)

currentEditorState :: CommandContext -> IO EditorState
currentEditorState ctx = uiEditor <$> getUiSnapshot (ahUiHandle (ccActorHandles ctx))

enqueueEditorStrokeActions :: CommandContext -> [(Int, Int)] -> IO ()
enqueueEditorStrokeActions ctx hexes =
  enqueueActions ctx (UiActionClearFlattenRef : map UiActionBrushStroke hexes <> [UiActionClearFlattenRef])

enqueueActions :: CommandContext -> [UiAction] -> IO ()
enqueueActions ctx actions =
  mapM_ (submitUiAction uiActionsHandle . mkRequest) actions
  where
    actorHandles = ccActorHandles ctx
    uiActionsHandle = ccUiActionsHandle ctx
    terrainReply = replyTo @TerrainReplyOps uiActionsHandle
    mkRequest action = UiActionRequest
      { uarAction = action
      , uarActorHandles = actorHandles
      , uarTerrainReplyTo = terrainReply
      }

applyBrushPatch :: BrushPatch -> EditorState -> EditorState
applyBrushPatch patch editor =
  let brush = editorBrush editor
      brush' = brush
        { brushRadius = maybe (brushRadius brush) clampBrushRadius (bpRadius patch)
        , brushStrength = maybe (brushStrength brush) clamp01 (bpStrength patch)
        , brushFalloff = maybe (brushFalloff brush) id (bpFalloff patch)
        }
  in editor
    { editorBrush = brush'
    , editorSmoothPasses = maybe (editorSmoothPasses editor) clampSmoothPasses (bpSmoothPasses patch)
    , editorNoiseFrequency = maybe (editorNoiseFrequency editor) clampNoiseFrequency (bpNoiseFrequency patch)
    , editorErodePasses = maybe (editorErodePasses editor) clampErodePasses (bpErodePasses patch)
    }

editorStateValue :: EditorState -> Value
editorStateValue editor = object
  [ "active" .= editorActive editor
  , "tool" .= toolToText (editorTool editor)
  , "brush" .= object
      [ "radius" .= brushRadius brush
      , "strength" .= brushStrength brush
      , "falloff" .= falloffToText (brushFalloff brush)
      ]
  , "smooth_passes" .= editorSmoothPasses editor
  , "noise_frequency" .= editorNoiseFrequency editor
  , "flatten_reference" .= editorFlattenRef editor
  , "stroke_id" .= editorStrokeId editor
  , "biome" .= object
      [ "name" .= biomeDisplayName (editorBiomeId editor)
      , "code" .= biomeIdToCode (editorBiomeId editor)
      ]
  , "terrain_form" .= object
      [ "name" .= Text.pack (terrainFormDisplayName (editorFormOverride editor))
      , "code" .= terrainFormToCode (editorFormOverride editor)
      ]
  , "hardness_target" .= editorHardnessTarget editor
  , "erode_passes" .= editorErodePasses editor
  ]
  where
    brush = editorBrush editor

parseBrushPatch :: Value -> Either Text BrushPatch
parseBrushPatch params = do
  fields <- expectObject "editor_set_brush" params
  radius <- traverse (valueToInt "radius") (optionalField "radius" fields)
  strength <- traverse (valueToFloat "strength") (optionalField "strength" fields)
  falloff <- traverse resolveFalloffValue (optionalField "falloff" fields)
  smoothPasses <- traverse (valueToInt "smooth_passes") (optionalField "smooth_passes" fields)
  noiseFrequency <- traverse (valueToFloat "noise_frequency") (optionalField "noise_frequency" fields)
  erodePasses <- traverse (valueToInt "erode_passes") (optionalField "erode_passes" fields)
  pure BrushPatch
    { bpRadius = radius
    , bpStrength = strength
    , bpFalloff = falloff
    , bpSmoothPasses = smoothPasses
    , bpNoiseFrequency = noiseFrequency
    , bpErodePasses = erodePasses
    }

parseOptionalBoolField :: Text -> Value -> Either Text (Maybe Bool)
parseOptionalBoolField _ Null = Right Nothing
parseOptionalBoolField field params = do
  fields <- expectObject "editor_toggle" params
  traverse (valueToBool field) (optionalField field fields)

parseHexParams :: Text -> Text -> Value -> Either Text (Int, Int)
parseHexParams qField rField params = do
  fields <- expectObject "editor brush command" params
  q <- requiredField qField fields >>= valueToInt qField
  r <- requiredField rField fields >>= valueToInt rField
  pure (q, r)

parseLineParams :: Value -> Either Text ((Int, Int), (Int, Int))
parseLineParams params = do
  fields <- expectObject "editor_brush_line" params
  fromQ <- requiredField "from_q" fields >>= valueToInt "from_q"
  fromR <- requiredField "from_r" fields >>= valueToInt "from_r"
  toQ <- requiredField "to_q" fields >>= valueToInt "to_q"
  toR <- requiredField "to_r" fields >>= valueToInt "to_r"
  pure ((fromQ, fromR), (toQ, toR))

parseRequiredField :: Text -> Value -> Either Text Value
parseRequiredField field params = do
  fields <- expectObject ("editor parameter object for '" <> field <> "'") params
  requiredField field fields

expectObject :: Text -> Value -> Either Text (KM.KeyMap Value)
expectObject label = \case
  Object fields -> Right fields
  _ -> Left (label <> " must be a JSON object")

optionalField :: Text -> KM.KeyMap Value -> Maybe Value
optionalField field = KM.lookup (Key.fromText field)

requiredField :: Text -> KM.KeyMap Value -> Either Text Value
requiredField field fields =
  maybe (Left ("missing '" <> field <> "' parameter")) Right (optionalField field fields)

valueToBool :: Text -> Value -> Either Text Bool
valueToBool field = \case
  Bool b -> Right b
  _ -> Left ("invalid '" <> field <> "' parameter (expected boolean)")

valueToInt :: Text -> Value -> Either Text Int
valueToInt field value =
  maybe
    (Left ("invalid '" <> field <> "' parameter (expected integer)"))
    Right
    (AesonTypes.parseMaybe Aeson.parseJSON value)

valueToFloat :: Text -> Value -> Either Text Float
valueToFloat field value =
  maybe
    (Left ("invalid '" <> field <> "' parameter (expected number)"))
    Right
    (AesonTypes.parseMaybe Aeson.parseJSON value)

valueToText :: Text -> Value -> Either Text Text
valueToText field = \case
  String txt -> Right txt
  _ -> Left ("invalid '" <> field <> "' parameter (expected string)")

resolveToolValue :: Value -> Either Text EditorTool
resolveToolValue value = do
  toolName <- valueToText "tool" value
  maybe (Left ("unknown editor tool: " <> toolName)) Right (textToTool toolName)

resolveFalloffValue :: Value -> Either Text Falloff
resolveFalloffValue value = do
  falloffName <- valueToText "falloff" value
  maybe (Left ("unknown brush falloff: " <> falloffName)) Right (textToFalloff falloffName)

resolveBiomeValue :: Value -> Either Text BiomeId
resolveBiomeValue = \case
  String biomeName ->
    maybe (Left ("unknown biome: " <> biomeName)) Right (textToBiome biomeName)
  numberValue@(Number _) ->
    case valueToInt "biome" numberValue of
      Left _ -> Left "invalid 'biome' parameter (expected integer code or display name)"
      Right code
        | code < (0 :: Int) -> Left "invalid 'biome' parameter (expected integer code or display name)"
        | code > 65 -> Left ("unknown biome code: " <> tshow code)
        | code == 9 -> Left ("unknown biome code: " <> tshow code)
        | otherwise ->
            case biomeIdFromCode (fromIntegral code) of
              Right biomeId -> Right biomeId
              Left _ -> Left ("unknown biome code: " <> tshow code)
  _ -> Left "invalid 'biome' parameter (expected integer code or display name)"

resolveTerrainFormValue :: Value -> Either Text TerrainForm
resolveTerrainFormValue = \case
  String formName ->
    maybe (Left ("unknown terrain form: " <> formName)) Right (textToTerrainForm formName)
  numberValue@(Number _) ->
    case valueToInt "form" numberValue of
      Left _ -> Left "invalid 'form' parameter (expected integer code or display name)"
      Right code
        | code < (0 :: Int) -> Left "invalid 'form' parameter (expected integer code or display name)"
        | code > 14 -> Left ("unknown terrain form code: " <> tshow code)
        | otherwise ->
            case terrainFormFromCode (fromIntegral code) of
              Right terrainForm -> Right terrainForm
              Left _ -> Left ("unknown terrain form code: " <> tshow code)
  _ -> Left "invalid 'form' parameter (expected integer code or display name)"

toolToText :: EditorTool -> Text
toolToText ToolRaise = "raise"
toolToText ToolLower = "lower"
toolToText ToolSmooth = "smooth"
toolToText ToolFlatten = "flatten"
toolToText ToolNoise = "noise"
toolToText ToolPaintBiome = "paint_biome"
toolToText ToolPaintForm = "paint_form"
toolToText ToolSetHardness = "set_hardness"
toolToText ToolErode = "erode"

textToTool :: Text -> Maybe EditorTool
textToTool txt = case normalizeToken txt of
  "raise" -> Just ToolRaise
  "lower" -> Just ToolLower
  "smooth" -> Just ToolSmooth
  "flatten" -> Just ToolFlatten
  "noise" -> Just ToolNoise
  "paint_biome" -> Just ToolPaintBiome
  "biome" -> Just ToolPaintBiome
  "paint_form" -> Just ToolPaintForm
  "form" -> Just ToolPaintForm
  "set_hardness" -> Just ToolSetHardness
  "hardness" -> Just ToolSetHardness
  "erode" -> Just ToolErode
  _ -> Nothing

falloffToText :: Falloff -> Text
falloffToText FalloffLinear = "linear"
falloffToText FalloffSmooth = "smooth"
falloffToText FalloffConstant = "constant"

textToFalloff :: Text -> Maybe Falloff
textToFalloff txt = case normalizeToken txt of
  "linear" -> Just FalloffLinear
  "smooth" -> Just FalloffSmooth
  "constant" -> Just FalloffConstant
  _ -> Nothing

textToBiome :: Text -> Maybe BiomeId
textToBiome query = fst <$> find matches allBiomes
  where
    needle = normalizeToken query
    matches (_, label) = normalizeToken label == needle

textToTerrainForm :: Text -> Maybe TerrainForm
textToTerrainForm query = fst <$> find matches allTerrainForms
  where
    needle = normalizeToken query
    matches (_, label) = normalizeToken label == needle

allBiomes :: [(BiomeId, Text)]
allBiomes =
  [ (biomeId, biomeDisplayName biomeId)
  | code <- [0 .. 8] <> [10 .. 65 :: Word16]
  , Right biomeId <- [biomeIdFromCode code]
  ]

allTerrainForms :: [(TerrainForm, Text)]
allTerrainForms =
  [ (terrainForm, Text.pack (terrainFormDisplayName terrainForm))
  | code <- [0 .. 14 :: Word8]
  , Right terrainForm <- [terrainFormFromCode code]
  ]

normalizeToken :: Text -> Text
normalizeToken = Text.dropAround (== '_') . collapseUnderscores . Text.map normalizeChar . Text.toLower
  where
    normalizeChar ch
      | isAlphaNum ch = ch
      | otherwise = '_'

collapseUnderscores :: Text -> Text
collapseUnderscores = Text.pack . go False . Text.unpack
  where
    go _ [] = []
    go prevUnderscore (ch:rest)
      | ch == '_' && prevUnderscore = go True rest
      | ch == '_' = ch : go True rest
      | otherwise = ch : go False rest

clampBrushRadius :: Int -> Int
clampBrushRadius = clampInt 0 6

clampSmoothPasses :: Int -> Int
clampSmoothPasses = clampInt 1 5

clampErodePasses :: Int -> Int
clampErodePasses = clampInt 1 20

clampNoiseFrequency :: Float -> Float
clampNoiseFrequency = clampFloat 0.5 4.0

clampInt :: Int -> Int -> Int -> Int
clampInt low high value
  | value < low = low
  | value > high = high
  | otherwise = value

clampFloat :: Float -> Float -> Float -> Float
clampFloat low high value
  | value < low = low
  | value > high = high
  | otherwise = value

tshow :: Show a => a -> Text
tshow = Text.pack . show

axialLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
axialLine start end =
  map axialPair (dedupeConsecutive cubes)
  where
    startHex = HexAxial (fst start) (snd start)
    endHex = HexAxial (fst end) (snd end)
    steps = hexDistance startHex endHex
    startCube = axialToCube startHex
    endCube = axialToCube endHex
    cubes =
      [ cubeRound (lerpCube startCube endCube (fraction i steps))
      | i <- [0 .. steps]
      ]

fraction :: Int -> Int -> Float
fraction _ 0 = 0
fraction i steps = fromIntegral i / fromIntegral steps

lerpCube :: HexCoord -> HexCoord -> Float -> (Float, Float, Float)
lerpCube a b t =
  let HexCube ax ay az = axialToCube a
      HexCube bx by bz = axialToCube b
  in ( lerp (fromIntegral ax) (fromIntegral bx) t
     , lerp (fromIntegral ay) (fromIntegral by) t
     , lerp (fromIntegral az) (fromIntegral bz) t
     )

cubeRound :: (Float, Float, Float) -> HexCoord
cubeRound (x, y, z) =
  let rx = round x
      ry = round y
      rz = round z
      dx = abs (fromIntegral rx - x)
      dy = abs (fromIntegral ry - y)
      dz = abs (fromIntegral rz - z)
      rounded
        | dx > dy && dx > dz = HexCube (-ry - rz) ry rz
        | dy > dz = HexCube rx (-rx - rz) rz
        | otherwise = HexCube rx ry (-rx - ry)
  in cubeToAxial rounded

axialPair :: HexCoord -> (Int, Int)
axialPair (HexAxial q r) = (q, r)
axialPair (HexCube x _ z) = (x, z)

dedupeConsecutive :: Eq a => [a] -> [a]
dedupeConsecutive = \case
  [] -> []
  firstValue:rest -> firstValue : go firstValue rest
  where
    go _ [] = []
    go previous (current:remaining)
      | current == previous = go previous remaining
      | otherwise = current : go current remaining