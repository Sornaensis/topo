{-# LANGUAGE OverloadedStrings #-}

-- | IPC handlers for terrain queries and data export:
-- @find_hexes@, @export_terrain_data@.
module Seer.Command.Handlers.Query
  ( handleFindHexes
  , handleExportTerrainData
  ) where

import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import qualified Data.Vector.Unboxed as U

import Actor.Data (TerrainSnapshot(..))
import Actor.SnapshotReceiver (readTerrainSnapshot)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Topo.Biome.Name (biomeDisplayName)
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Types
  ( TerrainChunk(..)
  , ClimateChunk(..)
  , VegetationChunk(..)
  , RiverChunk(..)
  , BiomeId
  , TerrainForm
  , biomeIdToCode
  , biomeIdFromCode
  , terrainFormToCode
  , terrainFormFromCode
  , terrainFormDisplayName
  )

-- | Handle @find_hexes@ — search tiles matching filter predicates.
--
-- Params:
-- @{ "filters": [{ "field": "elevation", "op": "gt", "value": 0.5 }],
--    "limit"?: int }@
--
-- All filters are ANDed.  Default limit: 50.
handleFindHexes :: CommandContext -> Int -> Value -> IO SeerResponse
handleFindHexes ctx reqId params = do
  case Aeson.parseMaybe parseFindHexes params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'filters' parameter"
    Just (filters, limit) -> do
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
      let chunkSize = tsChunkSize snap
      if chunkSize <= 0
        then pure $ okResponse reqId $ object
          [ "matches" .= ([] :: [Value])
          , "count"   .= (0 :: Int)
          ]
        else do
          let tileCount = chunkSize * chunkSize
              results = findMatches snap tileCount filters limit
          pure $ okResponse reqId $ object
            [ "matches" .= results
            , "count"   .= length results
            ]

-- | Handle @export_terrain_data@ — export terrain field data as JSON arrays.
--
-- Params:
-- @{ "chunks"?: [int], "fields"?: ["elevation", "moisture", ...] }@
--
-- Default: all chunks, elevation only.
handleExportTerrainData :: CommandContext -> Int -> Value -> IO SeerResponse
handleExportTerrainData ctx reqId params = do
  snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  let chunkSize = tsChunkSize snap
  if chunkSize <= 0
    then pure $ errResponse reqId "no terrain generated"
    else do
      let opts = maybe defaultExportOpts id (Aeson.parseMaybe parseExportOpts params)
          chunkIds = case eoChunks opts of
            Just ids -> ids
            Nothing  -> IntMap.keys (tsTerrainChunks snap)
          fields = case eoFields opts of
            Just fs -> fs
            Nothing -> ["elevation"]
          chunkEntries = map (exportChunk snap fields) chunkIds
      pure $ okResponse reqId $ object
        [ "chunk_count" .= length chunkEntries
        , "fields"      .= fields
        , "data"        .= object
            [ Key.fromText (Text.pack (show cid)) .= val | (cid, val) <- chunkEntries ]
        ]

-- =====================================================================
-- find_hexes implementation
-- =====================================================================

data HexFilter = HexFilter
  { hfField :: !Text
  , hfOp    :: !FilterOp
  , hfValue :: !Value
  }

data FilterOp = OpEq | OpNeq | OpGt | OpGte | OpLt | OpLte

findMatches :: TerrainSnapshot -> Int -> [HexFilter] -> Int -> [Value]
findMatches snap tileCount filters limit =
  take limit $ concatMap (matchChunk snap tileCount filters) chunkPairs
  where
    chunkPairs = IntMap.toList (tsTerrainChunks snap)

matchChunk :: TerrainSnapshot -> Int -> [HexFilter] -> (Int, TerrainChunk) -> [Value]
matchChunk snap tileCount filters (cid, tc) =
  [ buildMatchResult cid idx tc snap
  | idx <- [0 .. tileCount - 1]
  , all (\f -> evalFilter f cid idx tc snap) filters
  ]

evalFilter :: HexFilter -> Int -> Int -> TerrainChunk -> TerrainSnapshot -> Bool
evalFilter hf _cid idx tc snap = case hfField hf of
  "elevation"    -> compareFloat (hfOp hf) (hfValue hf) (safeIdx (tcElevation tc) idx)
  "moisture"     -> compareFloat (hfOp hf) (hfValue hf) (safeIdx (tcMoisture tc) idx)
  "hardness"     -> compareFloat (hfOp hf) (hfValue hf) (safeIdx (tcHardness tc) idx)
  "fertility"    -> compareFloat (hfOp hf) (hfValue hf) (safeIdx (tcFertility tc) idx)
  "roughness"    -> compareFloat (hfOp hf) (hfValue hf) (safeIdx (tcRoughness tc) idx)
  "biome"        -> compareText  (hfOp hf) (hfValue hf) (biomeAtIdx tc idx)
  "terrain_form" -> compareText  (hfOp hf) (hfValue hf) (formAtIdx tc idx)
  "temperature"  -> compareFloat (hfOp hf) (hfValue hf) (climateField _cid ccTempAvg snap idx)
  "precipitation" -> compareFloat (hfOp hf) (hfValue hf) (climateField _cid ccPrecipAvg snap idx)
  "vegetation_cover" -> compareFloat (hfOp hf) (hfValue hf) (vegField _cid vegCover snap idx)
  "river_discharge"  -> compareFloat (hfOp hf) (hfValue hf) (riverField _cid rcDischarge snap idx)
  "plate_id"     -> compareW16   (hfOp hf) (hfValue hf) (safeIdxW16 (tcPlateId tc) idx)
  _              -> False

climateField :: Int -> (ClimateChunk -> U.Vector Float) -> TerrainSnapshot -> Int -> Maybe Float
climateField cid accessor snap idx =
  case IntMap.lookup cid (tsClimateChunks snap) of
    Nothing -> Nothing
    Just cc -> safeIdx (accessor cc) idx

vegField :: Int -> (VegetationChunk -> U.Vector Float) -> TerrainSnapshot -> Int -> Maybe Float
vegField cid accessor snap idx =
  case IntMap.lookup cid (tsVegetationChunks snap) of
    Nothing -> Nothing
    Just vc -> safeIdx (accessor vc) idx

riverField :: Int -> (RiverChunk -> U.Vector Float) -> TerrainSnapshot -> Int -> Maybe Float
riverField cid accessor snap idx =
  case IntMap.lookup cid (tsRiverChunks snap) of
    Nothing -> Nothing
    Just rc -> safeIdx (accessor rc) idx

compareFloat :: FilterOp -> Value -> Maybe Float -> Bool
compareFloat _ _ Nothing = False
compareFloat op (Number n) (Just v) =
  let target = realToFrac n :: Float
  in case op of
    OpEq  -> v == target
    OpNeq -> v /= target
    OpGt  -> v > target
    OpGte -> v >= target
    OpLt  -> v < target
    OpLte -> v <= target
compareFloat _ _ _ = False

compareText :: FilterOp -> Value -> Maybe Text -> Bool
compareText _ _ Nothing = False
compareText op (String target) (Just v) = case op of
  OpEq  -> v == target
  OpNeq -> v /= target
  _     -> False  -- ordering on text doesn't make sense here
compareText _ _ _ = False

compareW16 :: FilterOp -> Value -> Maybe Word16 -> Bool
compareW16 _ _ Nothing = False
compareW16 op (Number n) (Just v) =
  let target = round (realToFrac n :: Double) :: Word16
  in case op of
    OpEq  -> v == target
    OpNeq -> v /= target
    OpGt  -> v > target
    OpGte -> v >= target
    OpLt  -> v < target
    OpLte -> v <= target
compareW16 _ _ _ = False

biomeAtIdx :: TerrainChunk -> Int -> Maybe Text
biomeAtIdx tc idx
  | idx >= 0 && idx < U.length (tcFlags tc) =
      Just (biomeDisplayName (tcFlags tc U.! idx))
  | otherwise = Nothing

formAtIdx :: TerrainChunk -> Int -> Maybe Text
formAtIdx tc idx
  | idx >= 0 && idx < U.length (tcTerrainForm tc) =
      Just (Text.pack (terrainFormDisplayName (tcTerrainForm tc U.! idx)))
  | otherwise = Nothing

safeIdx :: U.Vector Float -> Int -> Maybe Float
safeIdx v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

safeIdxW16 :: U.Vector Word16 -> Int -> Maybe Word16
safeIdxW16 v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

buildMatchResult :: Int -> Int -> TerrainChunk -> TerrainSnapshot -> Value
buildMatchResult cid idx tc _snap = object
  [ "chunk"     .= cid
  , "tile"      .= idx
  , "elevation" .= safeIdx (tcElevation tc) idx
  , "biome"     .= biomeAtIdx tc idx
  , "terrain_form" .= formAtIdx tc idx
  ]

-- =====================================================================
-- export_terrain_data implementation
-- =====================================================================

data ExportOpts = ExportOpts
  { eoChunks :: !(Maybe [Int])
  , eoFields :: !(Maybe [Text])
  }

defaultExportOpts :: ExportOpts
defaultExportOpts = ExportOpts Nothing Nothing

exportChunk :: TerrainSnapshot -> [Text] -> Int -> (Int, Value)
exportChunk snap fields cid =
  case IntMap.lookup cid (tsTerrainChunks snap) of
    Nothing -> (cid, Null)
    Just tc ->
      let fieldEntries = map (exportField cid tc snap) fields
      in (cid, object fieldEntries)

exportField :: Int -> TerrainChunk -> TerrainSnapshot -> Text -> (Aeson.Key, Value)
exportField cid tc snap field = (Key.fromText field, val)
  where
    val = case field of
      "elevation"    -> Aeson.toJSON (U.toList (tcElevation tc))
      "moisture"     -> Aeson.toJSON (U.toList (tcMoisture tc))
      "hardness"     -> Aeson.toJSON (U.toList (tcHardness tc))
      "fertility"    -> Aeson.toJSON (U.toList (tcFertility tc))
      "curvature"    -> Aeson.toJSON (U.toList (tcCurvature tc))
      "roughness"    -> Aeson.toJSON (U.toList (tcRoughness tc))
      "soil_depth"   -> Aeson.toJSON (U.toList (tcSoilDepth tc))
      "biome"        -> Aeson.toJSON [ biomeDisplayName b | b <- U.toList (tcFlags tc) ]
      "terrain_form" -> Aeson.toJSON [ Text.pack (terrainFormDisplayName f) | f <- U.toList (tcTerrainForm tc) ]
      "temperature"  -> case IntMap.lookup cid (tsClimateChunks snap) of
                          Nothing -> Null
                          Just cc -> Aeson.toJSON (U.toList (ccTempAvg cc))
      "precipitation" -> case IntMap.lookup cid (tsClimateChunks snap) of
                           Nothing -> Null
                           Just cc -> Aeson.toJSON (U.toList (ccPrecipAvg cc))
      "vegetation_cover" -> case IntMap.lookup cid (tsVegetationChunks snap) of
                              Nothing -> Null
                              Just vc -> Aeson.toJSON (U.toList (vegCover vc))
      "river_discharge" -> case IntMap.lookup cid (tsRiverChunks snap) of
                             Nothing -> Null
                             Just rc -> Aeson.toJSON (U.toList (rcDischarge rc))
      _              -> Null

-- =====================================================================
-- Parsing helpers
-- =====================================================================

parseFindHexes :: Value -> Aeson.Parser ([HexFilter], Int)
parseFindHexes = Aeson.withObject "find_hexes" $ \o -> do
  filters <- o .: "filters"
  limit   <- o .:? "limit"
  fs <- mapM parseFilter filters
  pure (fs, maybe 50 (max 1 . min 1000) limit)

parseFilter :: Value -> Aeson.Parser HexFilter
parseFilter = Aeson.withObject "filter" $ \o -> do
  field <- o .: "field"
  opStr <- o .: "op"
  value <- o .: "value"
  op <- case (opStr :: Text) of
    "eq"  -> pure OpEq
    "neq" -> pure OpNeq
    "gt"  -> pure OpGt
    "gte" -> pure OpGte
    "lt"  -> pure OpLt
    "lte" -> pure OpLte
    _     -> fail ("unknown filter op: " <> Text.unpack opStr)
  pure (HexFilter field op value)

parseExportOpts :: Value -> Aeson.Parser ExportOpts
parseExportOpts = Aeson.withObject "export_terrain_data" $ \o ->
  ExportOpts <$> o .:? "chunks" <*> o .:? "fields"
