{-# LANGUAGE OverloadedStrings #-}

-- | Terrain payload codecs for plugin RPC.
--
-- This module owns the payload contract for terrain/chunk transport,
-- including base64 chunk encoding and decode/apply helpers.
module Topo.Plugin.RPC.Payload
  ( terrainWorldToPayload
  , decodeTerrainWritesValue
  , applyGeneratorTerrainValue
  , encodeBase64Text
  , decodeBase64Text
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Text.Read (readMaybe)

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.=), object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

import Topo.Export
  ( ExportError(..)
  , decodeClimateChunk
  , decodeTerrainChunk
  , decodeVegetationChunk
  , encodeClimateChunk
  , encodeTerrainChunk
  , encodeVegetationChunk
  )
import Topo.Simulation
  ( TerrainWrites(..)
  , applyTerrainWrites
  , emptyTerrainWrites
  )
import qualified Topo.Types
import qualified Topo.World

-- | Encode a terrain world into the RPC terrain payload object.
terrainWorldToPayload :: Topo.World.TerrainWorld -> Either Text Value
terrainWorldToPayload world = do
  terrainObj <- encodeChunkMap
    (Topo.World.twTerrain world)
    (encodeTerrainChunk (Topo.World.twConfig world))
  climateObj <- encodeChunkMap
    (Topo.World.twClimate world)
    (encodeClimateChunk (Topo.World.twConfig world))
  vegetationObj <- encodeChunkMap
    (Topo.World.twVegetation world)
    (encodeVegetationChunk (Topo.World.twConfig world))
  Right $ object
    [ "chunk_count" .= IntMap.size (Topo.World.twTerrain world)
    , "climate_count" .= IntMap.size (Topo.World.twClimate world)
    , "river_count" .= IntMap.size (Topo.World.twRivers world)
    , "vegetation_count" .= IntMap.size (Topo.World.twVegetation world)
    , "chunk_size" .= Topo.Types.wcChunkSize (Topo.World.twConfig world)
    , "encoding" .= ("base64" :: Text)
    , "terrain" .= Object terrainObj
    , "climate" .= Object climateObj
    , "vegetation" .= Object vegetationObj
    ]

-- | Decode a simulation @terrain_writes@ payload into structured writes.
decodeTerrainWritesValue :: Maybe Value -> Either Text TerrainWrites
decodeTerrainWritesValue = decodeTerrainWrites

-- | Apply a generator terrain payload to a world by decoding and merging
-- chunk updates.
applyGeneratorTerrainValue :: Topo.World.TerrainWorld -> Value -> Either Text Topo.World.TerrainWorld
applyGeneratorTerrainValue world Null = Right world
applyGeneratorTerrainValue world (Object obj)
  | KM.null obj = Right world
  | hasOnlySummaryKeys obj = Right world
  | otherwise = applyTerrainPayload world obj
applyGeneratorTerrainValue _ _ = Left "generator terrain payload must be an object or null"

decodeTerrainWrites :: Maybe Value -> Either Text TerrainWrites
decodeTerrainWrites Nothing = Right emptyTerrainWrites
decodeTerrainWrites (Just Null) = Right emptyTerrainWrites
decodeTerrainWrites (Just (Object obj))
  | KM.null obj = Right emptyTerrainWrites
  | hasOnlySummaryKeys obj = Right emptyTerrainWrites
  | otherwise = terrainWritesFromPayload obj
decodeTerrainWrites (Just _) = Left "terrain_writes payload must be an object"

hasOnlySummaryKeys :: KM.KeyMap Value -> Bool
hasOnlySummaryKeys keyMap = all (`elem` allowedKeys) (map Key.toText (KM.keys keyMap))
  where
    allowedKeys =
      [ "chunk_count"
      , "climate_count"
      , "river_count"
      , "vegetation_count"
      , "chunk_size"
      , "encoding"
      ]

encodeChunkMap
  :: IntMap.IntMap a
  -> (a -> Either ExportError BS.ByteString)
  -> Either Text (KM.KeyMap Value)
encodeChunkMap chunks encodeChunk = do
  pairs <- traverse encodeOne (IntMap.toList chunks)
  Right (KM.fromList pairs)
  where
    encodeOne (chunkId, chunk) = do
      encoded <- firstExportError (encodeChunk chunk)
      Right
        ( Key.fromText (Text.pack (show chunkId))
        , Aeson.String (encodeBase64Text encoded)
        )

firstExportError :: Either ExportError a -> Either Text a
firstExportError (Right value) = Right value
firstExportError (Left err) = Left (renderExportError err)

renderExportError :: ExportError -> Text
renderExportError err =
  case err of
    ExportLengthMismatch label expected actual ->
      "length mismatch for " <> label
        <> ": expected " <> tshow expected
        <> ", actual " <> tshow actual
    ExportDecodeError msg ->
      "decode error: " <> msg

tshow :: Show a => a -> Text
tshow = Text.pack . show

terrainWritesFromPayload :: KM.KeyMap Value -> Either Text TerrainWrites
terrainWritesFromPayload payload = do
  ensureTerrainPayloadEncoding payload
  let chunkSize = lookupChunkSize payload
  terrain <- decodeChunkSection payload "terrain" decodeTerrainChunk chunkSize
  climate <- decodeChunkSection payload "climate" decodeClimateChunk chunkSize
  vegetation <- decodeChunkSection payload "vegetation" decodeVegetationChunk chunkSize
  Right TerrainWrites
    { twrTerrain = terrain
    , twrClimate = climate
    , twrVegetation = vegetation
    }

applyTerrainPayload
  :: Topo.World.TerrainWorld
  -> KM.KeyMap Value
  -> Either Text Topo.World.TerrainWorld
applyTerrainPayload world payload = do
  writes <- terrainWritesFromPayload payload
  Right (applyTerrainWrites writes world)

decodeChunkSection
  :: KM.KeyMap Value
  -> Text
  -> (Topo.Types.WorldConfig -> BS.ByteString -> Either ExportError a)
  -> Topo.Types.WorldConfig
  -> Either Text (IntMap.IntMap a)
decodeChunkSection payload fieldName decodeChunk config =
  case KM.lookup (Key.fromText fieldName) payload of
    Nothing -> Right IntMap.empty
    Just Null -> Right IntMap.empty
    Just (Object chunkMap) ->
      IntMap.fromList <$> traverse decodeOne (KM.toList chunkMap)
    Just _ -> Left (fieldName <> " payload must be an object")
  where
    decodeOne (chunkKey, rawChunkBytes) = do
      chunkId <- parseChunkId (Key.toText chunkKey)
      bytes <- decodeChunkBytes rawChunkBytes
      decoded <- firstExportError (decodeChunk config bytes)
      Right (chunkId, decoded)

parseChunkId :: Text -> Either Text Int
parseChunkId rawChunkId =
  case readMaybe (Text.unpack rawChunkId) of
    Nothing -> Left ("invalid chunk id: " <> rawChunkId)
    Just chunkId -> Right chunkId

decodeChunkBytes :: Value -> Either Text BS.ByteString
decodeChunkBytes (Aeson.String raw) = decodeBase64Text raw
decodeChunkBytes _ = Left "chunk payload must be a base64 string"

ensureTerrainPayloadEncoding :: KM.KeyMap Value -> Either Text ()
ensureTerrainPayloadEncoding payload =
  case KM.lookup "encoding" payload of
    Just (Aeson.String "base64") -> Right ()
    Just (Aeson.String value) -> Left ("unsupported terrain payload encoding: " <> value)
    Just _ -> Left "terrain payload encoding must be a string"
    Nothing -> Left "terrain payload missing required encoding field"

lookupChunkSize :: KM.KeyMap Value -> Topo.Types.WorldConfig
lookupChunkSize payload =
  case KM.lookup "chunk_size" payload >>= valueToPositiveInt of
    Just chunkSize -> Topo.Types.WorldConfig { Topo.Types.wcChunkSize = chunkSize }
    Nothing -> Topo.Types.WorldConfig { Topo.Types.wcChunkSize = defaultChunkSize }
  where
    defaultChunkSize = 64

valueToPositiveInt :: Value -> Maybe Int
valueToPositiveInt (Number n) =
  let asInteger = floor n :: Integer
  in if fromInteger asInteger == n && asInteger > 0 && asInteger <= fromIntegral (maxBound :: Int)
      then Just (fromInteger asInteger)
      else Nothing
valueToPositiveInt _ = Nothing

base64Alphabet :: Text
base64Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

encodeBase64Text :: BS.ByteString -> Text
encodeBase64Text bytes = Text.pack (go 0)
  where
    len = BS.length bytes

    at :: Int -> Int
    at index = fromIntegral (BS.index bytes index)

    emit :: Int -> Char
    emit index = Text.index base64Alphabet index

    go :: Int -> String
    go index
      | index >= len = []
      | index + 2 < len =
          let b0 = at index
              b1 = at (index + 1)
              b2 = at (index + 2)
              c0 = emit (b0 `shiftR` 2)
              c1 = emit (((b0 .&. 0x03) `shiftL` 4) .|. (b1 `shiftR` 4))
              c2 = emit (((b1 .&. 0x0F) `shiftL` 2) .|. (b2 `shiftR` 6))
              c3 = emit (b2 .&. 0x3F)
          in c0 : c1 : c2 : c3 : go (index + 3)
      | index + 1 < len =
          let b0 = at index
              b1 = at (index + 1)
              c0 = emit (b0 `shiftR` 2)
              c1 = emit (((b0 .&. 0x03) `shiftL` 4) .|. (b1 `shiftR` 4))
              c2 = emit ((b1 .&. 0x0F) `shiftL` 2)
          in [c0, c1, c2, '=']
      | otherwise =
          let b0 = at index
              c0 = emit (b0 `shiftR` 2)
              c1 = emit ((b0 .&. 0x03) `shiftL` 4)
          in [c0, c1, '=', '=']

decodeBase64Text :: Text -> Either Text BS.ByteString
decodeBase64Text raw = do
  sextets <- traverse decodeChar (Text.unpack raw)
  bytes <- decodeSextets sextets
  Right (BS.pack bytes)
  where
    decodeChar :: Char -> Either Text Int
    decodeChar '=' = Right (-1)
    decodeChar ch =
      case Text.findIndex (== ch) base64Alphabet of
        Just index -> Right index
        Nothing -> Left ("invalid base64 character: " <> Text.singleton ch)

    decodeSextets :: [Int] -> Either Text [Word8]
    decodeSextets values
      | null values = Right []
      | (length values `mod` 4) /= 0 = Left "invalid base64 length"
      | otherwise = go values

    go :: [Int] -> Either Text [Word8]
    go [] = Right []
    go (a:b:c:d:rest)
      | a < 0 || b < 0 = Left "invalid base64 padding"
      | c == (-1) && d /= (-1) = Left "invalid base64 padding"
      | otherwise = do
          let byte0 = fromIntegral (((a `shiftL` 2) .|. (b `shiftR` 4)) .&. 0xFF)
          suffix <- case (c, d) of
            (-1, -1) ->
              if null rest
                then Right []
                else Left "invalid base64 padding location"
            (cVal, -1) | cVal >= 0 ->
              if null rest
                then
                  let byte1 = fromIntegral ((((b .&. 0x0F) `shiftL` 4) .|. (cVal `shiftR` 2)) .&. 0xFF)
                  in Right [byte1]
                else Left "invalid base64 padding location"
            (cVal, dVal) | cVal >= 0 && dVal >= 0 -> do
              let byte1 = fromIntegral ((((b .&. 0x0F) `shiftL` 4) .|. (cVal `shiftR` 2)) .&. 0xFF)
                  byte2 = fromIntegral ((((cVal .&. 0x03) `shiftL` 6) .|. dVal) .&. 0xFF)
              more <- go rest
              Right (byte1 : byte2 : more)
            _ -> Left "invalid base64 sextet"
          Right (byte0 : suffix)
    go _ = Left "invalid base64 quartet"