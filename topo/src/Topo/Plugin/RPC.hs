{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | High-level plugin RPC client API.
--
-- This module provides 'RPCConnection' (an active plugin session)
-- and functions to invoke generator stages and simulation ticks
-- over the transport layer.  It also provides constructors that
-- produce 'PipelineStage' and 'SimNode' values from an RPC
-- connection, integrating plugins into the generator pipeline and
-- simulation DAG respectively.
--
-- = Lifecycle
--
-- 1. Parse the plugin manifest ('parseManifestFile')
-- 2. Launch the plugin process (Phase 7 — plugin manager)
-- 3. Connect via 'connectPlugin' to get a 'Transport'
-- 4. Create an 'RPCConnection' with 'newRPCConnection'
-- 5. Use 'rpcGeneratorStage' and\/or 'rpcSimNode' to integrate
-- 6. On shutdown, call 'rpcShutdown' then 'closeTransport'
module Topo.Plugin.RPC
  ( -- * Connection
    RPCConnection(..)
  , newRPCConnection
    -- * Invocation
  , invokeGenerator
  , invokeSimulation
  , rpcShutdown
    -- * Pipeline / DAG integration
  , rpcGeneratorStage
  , rpcSimNode
    -- * Terrain payload helpers
  , terrainWorldToPayload
  , decodeTerrainWritesValue
  , applyGeneratorTerrainValue
    -- * Errors
  , RPCError(..)
    -- * Re-exports
  , module Topo.Plugin.RPC.Manifest
  , module Topo.Plugin.RPC.Transport
  , module Topo.Plugin.RPC.Protocol
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Text.Read (readMaybe)

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.=), object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

import Topo.Overlay (Overlay(..), insertOverlay, lookupOverlay)
import Topo.Overlay.JSON (overlayFromJSON, overlayToJSON)
import Topo.Export
  ( ExportError(..)
  , decodeClimateChunk
  , decodeTerrainChunk
  , decodeVegetationChunk
  , encodeClimateChunk
  , encodeTerrainChunk
  , encodeVegetationChunk
  )
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Plugin (Capability(..), PluginError(..), PluginM, getWorldP, logInfo, putWorldP)
import Topo.Calendar (CalendarDate(..), WorldTime(..))
import Topo.Simulation
  ( SimNode(..)
  , SimNodeId(..)
  , SimContext(..)
  , TerrainWrites(..)
  , applyTerrainWrites
  , emptyTerrainWrites
  )
import qualified Topo.Types
import qualified Topo.World

import Topo.Plugin.RPC.Manifest
import Topo.Plugin.RPC.Protocol
import Topo.Plugin.RPC.Transport

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

-- | Errors from the RPC layer.
data RPCError
  = RPCTransportError !TransportError
    -- ^ Underlying transport failure.
  | RPCProtocolError !Text
    -- ^ Invalid message format or unexpected message type.
  | RPCPluginError !Int !Text
    -- ^ Plugin-reported error with code and message.
  | RPCTimeout !Text
    -- ^ Plugin did not respond in time.
  deriving (Eq, Show)

------------------------------------------------------------------------
-- Connection
------------------------------------------------------------------------

-- | An active RPC session with a plugin.
data RPCConnection = RPCConnection
  { rpcManifest  :: !RPCManifest
    -- ^ The plugin's parsed manifest.
  , rpcTransport :: !Transport
    -- ^ The underlying transport handle.
  , rpcParams    :: !(Map Text Value)
    -- ^ Current parameter values for this plugin.
  }

-- | Create an 'RPCConnection' from a manifest and transport.
newRPCConnection :: RPCManifest -> Transport -> Map Text Value -> RPCConnection
newRPCConnection manifest transport params = RPCConnection
  { rpcManifest  = manifest
  , rpcTransport = transport
  , rpcParams    = params
  }

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Send an envelope and wait for a response envelope.
rpcCall :: Transport -> RPCEnvelope -> IO (Either RPCError RPCEnvelope)
rpcCall transport envelope = do
  let encoded = encodeMessage envelope
  sendResult <- sendMessage transport encoded
  case sendResult of
    Left err -> pure (Left (RPCTransportError err))
    Right () -> do
      recvResult <- recvMessage transport
      case recvResult of
        Left err -> pure (Left (RPCTransportError err))
        Right bs -> case decodeMessage bs of
          Left err  -> pure (Left (RPCProtocolError err))
          Right env -> pure (Right env)

-- | Send an envelope, collecting progress\/log messages until a
-- final result envelope arrives.  Returns the final envelope.
rpcCallWithProgress
  :: Transport
  -> RPCEnvelope
  -> (PluginProgress -> IO ())
  -> (PluginLog -> IO ())
  -> IO (Either RPCError RPCEnvelope)
rpcCallWithProgress transport envelope onProgress onLog = do
  let encoded = encodeMessage envelope
  sendResult <- sendMessage transport encoded
  case sendResult of
    Left err -> pure (Left (RPCTransportError err))
    Right () -> recvLoop
  where
    recvLoop = do
      recvResult <- recvMessage transport
      case recvResult of
        Left err -> pure (Left (RPCTransportError err))
        Right bs -> case decodeMessage bs of
          Left err  -> pure (Left (RPCProtocolError err))
          Right env -> case envType env of
            MsgProgress -> do
              case Aeson.fromJSON (envPayload env) of
                Aeson.Success prog -> onProgress prog
                Aeson.Error _      -> pure ()
              recvLoop
            MsgLog -> do
              case Aeson.fromJSON (envPayload env) of
                Aeson.Success logMsg -> onLog logMsg
                Aeson.Error _        -> pure ()
              recvLoop
            MsgError -> do
              case Aeson.fromJSON (envPayload env) of
                Aeson.Success (Topo.Plugin.RPC.Protocol.PluginError code msg) ->
                  pure (Left (RPCPluginError code msg))
                Aeson.Error err -> pure (Left (RPCProtocolError (Text.pack err)))
            _ -> pure (Right env)

------------------------------------------------------------------------
-- Invocation
------------------------------------------------------------------------

-- | Invoke the plugin's generator stage.
--
-- Sends an @invoke_generator@ message with terrain data and config,
-- collects progress updates, and returns the generator result.
invokeGenerator
  :: RPCConnection
  -> Value
  -- ^ Encoded terrain data (relevant chunks).
  -> IO (Either RPCError GeneratorResult)
invokeGenerator conn terrainData = do
  let manifest = rpcManifest conn
      envelope = RPCEnvelope
        { envType = MsgInvokeGenerator
        , envPayload = Aeson.toJSON InvokeGenerator
          { igPayloadVersion = 1
          , igStageId = "plugin:" <> rmName manifest
            , igSeed    = 0  -- Seed is set by the pipeline at call time
            , igConfig  = rpcParams conn
            , igTerrain = terrainData
            }
        }
  result <- rpcCallWithProgress (rpcTransport conn) envelope
              (\_ -> pure ())
              (\_ -> pure ())
  case result of
    Left err  -> pure (Left err)
    Right env ->
      case Aeson.fromJSON (envPayload env) of
        Aeson.Success gr -> pure (Right gr)
        Aeson.Error err  -> pure (Left (RPCProtocolError (Text.pack err)))

-- | Invoke the plugin's simulation tick.
--
-- Sends an @invoke_simulation@ message with context and overlay data.
invokeSimulation
  :: RPCConnection
  -> SimContext
  -> Overlay
  -> (PluginProgress -> IO ())
  -> (PluginLog -> IO ())
  -> IO (Either RPCError SimulationResult)
invokeSimulation conn ctx overlay onProgress onLog = do
  let manifest = rpcManifest conn
      terrainPayloadResult
        | canReadTerrain manifest = terrainWorldToPayload (scTerrain ctx)
        | otherwise = Right Null
      overlaysPayload
        | canReadOverlay manifest = overlaysToJSON (scOverlays ctx)
        | otherwise = Object mempty
      ownOverlayPayload
        | canReadOverlay manifest || canWriteOverlay manifest = overlayToJSON overlay
        | otherwise = Null
  case terrainPayloadResult of
    Left err -> pure (Left (RPCProtocolError err))
    Right terrainPayload -> do
      let envelope = RPCEnvelope
            { envType = MsgInvokeSimulation
            , envPayload = Aeson.toJSON InvokeSimulation
              { isPayloadVersion = 1
              , isNodeId     = rmName manifest
                , isWorldTime  = wtTick (scWorldTime ctx)
                , isDeltaTicks = scDeltaTicks ctx
                , isCalendar   = calendarToJSON (scCalendar ctx)
                , isConfig     = rpcParams conn
                , isTerrain    = terrainPayload
                , isOverlays   = overlaysPayload
                , isOwnOverlay = ownOverlayPayload
                }
            }
      result <- rpcCallWithProgress (rpcTransport conn) envelope onProgress onLog
      case result of
        Left err  -> pure (Left err)
        Right env ->
          case Aeson.fromJSON (envPayload env) of
            Aeson.Success sr -> pure (Right sr)
            Aeson.Error err  -> pure (Left (RPCProtocolError (Text.pack err)))

-- | Send a shutdown message to the plugin.
rpcShutdown :: RPCConnection -> IO ()
rpcShutdown conn = do
  let envelope = RPCEnvelope
        { envType = MsgShutdown
        , envPayload = object []
        }
  _ <- sendMessage (rpcTransport conn) (encodeMessage envelope)
  pure ()

------------------------------------------------------------------------
-- Pipeline / DAG integration
------------------------------------------------------------------------

-- | Create a 'PipelineStage' from an RPC connection.
--
-- The stage's action sends terrain data to the plugin, receives
-- modified chunks back, and merges them into the world.
rpcGeneratorStage :: RPCConnection -> PipelineStage
rpcGeneratorStage conn =
  let manifest = rpcManifest conn
  in PipelineStage
    { stageId   = StagePlugin (rmName manifest)
    , stageName = rmName manifest
    , stageSeedTag = "plugin:" <> rmName manifest
    , stageOverlayProduces = if manifestHasOverlay manifest then Just (rmName manifest) else Nothing
    , stageOverlayReads = []
    , stageOverlaySchema = Nothing
    , stageRun  = do
        logInfo ("plugin:" <> rmName manifest <> ": invoking generator")
        world <- getWorldP
        case terrainWorldToPayload world of
          Left err ->
            throwError (PluginInvariantError ("rpc generator encode failed: " <> err))
          Right terrainPayload -> do
            result <- liftIO (invokeGenerator conn terrainPayload)
            case result of
              Left err -> throwError (PluginInvariantError ("rpc generator failed: " <> rpcErrorText err))
              Right generatorResult ->
                case applyGeneratorResult manifest world generatorResult of
                  Left mergeErr ->
                    throwError (PluginInvariantError ("rpc generator merge failed: " <> mergeErr))
                  Right mergedWorld -> do
                    putWorldP mergedWorld
                    logInfo ("plugin:" <> rmName manifest <> ": generator complete")
    }

-- | Create a 'SimNode' from an RPC connection.
--
-- Dispatches to 'SimNodeReader' or 'SimNodeWriter' based on
-- whether the manifest declares @writeTerrain@ capability.
rpcSimNode :: RPCConnection -> SimNode
rpcSimNode conn =
  let manifest = rpcManifest conn
      nodeId   = SimNodeId (rmName manifest)
      name     = rmName manifest
      deps     = case rmSimulation manifest of
        Just sd -> map SimNodeId (rsdDependencies sd)
        Nothing -> []
  in if manifestWritesTerrain manifest
    then SimNodeWriter
      { snwId           = nodeId
      , snwOverlayName  = name
      , snwDependencies = deps
      , snwWriteTick    = \ctx overlay -> do
          if not (canWriteOverlay manifest)
            then pure (Left "manifest missing writeOverlay capability")
            else do
              result <- invokeSimulation conn ctx overlay ignoreProgress ignoreLog
              case result of
                Left err -> pure (Left (rpcErrorText err))
                Right sr -> do
                  let decodedOverlay = overlayFromJSON (ovSchema overlay) (srOverlay sr)
                      decodedWrites = decodeTerrainWrites (srTerrainWrites sr)
                  pure $ case (decodedOverlay, decodedWrites) of
                    (Right nextOverlay, Right writes) -> Right (nextOverlay, writes)
                    (Left overlayErr, _) -> Left overlayErr
                    (_, Left writesErr) -> Left writesErr
      }
    else SimNodeReader
      { snrId           = nodeId
      , snrOverlayName  = name
      , snrDependencies = deps
      , snrReadTick     = \ctx overlay -> do
          if not (canWriteOverlay manifest)
            then pure (Left "manifest missing writeOverlay capability")
            else do
              result <- invokeSimulation conn ctx overlay ignoreProgress ignoreLog
              pure $ case result of
                Left err -> Left (rpcErrorText err)
                Right sr -> overlayFromJSON (ovSchema overlay) (srOverlay sr)
      }
  where
    ignoreProgress _ = pure ()
    ignoreLog _ = pure ()

hasCapability :: RPCManifest -> Capability -> Bool
hasCapability manifest capability = capability `elem` rmCapabilities manifest

canReadTerrain :: RPCManifest -> Bool
canReadTerrain manifest =
  hasCapability manifest CapReadTerrain || hasCapability manifest CapReadWorld

canReadOverlay :: RPCManifest -> Bool
canReadOverlay manifest =
  hasCapability manifest CapReadOverlay || hasCapability manifest CapReadWorld

canWriteOverlay :: RPCManifest -> Bool
canWriteOverlay manifest =
  hasCapability manifest CapWriteOverlay || hasCapability manifest CapWriteWorld

calendarToJSON :: CalendarDate -> Value
calendarToJSON calendarDate = object
  [ "year" .= cdYear calendarDate
  , "dayOfYear" .= cdDayOfYear calendarDate
  , "hourOfDay" .= cdHourOfDay calendarDate
  ]

overlaysToJSON :: Map Text Overlay -> Value
overlaysToJSON overlays =
  object [ Key.fromText name .= overlayToJSON overlay | (name, overlay) <- Map.toList overlays ]

decodeTerrainWrites :: Maybe Value -> Either Text TerrainWrites
decodeTerrainWrites Nothing = Right emptyTerrainWrites
decodeTerrainWrites (Just Null) = Right emptyTerrainWrites
decodeTerrainWrites (Just (Object obj))
  | KM.null obj = Right emptyTerrainWrites
  | hasOnlySummaryKeys obj = Right emptyTerrainWrites
  | otherwise = terrainWritesFromPayload obj
decodeTerrainWrites (Just _) = Left "terrain_writes payload must be an object"

-- | Decode a simulation @terrain_writes@ payload into structured writes.
decodeTerrainWritesValue :: Maybe Value -> Either Text TerrainWrites
decodeTerrainWritesValue = decodeTerrainWrites

rpcErrorText :: RPCError -> Text
rpcErrorText rpcError =
  case rpcError of
    RPCTransportError transportError -> Text.pack (show transportError)
    RPCProtocolError message -> message
    RPCPluginError code message -> "plugin error " <> Text.pack (show code) <> ": " <> message
    RPCTimeout message -> "timeout: " <> message

applyGeneratorResult
  :: RPCManifest
  -> Topo.World.TerrainWorld
  -> GeneratorResult
  -> Either Text Topo.World.TerrainWorld
applyGeneratorResult manifest world result = do
  worldWithTerrain <- applyGeneratorTerrainPayload world (grTerrain result)
  applyGeneratorOverlayPayload manifest worldWithTerrain (grOverlay result)

applyGeneratorTerrainPayload
  :: Topo.World.TerrainWorld
  -> Value
  -> Either Text Topo.World.TerrainWorld
applyGeneratorTerrainPayload world Null = Right world
applyGeneratorTerrainPayload world (Object obj)
  | KM.null obj = Right world
  | hasOnlySummaryKeys obj = Right world
  | otherwise = applyTerrainPayload world obj
applyGeneratorTerrainPayload _ _ = Left "generator terrain payload must be an object or null"

-- | Apply a generator terrain payload to a world by decoding and merging
-- chunk updates.
applyGeneratorTerrainValue :: Topo.World.TerrainWorld -> Value -> Either Text Topo.World.TerrainWorld
applyGeneratorTerrainValue = applyGeneratorTerrainPayload

applyGeneratorOverlayPayload
  :: RPCManifest
  -> Topo.World.TerrainWorld
  -> Maybe Value
  -> Either Text Topo.World.TerrainWorld
applyGeneratorOverlayPayload _ world Nothing = Right world
applyGeneratorOverlayPayload manifest world (Just overlayValue) =
  case lookupOverlay (rmName manifest) (Topo.World.twOverlays world) of
    Nothing -> Left "generator returned overlay data but no host overlay is registered"
    Just existingOverlay -> do
      decodedOverlay <- overlayFromJSON (ovSchema existingOverlay) overlayValue
      let nextOverlays = insertOverlay decodedOverlay (Topo.World.twOverlays world)
      Right world { Topo.World.twOverlays = nextOverlays }

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

terrainSummaryToJSON :: Topo.World.TerrainWorld -> Value
terrainSummaryToJSON world =
  object
    [ "chunk_count" .= IntMap.size (Topo.World.twTerrain world)
    , "climate_count" .= IntMap.size (Topo.World.twClimate world)
    , "river_count" .= IntMap.size (Topo.World.twRivers world)
    , "vegetation_count" .= IntMap.size (Topo.World.twVegetation world)
    , "chunk_size" .= Topo.Types.wcChunkSize (Topo.World.twConfig world)
    , "encoding" .= ("base64" :: Text)
    ]

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
