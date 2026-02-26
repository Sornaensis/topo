{-# LANGUAGE OverloadedStrings #-}
module Spec.Storage (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Aeson (toJSON)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import Topo
import Topo.Calendar (WorldTime(..), PlanetAge(..), defaultWorldTime, defaultPlanetAge)
import Topo.Overlay (Overlay(..), OverlayData(..), insertOverlay, lookupOverlay)
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice)
import Topo.Units (UnitScales(..), defaultUnitScales)
import Topo.Weather (weatherOverlaySchema, weatherChunkToOverlay, getWeatherChunk)
import Topo.WorldGen (defaultWorldGenConfig)

newtype Note = Note Text
  deriving (Eq, Show)

instance Metadata Note where
  metadataKey _ = pack "note"
  metadataVersion _ = 1
  metadataEncode (Note txt) = encodeJsonString txt
  metadataDecode _ payload = Note <$> decodeJsonString payload

spec :: Spec
spec = describe "Storage" $ do
  it "roundtrips world encoding" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        terrain = generateTerrainChunk config (\(TileCoord x y) -> fromIntegral (x + y))
        climate = mkClimateChunk config 0.25
        weather = mkWeatherChunk config 0.75
        rivers = mkRiverChunk config 0.2
        groundwater = mkGroundwaterChunk config 0.4
        glaciers = mkGlacierChunk config 0.1
        volcanism = mkVolcanismChunk config 0.05
        world1 = setVolcanismChunk (ChunkId 0) volcanism
                  (setGlacierChunk (ChunkId 0) glaciers
                    (setGroundwaterChunk (ChunkId 0) groundwater
                      (setRiverChunk (ChunkId 0) rivers
                        (insertWeatherChunkOverlay (ChunkId 0) weather
                          (setClimateChunk (ChunkId 0) climate
                            (setTerrainChunk (ChunkId 0) terrain world0))))))
    case encodeWorld world1 of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorld encoded of
          Left err -> expectationFailure (show err)
          Right world2 -> do
            getElevationAt (ChunkId 0) (TileCoord 1 2) world2 `shouldBe` Just 3
            sampleTerrain world2 (WorldPos 1 2) `shouldBe` sampleTerrain world1 (WorldPos 1 2)
            getClimateChunk (ChunkId 0) world2 `shouldSatisfy` isJust
            getWeatherChunk (ChunkId 0) world2 `shouldSatisfy` isJust
            getRiverChunk (ChunkId 0) world2 `shouldSatisfy` isJust
            getGroundwaterChunk (ChunkId 0) world2 `shouldSatisfy` isJust
            getGlacierChunk (ChunkId 0) world2 `shouldSatisfy` isJust
            getVolcanismChunk (ChunkId 0) world2 `shouldSatisfy` isJust
            twPlanet world2 `shouldBe` defaultPlanetConfig
            twSlice world2 `shouldBe` defaultWorldSlice

  it "roundtrips world metadata" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        world1 = putHexMetaWorld (HexAxial 1 2) (Note (pack "alpha")) world0
    case encodeWorld world1 of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorldWithMetadata [metadataCodec (Proxy :: Proxy Note)] encoded of
          Left err -> expectationFailure (show err)
          Right (_, world2) ->
            getHexMetaWorld (HexAxial 1 2) world2 `shouldBe` Just (Note (pack "alpha"))

  it "roundtrips Nothing genConfig" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
    case encodeWorld world0 of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorld encoded of
          Left err -> expectationFailure (show err)
          Right world1 -> twGenConfig world1 `shouldBe` Nothing

  it "roundtrips Just genConfig" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = (emptyWorld config defaultHexGridMeta)
          { twGenConfig = Just (toJSON defaultWorldGenConfig) }
    case encodeWorld world0 of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorld encoded of
          Left err -> expectationFailure (show err)
          Right world1 -> twGenConfig world1 `shouldBe` Just (toJSON defaultWorldGenConfig)

  it "roundtrips default UnitScales" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
    case encodeWorld world0 of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorld encoded of
          Left err -> expectationFailure (show err)
          Right world1 -> twUnitScales world1 `shouldBe` defaultUnitScales

  it "roundtrips custom UnitScales" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        custom = defaultUnitScales { usTempScale = 120.0, usTempOffset = -60.0 }
        world0 = (emptyWorld config defaultHexGridMeta)
          { twUnitScales = custom }
    case encodeWorld world0 of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorld encoded of
          Left err -> expectationFailure (show err)
          Right world1 -> twUnitScales world1 `shouldBe` custom

  it "roundtrips WorldTime" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        wt = WorldTime { wtTick = 12345, wtTickRate = 2.5 }
        world0 = (emptyWorld config defaultHexGridMeta) { twWorldTime = wt }
    case encodeWorld world0 of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorld encoded of
          Left err -> expectationFailure (show err)
          Right world1 -> twWorldTime world1 `shouldBe` wt

  it "roundtrips PlanetAge" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        pa = PlanetAge { paYears = 4.5e9 }
        world0 = (emptyWorld config defaultHexGridMeta) { twPlanetAge = pa }
    case encodeWorld world0 of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorld encoded of
          Left err -> expectationFailure (show err)
          Right world1 -> twPlanetAge world1 `shouldBe` pa

  it "roundtrips overlay manifest" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        manifest = [pack "weather", pack "civilization"]
        world0 = (emptyWorld config defaultHexGridMeta) { twOverlayManifest = manifest }
    case encodeWorld world0 of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorld encoded of
          Left err -> expectationFailure (show err)
          Right world1 -> twOverlayManifest world1 `shouldBe` manifest

  it "empty overlay manifest roundtrips" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
    case encodeWorld world0 of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorld encoded of
          Left err -> expectationFailure (show err)
          Right world1 -> twOverlayManifest world1 `shouldBe` []

  it "default world has defaultWorldTime and defaultPlanetAge" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
    case encodeWorld world0 of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorld encoded of
          Left err -> expectationFailure (show err)
          Right world1 -> do
            twWorldTime world1 `shouldBe` defaultWorldTime
            twPlanetAge world1 `shouldBe` defaultPlanetAge

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

mkClimateChunk :: WorldConfig -> Float -> ClimateChunk
mkClimateChunk config base =
  let n = chunkTileCount config
      temps = U.generate n (\i -> base + fromIntegral i * 0.001)
  in ClimateChunk
      { ccTempAvg = temps
      , ccPrecipAvg = U.replicate n base
      , ccWindDirAvg = U.replicate n 0.1
      , ccWindSpdAvg = U.replicate n 0.2
      , ccHumidityAvg = U.replicate n 0
      , ccTempRange = U.replicate n 0
      , ccPrecipSeasonality = U.replicate n 0
      }

mkWeatherChunk :: WorldConfig -> Float -> WeatherChunk
mkWeatherChunk config base =
  let n = chunkTileCount config
      temps = U.replicate n base
  in WeatherChunk
      { wcTemp = temps
      , wcHumidity = U.replicate n 0.3
      , wcWindDir = U.replicate n 0.4
      , wcWindSpd = U.replicate n 0.5
      , wcPressure = U.replicate n 0.6
      , wcPrecip = U.replicate n 0.7
      }

mkRiverChunk :: WorldConfig -> Float -> RiverChunk
mkRiverChunk config base =
  let n = chunkTileCount config
      flow = U.generate n (\i -> base + fromIntegral i * 0.01)
      discharge = U.map (* 0.5) flow
      depth = U.map (* 0.1) flow
      order = U.generate n (\i -> fromIntegral (i `mod` 3 + 1))
      basin = U.generate n fromIntegral
      baseflow = U.map (* 0.05) flow
      erosion = U.map (* 0.02) flow
      deposit = U.map (* 0.01) flow
  in RiverChunk
      { rcFlowAccum = flow
      , rcDischarge = discharge
      , rcChannelDepth = depth
      , rcRiverOrder = order
      , rcBasinId = basin
      , rcBaseflow = baseflow
      , rcErosionPotential = erosion
      , rcDepositPotential = deposit
      , rcFlowDir = U.replicate n (-1)
      , rcSegOffsets = U.replicate (n + 1) 0
      , rcSegEntryEdge = U.empty
      , rcSegExitEdge = U.empty
      , rcSegDischarge = U.empty
      , rcSegOrder = U.empty
      }

mkGroundwaterChunk :: WorldConfig -> Float -> GroundwaterChunk
mkGroundwaterChunk config base =
  let n = chunkTileCount config
      storage = U.generate n (\i -> base + fromIntegral i * 0.02)
      recharge = U.map (* 0.25) storage
      discharge = U.map (* 0.1) storage
      basin = U.generate n (\i -> fromIntegral (i `mod` 5))
  in GroundwaterChunk
      { gwStorage = storage
      , gwRecharge = recharge
      , gwDischarge = discharge
      , gwBasinId = basin
      , gwInfiltration = U.empty
      , gwWaterTableDepth = U.empty
      , gwRootZoneMoisture = U.empty
      }

mkGlacierChunk :: WorldConfig -> Float -> GlacierChunk
mkGlacierChunk config base =
  let n = chunkTileCount config
      snow = U.generate n (\i -> base + fromIntegral i * 0.01)
      ice = U.map (* 0.9) snow
      melt = U.map (* 0.2) snow
      flow = U.map (* 0.05) snow
      erosion = U.map (* 0.03) snow
      deposit = U.map (* 0.02) snow
  in GlacierChunk
      { glSnowpack = snow
      , glIceThickness = ice
      , glMelt = melt
      , glFlow = flow
      , glErosionPotential = erosion
      , glDepositPotential = deposit
      }

mkVolcanismChunk :: WorldConfig -> Float -> VolcanismChunk
mkVolcanismChunk config base =
  let n = chunkTileCount config
      ventType = U.replicate n VentShield
      activity = U.replicate n VentErupting
      magma = U.generate n (\i -> base + fromIntegral i * 0.02)
      count = U.replicate n 1
      total = U.map (* 0.3) magma
      lava = U.map (* 0.5) magma
      ash = U.map (* 0.2) magma
      deposit = U.map (* 0.6) magma
  in VolcanismChunk
      { vcVentType = ventType
      , vcActivity = activity
      , vcMagma = magma
      , vcEruptionCount = count
      , vcEruptedTotal = total
      , vcLavaPotential = lava
      , vcAshPotential = ash
      , vcDepositPotential = deposit
      }

-- | Insert a 'WeatherChunk' into the overlay store of a 'TerrainWorld'.
-- Uses the weather overlay schema to store the chunk as dense SoA data.
insertWeatherChunkOverlay :: ChunkId -> WeatherChunk -> TerrainWorld -> TerrainWorld
insertWeatherChunkOverlay (ChunkId cid) wc world =
  let fields = weatherChunkToOverlay wc
      existing = case lookupOverlay "weather" (twOverlays world) of
        Just ov -> case ovData ov of
          DenseData m -> m
          _           -> IntMap.empty
        Nothing -> IntMap.empty
      updated = IntMap.insert cid fields existing
      overlay = Overlay
        { ovSchema = weatherOverlaySchema
        , ovData   = DenseData updated
        }
  in world { twOverlays = insertOverlay overlay (twOverlays world) }
