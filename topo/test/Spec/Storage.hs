module Spec.Storage (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed as U
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import Topo
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice)

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
                        (setWeatherChunk (ChunkId 0) weather
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
