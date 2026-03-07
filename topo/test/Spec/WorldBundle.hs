{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.WorldBundle (spec) where

import Data.Aeson (Value(..))
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (IOException, throwIO)
import Test.Hspec
import Test.QuickCheck (ioProperty, property)
import Spec.Support.OverlayFixtures
  ( mkDenseFloatOverlay
  , mkSparseFloatOverlay
  , overlayProvenanceFixture
  , sparseFloatOverlaySchema
  )
import Spec.Support.Storage (emptyProvenance, saveWorldWithProvenance)

import Topo.Hex (defaultHexGridMeta)
import Topo.Overlay
  ( Overlay(..)
  , OverlayChunk(..)
  , OverlayData(..)
  , OverlayRecord(..)
  , OverlayValue(..)
  , emptyOverlayStore
  , insertOverlay
  , lookupOverlay
  )
import Topo.Overlay.Schema
  ( OverlaySchema(..) )
import Topo.Overlay.Storage (saveOverlay)
import Topo.Persistence.WorldBundle
  ( BundleLoadPolicy(..)
  , BundleSaveHooks(..)
  , WorldBundleError(..)
  , defaultBundleSaveHooks
  , loadWorldBundle
  , saveWorldBundle
  , saveWorldBundleWithProvenance
  , saveWorldBundleWithProvenanceAndHooks
  )
import Topo.Types (ChunkId(..), TileCoord(..), WorldConfig(..))
import Topo.World (TerrainWorld(..), emptyWorld, generateTerrainChunk, getElevationAt, setTerrainChunk)

spec :: Spec
spec = describe "WorldBundle" $ do
  it "round-trips an empty world with empty overlay store" $
    withSystemTempDirectory "topo-world-bundle-empty" $ \tmp -> do
      let topoPath = tmp </> "world.topo"
          config = WorldConfig { wcChunkSize = 16 }
          world0 = emptyWorld config defaultHexGridMeta

      saveResult <- saveWorldBundle topoPath world0
      case saveResult of
        Left err -> expectationFailure (show err)
        Right () -> pure ()

      loadResult <- loadWorldBundle StrictManifest topoPath
      case loadResult of
        Left err -> expectationFailure (show err)
        Right world1 -> do
          twOverlayManifest world1 `shouldBe` []
          lookupOverlay "weather" (twOverlays world1) `shouldBe` Nothing

  it "round-trips a single dense overlay" $
    withSystemTempDirectory "topo-world-bundle-dense" $ \tmp -> do
      let topoPath = tmp </> "world.topo"
          config = WorldConfig { wcChunkSize = 16 }
          base = emptyWorld config defaultHexGridMeta
          denseOverlay = mkDenseFloatOverlay "weather" "world bundle test dense overlay" 0.5 overlayProvenanceFixture
          world0 = base { twOverlays = insertOverlay denseOverlay emptyOverlayStore }

      saveResult <- saveWorldBundle topoPath world0
      case saveResult of
        Left err -> expectationFailure (show err)
        Right () -> pure ()

      loadResult <- loadWorldBundle StrictManifest topoPath
      case loadResult of
        Left err -> expectationFailure (show err)
        Right world1 -> do
          twOverlayManifest world1 `shouldBe` ["weather"]
          case lookupOverlay "weather" (twOverlays world1) of
            Nothing -> expectationFailure "expected weather dense overlay"
            Just ov -> case ovData ov of
              DenseData chunks -> IntMap.member 0 chunks `shouldBe` True
              SparseData _ -> expectationFailure "expected dense overlay"

  it "round-trips sparse plugin-style overlay with weather overlay together" $
    withSystemTempDirectory "topo-world-bundle-weather-sparse" $ \tmp -> do
      let topoPath = tmp </> "world.topo"
          config = WorldConfig { wcChunkSize = 16 }
          base = emptyWorld config defaultHexGridMeta
          sparseOverlay = mkSparseFloatOverlay "civilization" "world bundle test overlay" 0.75 overlayProvenanceFixture
          weatherOverlay = mkDenseFloatOverlay "weather" "world bundle test dense overlay" 0.35 overlayProvenanceFixture
          withSparse = insertOverlay sparseOverlay emptyOverlayStore
          world0 = base { twOverlays = insertOverlay weatherOverlay withSparse }

      saveResult <- saveWorldBundle topoPath world0
      case saveResult of
        Left err -> expectationFailure (show err)
        Right () -> pure ()

      loadResult <- loadWorldBundle StrictManifest topoPath
      case loadResult of
        Left err -> expectationFailure (show err)
        Right world1 -> do
          twOverlayManifest world1 `shouldBe` ["civilization", "weather"]
          lookupOverlay "civilization" (twOverlays world1) `shouldSatisfy` isJust
          lookupOverlay "weather" (twOverlays world1) `shouldSatisfy` isJust

  it "round-trips sparse overlay data and provenance" $
    withSystemTempDirectory "topo-world-bundle" $ \tmp -> do
      let topoPath = tmp </> "world.topo"
          world0 = mkWorldWithSparseOverlay
      saveResult <- saveWorldBundle topoPath world0
      case saveResult of
        Left err -> expectationFailure (show err)
        Right () -> pure ()

      loadResult <- loadWorldBundle StrictManifest topoPath
      case loadResult of
        Left err -> expectationFailure (show err)
        Right world1 -> do
          twOverlayManifest world1 `shouldBe` ["bundle_sparse"]
          case lookupOverlay "bundle_sparse" (twOverlays world1) of
            Nothing -> expectationFailure "expected bundle_sparse overlay"
            Just ov -> do
              ovProvenance ov `shouldBe` overlayProvenanceFixture
              case ovData ov of
                DenseData _ -> expectationFailure "expected sparse overlay"
                SparseData chunks -> IntMap.member 0 chunks `shouldBe` True

  it "round-trips dense and sparse overlays together" $
    withSystemTempDirectory "topo-world-bundle-mixed" $ \tmp -> do
      let topoPath = tmp </> "world.topo"
          world0 = mkWorldWithDenseAndSparseOverlays

      saveResult <- saveWorldBundle topoPath world0
      case saveResult of
        Left err -> expectationFailure (show err)
        Right () -> pure ()

      loadResult <- loadWorldBundle StrictManifest topoPath
      case loadResult of
        Left err -> expectationFailure (show err)
        Right world1 -> do
          twOverlayManifest world1 `shouldBe` ["bundle_dense", "bundle_sparse"]
          case lookupOverlay "bundle_dense" (twOverlays world1) of
            Nothing -> expectationFailure "expected bundle_dense overlay"
            Just ov -> case ovData ov of
              DenseData chunks -> IntMap.member 0 chunks `shouldBe` True
              SparseData _ -> expectationFailure "expected dense overlay"
          case lookupOverlay "bundle_sparse" (twOverlays world1) of
            Nothing -> expectationFailure "expected bundle_sparse overlay"
            Just ov -> case ovData ov of
              DenseData _ -> expectationFailure "expected sparse overlay"
              SparseData chunks -> IntMap.member 0 chunks `shouldBe` True

  it "official persistence path round-trips terrain and overlays together" $
    withSystemTempDirectory "topo-world-bundle-official" $ \tmp -> do
      let topoPath = tmp </> "world.topo"
          config = WorldConfig { wcChunkSize = 16 }
          terrain = generateTerrainChunk config (\(TileCoord x y) -> fromIntegral (x + y))
          base = emptyWorld config defaultHexGridMeta
          overlay = mkSparseFloatOverlay "bundle_sparse" "world bundle test overlay" 0.75 overlayProvenanceFixture
          world0 = setTerrainChunk (ChunkId 0) terrain
                 $ base { twOverlays = insertOverlay overlay emptyOverlayStore }

      saveResult <- saveWorldBundle topoPath world0
      case saveResult of
        Left err -> expectationFailure (show err)
        Right () -> pure ()

      loadResult <- loadWorldBundle StrictManifest topoPath
      case loadResult of
        Left err -> expectationFailure (show err)
        Right world1 -> do
          getElevationAt (ChunkId 0) (TileCoord 1 2) world1 `shouldBe` Just 3
          case lookupOverlay "bundle_sparse" (twOverlays world1) of
            Nothing -> expectationFailure "expected bundle_sparse overlay"
            Just loadedOverlay ->
              case ovData loadedOverlay of
                DenseData _ -> expectationFailure "expected sparse overlay"
                SparseData chunks -> IntMap.member 0 chunks `shouldBe` True

  it "fails strict load when manifest overlay schema is missing" $
    withSystemTempDirectory "topo-world-bundle-missing" $ \tmp -> do
      let topoPath = tmp </> "world.topo"
          config = WorldConfig { wcChunkSize = 16 }
          world0 = (emptyWorld config defaultHexGridMeta)
            { twOverlayManifest = ["missing_overlay"]
            }
      rawSave <- saveWorldWithProvenance topoPath emptyProvenance world0
      rawSave `shouldBe` Right ()

      loadResult <- loadWorldBundle StrictManifest topoPath
      case loadResult of
        Left (BundleMissingOverlays names) -> names `shouldBe` ["missing_overlay"]
        Left err -> expectationFailure ("unexpected error: " <> show err)
        Right _ -> expectationFailure "expected strict manifest failure"

  it "ignores sidecar overlays not listed in manifest" $
    withSystemTempDirectory "topo-world-bundle-extra" $ \tmp -> do
      let topoPath = tmp </> "world.topo"
          sidecarDir = tmp </> "world.topolay"
          world0 = mkWorldWithSparseOverlay
          extraOverlay = mkSparseFloatOverlay "extra_sparse" "world bundle test overlay" 0.9 overlayProvenanceFixture

      saveResult <- saveWorldBundle topoPath world0
      case saveResult of
        Left err -> expectationFailure (show err)
        Right () -> pure ()

      extraSave <- saveOverlay sidecarDir extraOverlay
      case extraSave of
        Left err -> expectationFailure (show err)
        Right () -> pure ()

      loadResult <- loadWorldBundle StrictManifest topoPath
      case loadResult of
        Left err -> expectationFailure (show err)
        Right world1 -> do
          lookupOverlay "bundle_sparse" (twOverlays world1) `shouldSatisfy` isJust
          lookupOverlay "extra_sparse" (twOverlays world1) `shouldBe` Nothing

  it "writes extra files atomically and cleans backup dir" $
    withSystemTempDirectory "topo-world-bundle-atomic" $ \tmp -> do
      let topoPath = tmp </> "world.topo"
          worldDir = tmp
          oldBackupDir = worldDir <> ".old"
          world0 = mkWorldWithSparseOverlay
          extras = [("meta.json", BS.pack [123, 125])]

      saveResult <- saveWorldBundleWithProvenance topoPath emptyProvenance extras world0
      case saveResult of
        Left err -> expectationFailure (show err)
        Right () -> pure ()

      metaExists <- doesFileExist (worldDir </> "meta.json")
      backupExists <- doesDirectoryExist oldBackupDir
      metaExists `shouldBe` True
      backupExists `shouldBe` False

  it "keeps previous save intact when commit fails after backup rename" $
    withSystemTempDirectory "topo-world-bundle-crash" $ \tmp -> do
      let topoPath = tmp </> "world.topo"
          config = WorldConfig { wcChunkSize = 16 }
          terrainA = generateTerrainChunk config (\_ -> 1.0)
          terrainB = generateTerrainChunk config (\_ -> 9.0)
          worldA = setTerrainChunk (ChunkId 0) terrainA (emptyWorld config defaultHexGridMeta)
          worldB = setTerrainChunk (ChunkId 0) terrainB (emptyWorld config defaultHexGridMeta)

      firstSave <- saveWorldBundle topoPath worldA
      case firstSave of
        Left err -> expectationFailure (show err)
        Right () -> pure ()

      let crashHooks = defaultBundleSaveHooks
            { bshAfterBackupRename = throwIO (userError "simulated crash") :: IO ()
            }
      crashSave <- saveWorldBundleWithProvenanceAndHooks crashHooks topoPath emptyProvenance [] worldB
      case crashSave of
        Left (BundleAtomicRenameError _) -> pure ()
        Left err -> expectationFailure ("unexpected error: " <> show err)
        Right () -> expectationFailure "expected atomic commit failure"

      loadResult <- loadWorldBundle StrictManifest topoPath
      case loadResult of
        Left err -> expectationFailure (show err)
        Right loaded ->
          getElevationAt (ChunkId 0) (TileCoord 0 0) loaded `shouldBe` Just 1.0

  it "property: arbitrary sparse overlay values round-trip through world bundle" $
    property $ \(values :: [Int]) ->
      ioProperty $ do
        let trimmed = take 32 values
        withSystemTempDirectory "topo-world-bundle-prop" $ \tmp -> do
          let topoPath = tmp </> "world.topo"
              config = WorldConfig { wcChunkSize = 16 }
              base = emptyWorld config defaultHexGridMeta
              recordAt i = OverlayRecord (V.fromList [OVFloat (fromIntegral i / 100.0)])
              sparseChunk = OverlayChunk (IntMap.fromList (zip [0 ..] (map recordAt trimmed)))
              overlay = Overlay
                { ovSchema = sparseOverlaySchema "prop_sparse"
                , ovData = SparseData (IntMap.singleton 0 sparseChunk)
                , ovProvenance = overlayProvenanceFixture
                }
              world0 = base { twOverlays = insertOverlay overlay emptyOverlayStore }

          saveResult <- saveWorldBundle topoPath world0
          case saveResult of
            Left _ -> pure False
            Right () -> do
              loadResult <- loadWorldBundle StrictManifest topoPath
              pure $ case loadResult of
                Left _ -> False
                Right world1 ->
                  case lookupOverlay "prop_sparse" (twOverlays world1) of
                    Nothing -> False
                    Just loadedOverlay ->
                      case ovData loadedOverlay of
                        DenseData _ -> False
                        SparseData chunks ->
                          case IntMap.lookup 0 chunks of
                            Nothing -> null trimmed
                            Just (OverlayChunk recs) ->
                              let expected = IntMap.fromList (zip [0 ..] (map recordAt trimmed))
                              in recs == expected

mkWorldWithSparseOverlay :: TerrainWorld
mkWorldWithSparseOverlay =
  let config = WorldConfig { wcChunkSize = 16 }
      base = emptyWorld config defaultHexGridMeta
      overlay = mkSparseFloatOverlay "bundle_sparse" "world bundle test overlay" 0.75 overlayProvenanceFixture
  in base { twOverlays = insertOverlay overlay emptyOverlayStore }

mkWorldWithDenseAndSparseOverlays :: TerrainWorld
mkWorldWithDenseAndSparseOverlays =
  let config = WorldConfig { wcChunkSize = 16 }
      base = emptyWorld config defaultHexGridMeta
      sparseOverlay = mkSparseFloatOverlay "bundle_sparse" "world bundle test overlay" 0.75 overlayProvenanceFixture
      denseOverlay = mkDenseFloatOverlay "bundle_dense" "world bundle test dense overlay" 0.2 overlayProvenanceFixture
      withSparse = insertOverlay sparseOverlay emptyOverlayStore
      withBoth = insertOverlay denseOverlay withSparse
  in base { twOverlays = withBoth }

sparseOverlaySchema :: Text.Text -> OverlaySchema
sparseOverlaySchema name = sparseFloatOverlaySchema name "world bundle test overlay"

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True
