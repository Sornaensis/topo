{-# LANGUAGE OverloadedStrings #-}

module Spec.FeatureMatrix (spec) where

import Control.Monad (forM_, unless)
import Data.Aeson
  ( FromJSON(..)
  , Value(..)
  , eitherDecode
  , withObject
  , (.:)
  , (.:?)
  )
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory)
import Test.Hspec

import Seer.HTTP.OpenAPI (HttpRouteSpec(..), routePathText)
import Seer.HTTP.Server (publicHttpRouteSpecs)
import Seer.Service.AppService (appServiceOperationSpecs, serviceOperationMethod, serviceOperationName)

spec :: Spec
spec = describe "Feature matrix" $ do
  it "maps every exposed module, public HTTP route, service operation, and feature to accountable metadata" $ do
    root <- findRepoRoot
    matrix <- loadFeatureMatrix root

    assertFeatureGroupMetadata matrix
    assertTopLevelFeatureMetadata matrix
    assertExposedModules root matrix
    assertPublicHttpRoutes matrix
    assertServiceOperations matrix

  it "marks plugin/data-resource and simulation DAG feature surfaces present for 1.0" $ do
    root <- findRepoRoot
    matrix <- loadFeatureMatrix root
    assertFeaturePresent matrix "plugin-resource-status-surfaces-present"
    assertFeaturePresent matrix "simulation-dag-status-surfaces-present"

-- Matrix shape --------------------------------------------------------------

data FeatureMatrix = FeatureMatrix
  { fmPackages :: [MatrixPackage]
  , fmFeatures :: [MatrixFeature]
  , fmPublicHttpRoutes :: [RouteSection]
  , fmServiceOperations :: [OperationSection]
  , fmFeatureGroups :: [FeatureGroup]
  } deriving (Eq, Show)

instance FromJSON FeatureMatrix where
  parseJSON = withObject "FeatureMatrix" $ \obj ->
    FeatureMatrix
      <$> obj .: "packages"
      <*> obj .: "features"
      <*> obj .: "publicHttpRoutes"
      <*> obj .: "serviceOperations"
      <*> obj .: "featureGroups"

data MatrixPackage = MatrixPackage
  { mpName :: Text
  , mpPackageYaml :: FilePath
  } deriving (Eq, Show)

instance FromJSON MatrixPackage where
  parseJSON = withObject "MatrixPackage" $ \obj ->
    MatrixPackage
      <$> obj .: "name"
      <*> obj .: "packageYaml"

data MatrixFeature = MatrixFeature
  { mfId :: Text
  , mfTitle :: Text
  , mfAvailability :: Text
  , mfOwner :: Text
  , mfStatus :: Text
  , mfFeatureGroup :: Text
  , mfTests :: Maybe [Text]
  , mfWaiver :: Maybe Text
  } deriving (Eq, Show)

instance FromJSON MatrixFeature where
  parseJSON = withObject "MatrixFeature" $ \obj ->
    MatrixFeature
      <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .: "availability"
      <*> obj .: "owner"
      <*> obj .: "status"
      <*> obj .: "featureGroup"
      <*> obj .:? "tests"
      <*> obj .:? "waiver"

data FeatureGroup = FeatureGroup
  { fgId :: Text
  , fgOwner :: Text
  , fgStatus :: Text
  , fgTests :: Maybe [Text]
  , fgWaiver :: Maybe Text
  , fgModules :: Map Text [Text]
  } deriving (Eq, Show)

instance FromJSON FeatureGroup where
  parseJSON = withObject "FeatureGroup" $ \obj ->
    FeatureGroup
      <$> obj .: "id"
      <*> obj .: "owner"
      <*> obj .: "status"
      <*> obj .:? "tests"
      <*> obj .:? "waiver"
      <*> obj .: "modules"

data RouteSection = RouteSection
  { rsFeatureGroup :: Text
  , rsRoutes :: [MatrixRoute]
  } deriving (Eq, Show)

instance FromJSON RouteSection where
  parseJSON = withObject "RouteSection" $ \obj ->
    RouteSection
      <$> obj .: "featureGroup"
      <*> obj .: "routes"

data MatrixRoute = MatrixRoute
  { mrOperationId :: Text
  , mrMethod :: Text
  , mrPath :: Text
  } deriving (Eq, Show)

instance FromJSON MatrixRoute where
  parseJSON = withObject "MatrixRoute" $ \obj ->
    MatrixRoute
      <$> obj .: "operationId"
      <*> obj .: "method"
      <*> obj .: "path"

data OperationSection = OperationSection
  { osFeatureGroup :: Text
  , osOperations :: [MatrixOperation]
  } deriving (Eq, Show)

instance FromJSON OperationSection where
  parseJSON = withObject "OperationSection" $ \obj ->
    OperationSection
      <$> obj .: "featureGroup"
      <*> obj .: "operations"

data MatrixOperation = MatrixOperation
  { moName :: Text
  , moMethod :: Text
  } deriving (Eq, Show)

instance FromJSON MatrixOperation where
  parseJSON = withObject "MatrixOperation" $ \obj ->
    MatrixOperation
      <$> obj .: "name"
      <*> obj .: "method"

-- Assertions ----------------------------------------------------------------

assertFeatureGroupMetadata :: FeatureMatrix -> Expectation
assertFeatureGroupMetadata matrix = do
  let groups = fmFeatureGroups matrix
      groupIds = map fgId groups
  duplicates groupIds `shouldBe` []
  forM_ groups $ \group -> do
    nonBlank (fgId group) `shouldBe` True
    nonBlank (fgOwner group) `shouldBe` True
    nonBlank (fgStatus group) `shouldBe` True
    hasTestsOrWaiver (fgTests group) (fgWaiver group) `shouldBe` True
  forM_ (fmPublicHttpRoutes matrix) $ \section ->
    rsFeatureGroup section `shouldSatisfy` (`elem` groupIds)
  forM_ (fmServiceOperations matrix) $ \section ->
    osFeatureGroup section `shouldSatisfy` (`elem` groupIds)

assertTopLevelFeatureMetadata :: FeatureMatrix -> Expectation
assertTopLevelFeatureMetadata matrix = do
  let groupIds = map fgId (fmFeatureGroups matrix)
      features = fmFeatures matrix
  features `shouldSatisfy` (not . null)
  duplicates (map mfId features) `shouldBe` []
  forM_ features $ \feature -> do
    nonBlank (mfId feature) `shouldBe` True
    nonBlank (mfTitle feature) `shouldBe` True
    mfAvailability feature `shouldSatisfy` (`elem` ["present", "missing"])
    nonBlank (mfOwner feature) `shouldBe` True
    nonBlank (mfStatus feature) `shouldBe` True
    mfFeatureGroup feature `shouldSatisfy` (`elem` groupIds)
    hasTestsOrWaiver (mfTests feature) (mfWaiver feature) `shouldBe` True

assertExposedModules :: FilePath -> FeatureMatrix -> Expectation
assertExposedModules root matrix = do
  let packagesByName = Map.fromList [(mpName pkg, mpPackageYaml pkg) | pkg <- fmPackages matrix]
      matrixModules = modulesByPackage matrix
  Map.keys matrixModules `shouldSatisfy` all (`Map.member` packagesByName)
  forM_ (Map.toList packagesByName) $ \(packageName, packageYaml) -> do
    exposed <- parseExposedModulesFile (root </> packageYaml)
    let inventoried = sort (fromMaybe [] (Map.lookup packageName matrixModules))
    inventoried `shouldBe` sort exposed

assertPublicHttpRoutes :: FeatureMatrix -> Expectation
assertPublicHttpRoutes matrix = do
  let matrixRoutes = sort
        [ (mrOperationId route, mrMethod route, mrPath route)
        | section <- fmPublicHttpRoutes matrix
        , route <- rsRoutes section
        ]
      sourceRoutes = sort
        [ (hrsOperationId route, hrsMethod route, routePathText route)
        | route <- publicHttpRouteSpecs
        ]
  duplicates matrixRoutes `shouldBe` []
  matrixRoutes `shouldBe` sourceRoutes

assertFeaturePresent :: FeatureMatrix -> Text -> Expectation
assertFeaturePresent matrix featureId =
  case filter ((== featureId) . mfId) (fmFeatures matrix) of
    [feature] -> do
      mfAvailability feature `shouldBe` "present"
      mfStatus feature `shouldBe` "present"
    [] -> expectationFailure ("missing feature matrix entry " <> Text.unpack featureId)
    _ -> expectationFailure ("duplicate feature matrix entry " <> Text.unpack featureId)

assertServiceOperations :: FeatureMatrix -> Expectation
assertServiceOperations matrix = do
  let matrixOperations = sort
        [ (moName operation, moMethod operation)
        | section <- fmServiceOperations matrix
        , operation <- osOperations section
        ]
      sourceOperations = sort
        [ (serviceOperationName operation, serviceOperationMethod operation)
        | operation <- appServiceOperationSpecs
        ]
  duplicates matrixOperations `shouldBe` []
  matrixOperations `shouldBe` sourceOperations

modulesByPackage :: FeatureMatrix -> Map Text [Text]
modulesByPackage matrix = Map.fromListWith (<>)
  [ (packageName, modules)
  | group <- fmFeatureGroups matrix
  , (packageName, modules) <- Map.toList (fgModules group)
  ]

-- File loading and lightweight Hpack parsing --------------------------------

findRepoRoot :: IO FilePath
findRepoRoot = do
  start <- getCurrentDirectory
  go (6 :: Int) start
  where
    go remaining dir = do
      exists <- doesFileExist (dir </> "docs" </> "inventory" </> "public-surface.json")
      if exists
        then pure dir
        else do
          let parent = takeDirectory dir
          unless (remaining > 0 && parent /= dir) $
            expectationFailure "could not find docs/inventory/public-surface.json by walking up from the test cwd"
          go (remaining - 1) parent

loadFeatureMatrix :: FilePath -> IO FeatureMatrix
loadFeatureMatrix root = do
  let path = root </> "docs" </> "inventory" </> "public-surface.json"
  bytes <- LBS.readFile path
  case eitherDecode bytes of
    Left err -> fail ("feature matrix JSON did not parse: " <> err)
    Right matrix -> pure matrix

parseExposedModulesFile :: FilePath -> IO [Text]
parseExposedModulesFile path = parseExposedModules <$> TextIO.readFile path

parseExposedModules :: Text -> [Text]
parseExposedModules = reverse . go False False 0 [] . Text.lines
  where
    go _ _ _ acc [] = acc
    go inLibrary inField fieldIndent acc (line:rest)
      | Text.null stripped = go inLibrary inField fieldIndent acc rest
      | indent == 0 && "library:" `Text.isPrefixOf` stripped = go True False 0 acc rest
      | indent == 0 = go False False 0 acc rest
      | not inLibrary = go inLibrary inField fieldIndent acc rest
      | "exposed-modules:" `Text.isPrefixOf` stripped =
          let inlineValue = stripText (Text.drop 16 stripped)
          in go True (Text.null inlineValue) indent (parseInlineList inlineValue <> acc) rest
      | inField && indent > fieldIndent && "- " `Text.isPrefixOf` stripped =
          go True True fieldIndent (stripText (Text.drop 2 stripped) : acc) rest
      | inField && indent <= fieldIndent = go True False fieldIndent acc (line:rest)
      | otherwise = go inLibrary inField fieldIndent acc rest
      where
        uncommented = stripComment line
        stripped = stripText uncommented
        indent = Text.length uncommented - Text.length (Text.dropWhile (== ' ') uncommented)

parseInlineList :: Text -> [Text]
parseInlineList value
  | Text.null value = []
  | value == "[]" = []
  | "[" `Text.isPrefixOf` value && "]" `Text.isSuffixOf` value =
      map stripQuotes . filter (not . Text.null) . map stripText . Text.splitOn "," $ Text.dropEnd 1 (Text.drop 1 value)
  | otherwise = [stripQuotes value]

stripComment :: Text -> Text
stripComment = Text.takeWhile (/= '#')

stripText :: Text -> Text
stripText = Text.dropAround isSpace

stripQuotes :: Text -> Text
stripQuotes value = fromMaybe value $ do
  first <- Text.uncons value
  lastChar <- if Text.null value then Nothing else Just (Text.last value)
  if fst first == lastChar && fst first `elem` ['\'', '"']
    then Just (Text.init (Text.tail value))
    else Nothing

-- Small helpers -------------------------------------------------------------

nonBlank :: Text -> Bool
nonBlank = not . Text.null . Text.strip

hasTestsOrWaiver :: Maybe [Text] -> Maybe Text -> Bool
hasTestsOrWaiver tests waiver =
  maybe False (any nonBlank) tests || maybe False nonBlank waiver

duplicates :: Ord a => [a] -> [a]
duplicates = mapMaybe duplicate . grouped . sort
  where
    duplicate (x:_:_) = Just x
    duplicate _ = Nothing

    grouped [] = []
    grouped (x:xs) = let (same, rest) = span (== x) xs in (x:same) : grouped rest
