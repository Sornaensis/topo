{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Spec.FixtureHarness (spec) where

import Control.Exception (SomeException, bracket, catch, try)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.=), object)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (Handle, hClose)
import System.Info (os)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , getProcessExitCode
  , proc
  , terminateProcess
  , waitForProcess
  )
import System.Timeout (timeout)
import Test.Hspec

import Topo.Hex (defaultHexGridMeta)
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice)
import Topo.Plugin.RPC.DataService
  ( DataMutation(..)
  , DataQuery(..)
  , DataRecord(..)
  , MutateResource(..)
  , MutateResult(..)
  , QueryResource(..)
  , QueryResult(..)
  )
import Topo.Plugin.RPC.ExternalDataSource
  ( RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceGrantRevocation(..)
  , RPCExternalDataSourceStatusEntry(..)
  , RPCExternalDataSourceStatusReport(..)
  , RPCExternalDataSourceStatusRequest(..)
  )
import Topo.Plugin.RPC.Manifest
  ( RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCManifest(..)
  , defaultRPCExternalDataSourceStatus
  , manifestV3
  , parseManifestFile
  , validateManifest
  )
import Topo.Plugin.RPC.Protocol
  ( GeneratorResult(..)
  , Handshake(..)
  , HandshakeAck(..)
  , InvokeGenerator(..)
  , InvokeSimulation(..)
  , RPCEnvelope(..)
  , RPCMessageType(..)
  , SimulationResult(..)
  , currentProtocolVersion
  , decodeMessage
  , encodeMessage
  )
import Topo.Plugin.RPC.Transport
  ( Transport
  , closeTransport
  , connectPlugin
  , pluginEndpointEnv
  , pluginEndpointKindEnv
  , pluginStdioCompatibilityEnv
  , recvMessage
  , sendMessage
  )
import Topo.Plugin.SDK.Test.Fixtures (fixtureNames)

spec :: Spec
spec = describe "plugin fixture harness" $ do
  it "spawns addressable stdio plugin fixtures with explicit compatibility enabled" $
    mapM_ expectHandshake ["echo", "generator", "simulation", "crud", "external-provider", "external-consumer"]

  it "writes valid manifest v3 files for SDK fixture examples" $
    mapM_ expectGeneratedManifest ["generator", "simulation", "crud", "external-provider", "external-consumer"]

  it "spawns the dedicated topo-plugin-fixture executable when built" $ do
    mFixtureExe <- findDedicatedFixtureExecutable
    case mFixtureExe of
      Nothing -> pendingWith "topo-plugin-fixture executable not found; run stack build topo-plugin-sdk:exe:topo-plugin-fixture"
      Just fixtureExe -> withDedicatedFixtureProcess fixtureExe "echo" $ \fixture -> do
        ack <- handshakeFixture fixture
        haProtocolVersion ack `shouldBe` currentProtocolVersion
        shutdownFixture fixture

  it "addresses the generator fixture through framed RPC" $
    withFixtureProcess "generator" $ \fixture -> do
      _ <- handshakeFixture fixture
      sendEnvelope fixture (RPCEnvelope MsgInvokeGenerator (Aeson.toJSON generatorInvoke) (Just 101))
      env <- recvEnvelopeOfType fixture MsgGeneratorResult
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success result -> do
          grTerrain result `shouldBe` object ["fixture" .= ("generator" :: Text), "seed" .= (123 :: Int)]
          grOverlay result `shouldBe` Just (object ["generated" .= True])
      shutdownFixture fixture

  it "addresses the simulation fixture through framed RPC" $
    withFixtureProcess "simulation" $ \fixture -> do
      _ <- handshakeFixture fixture
      sendEnvelope fixture (RPCEnvelope MsgInvokeSimulation (Aeson.toJSON simulationInvoke) (Just 102))
      env <- recvEnvelopeOfType fixture MsgSimulationResult
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> expectationFailure err
        Aeson.Success result -> do
          srOverlay result `shouldBe` object
            [ "fixture" .= ("simulation" :: Text)
            , "seed" .= (0 :: Int)
            , "dependencies" .= ["weather" :: Text]
            ]
          srTerrainWrites result `shouldBe` Just (object ["writes" .= ([] :: [Value])])
      shutdownFixture fixture

  it "addresses CRUD fixture create/list/get/update/delete/query-by-hex operations" $
    withFixtureProcess "crud" $ \fixture -> do
      _ <- handshakeFixture fixture
      queryAll fixture "items" >>= \rows -> length rows `shouldBe` 2

      let gamma = DataRecord (Map.fromList
            [ ("id", String "gamma")
            , ("name", String "Gamma")
            , ("chunk", Number 7)
            , ("tile", Number 8)
            ])
      mutate fixture "items" (MutCreate gamma) >>= \created -> mrsRecord created `shouldBe` Just gamma
      queryByKey fixture "items" (String "gamma") >>= \rows -> rows `shouldBe` [gamma]

      let gamma' = DataRecord (Map.fromList
            [ ("id", String "gamma")
            , ("name", String "Gamma Prime")
            , ("chunk", Number 7)
            , ("tile", Number 8)
            ])
      mutate fixture "items" (MutUpdate (String "gamma") gamma') >>= \updated -> mrsRecord updated `shouldBe` Just gamma'
      queryByHex fixture "items" 7 8 >>= \rows -> rows `shouldBe` [gamma']

      mutate fixture "items" (MutDelete (String "gamma")) >>= \deleted -> mrsSuccess deleted `shouldBe` True
      queryByKey fixture "items" (String "gamma") >>= \rows -> rows `shouldBe` []
      shutdownFixture fixture

  it "keeps external data-source provider and consumer fixtures backend-neutral" $ do
    providerManifest <- withFixtureProcess "external-provider" $ \fixture -> do
      ack <- handshakeFixture fixture
      haDataDirectory ack `shouldBe` Just "external-provider-data"
      manifest <- readFixtureManifest fixture
      queryByField fixture "shared_sources" "source_id" (String externalSourceName) >>= \rows -> do
        length rows `shouldBe` 1
        rows `shouldSatisfy` all (recordHasField "endpoint" (String externalProviderEndpoint))
      requestExternalStatus fixture externalProviderStatusRequest >>= expectProviderGrantStatus
      shutdownFixture fixture
      pure manifest
    withFixtureProcess "external-consumer" $ \fixture -> do
      ack <- handshakeFixture fixture
      haDataDirectory ack `shouldBe` Just "external-consumer-data"
      consumerManifest <- readFixtureManifest fixture
      expectExternalContract providerManifest consumerManifest
      queryByField fixture "source_bindings" "source_id" (String externalSourceName) >>= \rows -> do
        length rows `shouldBe` 1
        rows `shouldSatisfy` all (recordHasField "provider" (String externalProviderPluginName))
        rows `shouldSatisfy` all (recordHasField "grant" (String externalGrantName))
        rows `shouldSatisfy` all (recordHasField "status" (String "declared"))
      requestExternalStatus fixture externalConsumerStatusRequest >>= expectConsumerRefStatus
      sendEnvelope fixture (RPCEnvelope MsgExternalDataSourceGrant (Aeson.toJSON externalGrantMessage) Nothing)
      queryByField fixture "source_bindings" "status" (String "granted") >>= \rows -> do
        length rows `shouldBe` 1
        rows `shouldSatisfy` all (recordHasField "source_id" (String externalSourceName))
      sendEnvelope fixture (RPCEnvelope MsgExternalDataSourceRevoke (Aeson.toJSON externalGrantRevocation) Nothing)
      queryByField fixture "source_bindings" "status" (String "revoked") >>= \rows -> do
        length rows `shouldBe` 1
        rows `shouldSatisfy` all (recordHasField "source_id" (String externalSourceName))
      shutdownFixture fixture

  it "exposes bad-handshake, slow, and crashy failure fixtures" $ do
    withFixtureProcess "bad-handshake" $ \fixture -> do
      sendEnvelope fixture handshakeEnvelope
      raw <- recvRaw fixture 2000000
      decodeMessage raw `shouldSatisfy` isDecodeFailure
    withFixtureProcess "slow" $ \fixture -> do
      sendEnvelope fixture handshakeEnvelope
      expectNoImmediateMessage fixture
    withFixtureProcess "crashy" $ \fixture -> do
      waitForExit fixture 2000000 >>= \exitCode -> exitCode `shouldBe` ExitFailure 1

data FixtureProcess = FixtureProcess
  { fpName :: !String
  , fpWorkDir :: !FilePath
  , fpTransport :: !Transport
  , fpProcessHandle :: !ProcessHandle
  }

expectHandshake :: String -> IO ()
expectHandshake name = withFixtureProcess name $ \fixture -> do
  ack <- handshakeFixture fixture
  haProtocolVersion ack `shouldBe` currentProtocolVersion
  shutdownFixture fixture

expectGeneratedManifest :: String -> IO ()
expectGeneratedManifest name = withFixtureProcess name $ \fixture -> do
  _ <- handshakeFixture fixture
  manifest <- readFixtureManifest fixture
  rmManifestVersion manifest `shouldBe` manifestV3
  validateManifest manifest `shouldBe` []
  shutdownFixture fixture

externalProviderPluginName :: Text
externalProviderPluginName = "fixture-external-provider"

externalConsumerPluginName :: Text
externalConsumerPluginName = "fixture-external-consumer"

externalSourceName :: Text
externalSourceName = "terrain.catalog"

externalGrantName :: Text
externalGrantName = "terrain-catalog-read"

externalProviderEndpoint :: Text
externalProviderEndpoint = "fixture://provider/terrain.catalog"

externalSharedResources :: [Text]
externalSharedResources = ["shared_sources"]

externalCapabilities :: [RPCExternalDataSourceCapability]
externalCapabilities = [ExternalSourceQuery, ExternalSourceHealth]

externalReadAccess :: [RPCExternalDataSourceAccess]
externalReadAccess = [ExternalAccessRead]

expectExternalContract :: RPCManifest -> RPCManifest -> Expectation
expectExternalContract providerManifest consumerManifest = do
  rmName providerManifest `shouldBe` externalProviderPluginName
  rmName consumerManifest `shouldBe` externalConsumerPluginName
  validateManifest providerManifest `shouldBe` []
  validateManifest consumerManifest `shouldBe` []
  case (rmExternalDataSources providerManifest, rmExternalDataSourceRefs consumerManifest) of
    ([source], [ref]) -> do
      redsdName source `shouldBe` externalSourceName
      redsdKind source `shouldBe` "catalog"
      redsdCapabilities source `shouldBe` externalCapabilities
      redsdResources source `shouldBe` externalSharedResources
      redsdConnection source `shouldBe` Nothing
      redsdConfigRefs source `shouldBe` []
      redssState (redsdStatus source) `shouldBe` ExternalStatusReady
      redssProviderId (redsdStatus source) `shouldBe` Just externalProviderPluginName
      redssAvailability (redsdStatus source) `shouldBe` Just ExternalAvailabilityAvailable
      redssHealth (redsdStatus source) `shouldBe` Just ExternalHealthHealthy
      redssAccessMode (redsdStatus source) `shouldBe` Just ExternalAccessModeReadOnly
      redssCapabilityScope (redsdStatus source) `shouldBe` externalCapabilities
      case redsdGrants source of
        [grant] -> do
          redsgName grant `shouldBe` externalGrantName
          redsgAccess grant `shouldBe` externalReadAccess
          redsgCapabilities grant `shouldBe` externalCapabilities
          redsgResources grant `shouldBe` externalSharedResources
          redsgReference grant `shouldBe` Nothing
          redsgConfigRefs grant `shouldBe` []
          redsrName ref `shouldBe` externalSourceName
          redsrProvider ref `shouldBe` Just externalProviderPluginName
          redsrSource ref `shouldBe` redsdName source
          redsrRequired ref `shouldBe` True
          redsrGrant ref `shouldBe` Just (redsgName grant)
          redsrAccess ref `shouldBe` redsgAccess grant
          redsrResources ref `shouldBe` redsgResources grant
          redsrReference ref `shouldBe` Nothing
          redsrConfigRefs ref `shouldBe` []
          redssState (redsrStatus ref) `shouldBe` ExternalStatusReady
          redssProviderId (redsrStatus ref) `shouldBe` Just externalProviderPluginName
          redssAvailability (redsrStatus ref) `shouldBe` Just ExternalAvailabilityAvailable
          redssHealth (redsrStatus ref) `shouldBe` Just ExternalHealthHealthy
          redssAccessMode (redsrStatus ref) `shouldBe` Just ExternalAccessModeReadOnly
          redssCapabilityScope (redsrStatus ref) `shouldBe` externalCapabilities
        _ -> expectationFailure "expected provider fixture to expose exactly one external data-source grant"
    _ -> expectationFailure "expected one provider source and one consumer reference fixture contract"

requestExternalStatus :: FixtureProcess -> RPCExternalDataSourceStatusRequest -> IO [RPCExternalDataSourceStatusEntry]
requestExternalStatus fixture request = do
  sendEnvelope fixture (RPCEnvelope MsgExternalDataSourceStatusRequest (Aeson.toJSON request) (Just 203))
  env <- recvEnvelopeOfType fixture MsgExternalDataSourceStatus
  case Aeson.fromJSON (envPayload env) of
    Aeson.Error err -> expectationFailure err >> fail "external status decode failed"
    Aeson.Success (report :: RPCExternalDataSourceStatusReport) -> pure (redssReportStatuses report)

externalProviderStatusRequest :: RPCExternalDataSourceStatusRequest
externalProviderStatusRequest = RPCExternalDataSourceStatusRequest
  { redssrProviderId = Just externalProviderPluginName
  , redssrConsumerId = Nothing
  , redssrSources = [externalSourceName]
  , redssrGrants = [externalGrantName]
  , redssrIncludeDiagnostics = False
  , redssrReference = Nothing
  }

externalConsumerStatusRequest :: RPCExternalDataSourceStatusRequest
externalConsumerStatusRequest = externalProviderStatusRequest
  { redssrConsumerId = Just externalSourceName
  }

expectProviderGrantStatus :: [RPCExternalDataSourceStatusEntry] -> Expectation
expectProviderGrantStatus [entry] = do
  expectExternalStatusEntry Nothing entry
  redssState (redsstStatus entry) `shouldBe` ExternalStatusReady
expectProviderGrantStatus _ = expectationFailure "expected exactly one provider grant status entry"

expectConsumerRefStatus :: [RPCExternalDataSourceStatusEntry] -> Expectation
expectConsumerRefStatus [entry] = do
  expectExternalStatusEntry (Just externalSourceName) entry
  redssState (redsstStatus entry) `shouldBe` ExternalStatusReady
expectConsumerRefStatus _ = expectationFailure "expected exactly one consumer reference status entry"

expectExternalStatusEntry :: Maybe Text -> RPCExternalDataSourceStatusEntry -> Expectation
expectExternalStatusEntry consumerId entry = do
  redsstProviderId entry `shouldBe` externalProviderPluginName
  redsstConsumerId entry `shouldBe` consumerId
  redsstSource entry `shouldBe` externalSourceName
  redsstGrant entry `shouldBe` Just externalGrantName
  redsstAccess entry `shouldBe` externalReadAccess
  redsstResources entry `shouldBe` externalSharedResources
  redsstCapabilityScope entry `shouldBe` externalCapabilities
  redsstReference entry `shouldBe` Nothing
  redsstConfigRefs entry `shouldBe` []
  redssProviderId (redsstStatus entry) `shouldBe` Just externalProviderPluginName
  redssAvailability (redsstStatus entry) `shouldBe` Just ExternalAvailabilityAvailable
  redssHealth (redsstStatus entry) `shouldBe` Just ExternalHealthHealthy
  redssAccessMode (redsstStatus entry) `shouldBe` Just ExternalAccessModeReadOnly
  redssCompatibility (redsstStatus entry) `shouldBe` Just "manifest-v3"
  redssDiagnostics (redsstStatus entry) `shouldBe` Nothing
  redsstDiagnostics entry `shouldBe` Nothing

externalGrantMessage :: RPCExternalDataSourceGrantMessage
externalGrantMessage = RPCExternalDataSourceGrantMessage
  { redsgmProviderId = externalProviderPluginName
  , redsgmConsumerId = Just externalConsumerPluginName
  , redsgmSource = externalSourceName
  , redsgmGrant = externalGrantName
  , redsgmAccess = externalReadAccess
  , redsgmResources = externalSharedResources
  , redsgmCapabilityScope = externalCapabilities
  , redsgmStatus = defaultRPCExternalDataSourceStatus
      { redssState = ExternalStatusReady
      , redssProviderId = Just externalProviderPluginName
      , redssAvailability = Just ExternalAvailabilityAvailable
      , redssHealth = Just ExternalHealthHealthy
      , redssAccessMode = Just ExternalAccessModeReadOnly
      , redssCapabilityScope = externalCapabilities
      , redssCompatibility = Just "manifest-v3"
      }
  , redsgmReference = Nothing
  , redsgmConfigRefs = []
  , redsgmDiagnostics = Nothing
  }

externalGrantRevocation :: RPCExternalDataSourceGrantRevocation
externalGrantRevocation = RPCExternalDataSourceGrantRevocation
  { redsrvProviderId = externalProviderPluginName
  , redsrvConsumerId = Just externalConsumerPluginName
  , redsrvSource = externalSourceName
  , redsrvGrant = externalGrantName
  , redsrvReason = Just "fixture revocation"
  , redsrvStatus = defaultRPCExternalDataSourceStatus
      { redssState = ExternalStatusUnavailable
      , redssProviderId = Just externalProviderPluginName
      , redssAvailability = Just ExternalAvailabilityUnavailable
      , redssHealth = Just ExternalHealthUnhealthy
      , redssAccessMode = Just ExternalAccessModeDisabled
      , redssCompatibility = Just "manifest-v3"
      }
  , redsrvReference = Nothing
  , redsrvDiagnostics = Nothing
  }

readFixtureManifest :: FixtureProcess -> IO RPCManifest
readFixtureManifest fixture = do
  result <- parseManifestFile (fpWorkDir fixture </> "manifest.json")
  case result of
    Left err -> do
      expectationFailure (fpName fixture <> " manifest parse failed: " <> Text.unpack err)
      fail "manifest parse failed"
    Right manifest -> pure manifest

withFixtureProcess :: String -> (FixtureProcess -> IO a) -> IO a
withFixtureProcess name action = withFixtureWorkDir name $ \workDir ->
  bracket (spawnFixture testExecutableLauncher name workDir) cleanupFixture action

withDedicatedFixtureProcess :: FilePath -> String -> (FixtureProcess -> IO a) -> IO a
withDedicatedFixtureProcess fixtureExe name action = withFixtureWorkDir ("dedicated-" <> name) $ \workDir ->
  bracket (spawnFixture (dedicatedExecutableLauncher fixtureExe) name workDir) cleanupFixture action

withFixtureWorkDir :: String -> (FilePath -> IO a) -> IO a
withFixtureWorkDir name action = bracket setup teardown action
  where
    setup = do
      tmp <- getTemporaryDirectory
      now <- getPOSIXTime
      let dir = tmp </> ("topo-plugin-fixture-" <> name <> "-" <> show (round (now * 1000000) :: Integer))
      createDirectoryIfMissing True dir
      pure dir
    teardown dir = removePathForcibly dir `catch` \(_ :: SomeException) -> pure ()

data FixtureLauncher = FixtureLauncher
  { flExecutable :: IO FilePath
  , flArgs :: String -> [String]
  }

testExecutableLauncher :: FixtureLauncher
testExecutableLauncher = FixtureLauncher
  { flExecutable = getExecutablePath
  , flArgs = \name -> ["--topo-plugin-fixture", name]
  }

dedicatedExecutableLauncher :: FilePath -> FixtureLauncher
dedicatedExecutableLauncher fixtureExe = FixtureLauncher
  { flExecutable = pure fixtureExe
  , flArgs = \name -> [name]
  }

spawnFixture :: FixtureLauncher -> String -> FilePath -> IO FixtureProcess
spawnFixture launcher name workDir = do
  exe <- flExecutable launcher
  inherited <- getEnvironment
  let fixtureOverrides =
        [ pluginEndpointEnv
        , pluginEndpointKindEnv
        , pluginStdioCompatibilityEnv
        ]
      fixtureEnv =
        (pluginStdioCompatibilityEnv, "1")
          : filter (not . isFixtureOverride fixtureOverrides . fst) inherited
      cp = (proc exe (flArgs launcher name))
        { cwd = Just workDir
        , env = Just fixtureEnv
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        }
  (mStdin, mStdout, _mStderr, processHandle) <- createProcess cp
  case (mStdin, mStdout) of
    (Just childStdin, Just childStdout) -> do
      connection <- connectPlugin (Text.pack name) childStdout childStdin
      case connection of
        Left err -> do
          safeClose childStdin
          safeClose childStdout
          terminateProcess processHandle
          _ <- waitForProcess processHandle
          expectationFailure ("fixture transport failed: " <> show err)
          fail "fixture transport failed"
        Right transport -> pure FixtureProcess
          { fpName = name
          , fpWorkDir = workDir
          , fpTransport = transport
          , fpProcessHandle = processHandle
          }
    _ -> do
      mapM_ safeClose mStdin
      mapM_ safeClose mStdout
      terminateProcess processHandle
      _ <- waitForProcess processHandle
      expectationFailure "fixture stdio handles were not created"
      fail "fixture stdio handles were not created"

isFixtureOverride :: [String] -> String -> Bool
isFixtureOverride overrides key = any (envKeyEquals key) overrides

envKeyEquals :: String -> String -> Bool
envKeyEquals left right
  | os == "mingw32" = map toLower left == map toLower right
  | otherwise = left == right

cleanupFixture :: FixtureProcess -> IO ()
cleanupFixture fixture = do
  closeTransport (fpTransport fixture) `catch` \(_ :: SomeException) -> pure ()
  exitStatus <- getProcessExitCode (fpProcessHandle fixture)
  case exitStatus of
    Just _ -> pure ()
    Nothing -> do
      terminateProcess (fpProcessHandle fixture) `catch` \(_ :: SomeException) -> pure ()
      _ <- timeout 1000000 (waitForProcess (fpProcessHandle fixture))
      pure ()

safeClose :: Handle -> IO ()
safeClose handle = do
  _ <- try @SomeException (hClose handle)
  pure ()

sendEnvelope :: FixtureProcess -> RPCEnvelope -> IO ()
sendEnvelope fixture envelope = do
  result <- sendMessage (fpTransport fixture) (encodeMessage envelope)
  case result of
    Left err -> expectationFailure (fpName fixture <> " send failed: " <> show err)
    Right () -> pure ()

recvRaw :: FixtureProcess -> Int -> IO BS.ByteString
recvRaw fixture timeoutMicros = do
  result <- timeout timeoutMicros (recvMessage (fpTransport fixture))
  case result of
    Nothing -> expectationFailure (fpName fixture <> " recv timeout") >> fail "recv timeout"
    Just (Left err) -> expectationFailure (fpName fixture <> " recv failed: " <> show err) >> fail "recv failed"
    Just (Right bytes) -> pure bytes

recvEnvelope :: FixtureProcess -> IO RPCEnvelope
recvEnvelope fixture = do
  bytes <- recvRaw fixture 2000000
  case decodeMessage bytes of
    Left err -> expectationFailure (fpName fixture <> " decode failed: " <> Text.unpack err) >> fail "decode failed"
    Right envelope -> pure envelope

recvEnvelopeOfType :: FixtureProcess -> RPCMessageType -> IO RPCEnvelope
recvEnvelopeOfType fixture expected = do
  envelope <- recvEnvelope fixture
  if envType envelope == expected
    then pure envelope
    else case envType envelope of
      MsgLog -> recvEnvelopeOfType fixture expected
      MsgProgress -> recvEnvelopeOfType fixture expected
      actual -> expectationFailure ("expected " <> show expected <> ", got " <> show actual) >> fail "unexpected envelope"

expectNoImmediateMessage :: FixtureProcess -> IO ()
expectNoImmediateMessage fixture = do
  result <- timeout 100000 (recvMessage (fpTransport fixture))
  case result of
    Nothing -> pure ()
    Just (Left err) -> assertProcessAlive fixture ("transport reported before slow response: " <> show err)
    Just (Right bytes) -> expectationFailure (fpName fixture <> " unexpectedly sent " <> show (BS.length bytes) <> " bytes")

handshakeFixture :: FixtureProcess -> IO HandshakeAck
handshakeFixture fixture = do
  sendEnvelope fixture handshakeEnvelope
  env <- recvEnvelopeOfType fixture MsgHandshakeAck
  case Aeson.fromJSON (envPayload env) of
    Aeson.Error err -> expectationFailure err >> fail "handshake ack decode failed"
    Aeson.Success ack -> pure ack

handshakeEnvelope :: RPCEnvelope
handshakeEnvelope = RPCEnvelope MsgHandshake (Aeson.toJSON (Handshake
  { hsProtocolVersion = currentProtocolVersion
  , hsWorldPath = Just "/tmp/topo-fixture-world"
  , hsHostCapabilities = ["query", "mutate"]
  , hsAuthChallenge = Nothing
  })) (Just 100)

shutdownFixture :: FixtureProcess -> IO ()
shutdownFixture fixture = do
  sendEnvelope fixture (RPCEnvelope MsgShutdown (object []) Nothing)
  result <- timeout 2000000 (waitForProcess (fpProcessHandle fixture))
  case result of
    Nothing -> expectationFailure (fpName fixture <> " did not exit after shutdown")
    Just ExitSuccess -> pure ()
    Just exitCode -> expectationFailure (fpName fixture <> " exited after shutdown with " <> show exitCode)

waitForExit :: FixtureProcess -> Int -> IO ExitCode
waitForExit fixture timeoutMicros = do
  result <- timeout timeoutMicros (waitForProcess (fpProcessHandle fixture))
  case result of
    Nothing -> expectationFailure (fpName fixture <> " did not exit") >> fail "wait timeout"
    Just exitCode -> pure exitCode

assertProcessAlive :: FixtureProcess -> String -> IO ()
assertProcessAlive fixture diagnostic = do
  status <- getProcessExitCode (fpProcessHandle fixture)
  case status of
    Nothing -> pure ()
    Just exitCode -> expectationFailure (fpName fixture <> " exited early with " <> show exitCode <> " (" <> diagnostic <> ")")

findDedicatedFixtureExecutable :: IO (Maybe FilePath)
findDedicatedFixtureExecutable = do
  fromEnv <- lookupEnv "TOPO_PLUGIN_FIXTURE_EXE"
  case fromEnv of
    Just path -> pure (Just path)
    Nothing -> do
      cwd <- getCurrentDirectory
      let roots =
            [ cwd </> ".stack-work"
            , cwd </> "topo-plugin-sdk" </> ".stack-work"
            , cwd </> ".." </> ".stack-work"
            ]
      findFirstJustM findFixtureExecutableRecursively roots

findFixtureExecutableRecursively :: FilePath -> IO (Maybe FilePath)
findFixtureExecutableRecursively root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure Nothing
    else go root
  where
    exeNames
      | os == "mingw32" = ["topo-plugin-fixture.exe", "topo-plugin-fixture"]
      | otherwise = ["topo-plugin-fixture"]

    go dir = do
      entries <- listDirectory dir
      findFirstJustM (checkEntry dir) entries

    checkEntry dir entry = do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then go path
        else pure (if entry `elem` exeNames then Just path else Nothing)

findFirstJustM :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe b)
findFirstJustM _ [] = pure Nothing
findFirstJustM f (x:xs) = do
  result <- f x
  case result of
    Just value -> pure (Just value)
    Nothing -> findFirstJustM f xs

queryAll :: FixtureProcess -> Text -> IO [DataRecord]
queryAll fixture resource = queryRecords fixture resource QueryAll

queryByKey :: FixtureProcess -> Text -> Value -> IO [DataRecord]
queryByKey fixture resource key = queryRecords fixture resource (QueryByKey key)

queryByHex :: FixtureProcess -> Text -> Int -> Int -> IO [DataRecord]
queryByHex fixture resource chunk tile = queryRecords fixture resource (QueryByHex chunk tile)

queryByField :: FixtureProcess -> Text -> Text -> Value -> IO [DataRecord]
queryByField fixture resource field value = queryRecords fixture resource (QueryByField field value)

queryRecords :: FixtureProcess -> Text -> DataQuery -> IO [DataRecord]
queryRecords fixture resource query = do
  sendEnvelope fixture (RPCEnvelope MsgQueryResource (Aeson.toJSON (QueryResource
    { qrResource = resource
    , qrQuery = query
    , qrPageSize = Nothing
    , qrPageOffset = Nothing
    })) (Just 201))
  env <- recvEnvelopeOfType fixture MsgQueryResult
  case Aeson.fromJSON (envPayload env) of
    Aeson.Error err -> expectationFailure err >> fail "query result decode failed"
    Aeson.Success (result :: QueryResult) -> pure (qrsRecords result)

mutate :: FixtureProcess -> Text -> DataMutation -> IO MutateResult
mutate fixture resource mutation = do
  sendEnvelope fixture (RPCEnvelope MsgMutateResource (Aeson.toJSON (MutateResource
    { mrResource = resource
    , mrMutation = mutation
    })) (Just 202))
  env <- recvEnvelopeOfType fixture MsgMutateResult
  case Aeson.fromJSON (envPayload env) of
    Aeson.Error err -> expectationFailure err >> fail "mutate result decode failed"
    Aeson.Success result -> pure result

generatorInvoke :: InvokeGenerator
generatorInvoke = InvokeGenerator
  { igPayloadVersion = 1
  , igStageId = "fixture-generator"
  , igSeed = 123
  , igConfig = Map.empty
  , igTerrain = minimalTerrainPayload
  }

simulationInvoke :: InvokeSimulation
simulationInvoke = InvokeSimulation
  { isPayloadVersion = 1
  , isNodeId = "fixture-simulation"
  , isWorldTime = 10
  , isDeltaTicks = 1
  , isCalendar = object ["year" .= (1 :: Int)]
  , isConfig = Map.empty
  , isTerrain = minimalTerrainPayload
  , isOverlays = object ["weather" .= object ["kind" .= ("clear" :: Text)]]
  , isOwnOverlay = object []
  }

minimalTerrainPayload :: Value
minimalTerrainPayload = object
  [ "chunk_count" .= (0 :: Int)
  , "climate_count" .= (0 :: Int)
  , "river_count" .= (0 :: Int)
  , "vegetation_count" .= (0 :: Int)
  , "chunk_size" .= (64 :: Int)
  , "hex_grid" .= defaultHexGridMeta
  , "planet" .= defaultPlanetConfig
  , "slice" .= defaultWorldSlice
  , "encoding" .= ("base64" :: Text)
  , "terrain" .= object []
  , "climate" .= object []
  , "vegetation" .= object []
  ]

recordHasField :: Text -> Value -> DataRecord -> Bool
recordHasField field value (DataRecord fields) = Map.lookup field fields == Just value

isDecodeFailure :: Either Text RPCEnvelope -> Bool
isDecodeFailure (Left _) = True
isDecodeFailure (Right _) = False

_fixtureNamesAreDocumented :: [String]
_fixtureNamesAreDocumented = fixtureNames
