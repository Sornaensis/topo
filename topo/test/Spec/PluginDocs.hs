{-# LANGUAGE OverloadedStrings #-}

module Spec.PluginDocs (spec) where

import Control.Monad (forM_)
import Data.Aeson (Value(..), (.=), object, toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec

import Topo.Pipeline.Registry (builtinStageDocs, stageDocsMarkdown)
import Topo.Plugin.RPC.DataService
import Topo.Plugin.RPC.ExternalDataSource
import Topo.Plugin.RPC.Manifest
  ( RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusState(..)
  , defaultRPCExternalDataSourceStatus
  , manifestV3Schema
  )
import Topo.Plugin.RPC.Protocol

spec :: Spec
spec = describe "Plugin and pipeline contract docs" $ do
  it "keeps the pipeline stage reference generated from the registry" $ do
    doc <- readDoc "docs/internal/pipeline/stages.md"
    extractGenerated "pipeline-stage-registry" doc
      `shouldBe` Text.strip (stageDocsMarkdown builtinStageDocs)

  it "documents every canonical RPC message tag in public and internal protocol refs" $ do
    forM_ rpcProtocolDocs $ \path -> do
      doc <- readDoc path
      let missing = [tag | tag <- rpcMessageTags, not (tagMention tag `Text.isInfixOf` doc)]
      missing `shouldBe` []

  it "documents RPC payload fields emitted by compiled JSON encoders" $ do
    doc <- readDoc "docs/plugin-dev/rpc-protocol.md"
    let missing =
          [ sampleName <> "." <> field
          | (sampleName, value) <- rpcPayloadSamples
          , field <- objectPropertyNames value
          , not (fieldMention field `Text.isInfixOf` doc || jsonFieldMention field `Text.isInfixOf` doc)
          ]
    missing `shouldBe` []

  it "documents safe manifest packaging before plugin discovery" $ do
    docs <- mconcat <$> traverse readDoc manifestPackagingDocs
    docs `shouldSatisfy` Text.isInfixOf "stack install` only copies the executable"
    docs `shouldSatisfy` Text.isInfixOf "--topo-write-manifest"
    docs `shouldSatisfy` Text.isInfixOf "will not execute unmanifested"
    docs `shouldSatisfy` Text.isInfixOf "Non-Haskell plugins can hand-write"

  it "documents every manifest v3 top-level schema field in the public manifest ref" $ do
    doc <- readDoc "docs/plugin-dev/manifest.md"
    fields <- manifestSchemaPropertyNames manifestV3Schema
    let missing = [field | field <- fields, not (fieldMention field `Text.isInfixOf` doc)]
    missing `shouldBe` []

  it "keeps targeted plugin contract docs backend-neutral" $ do
    docs <- mconcat <$> traverse readDoc backendNeutralDocs
    let lowered = Text.toLower docs
    lowered `shouldNotSatisfy` Text.isInfixOf "sqlite"
    lowered `shouldSatisfy` Text.isInfixOf "backend-neutral"
    lowered `shouldSatisfy` Text.isInfixOf "provider-owned"
    lowered `shouldSatisfy` Text.isInfixOf "opaque"

rpcProtocolDocs :: [FilePath]
rpcProtocolDocs =
  [ "docs/plugin-dev/rpc-protocol.md"
  , "docs/internal/plugin-internals/protocol.md"
  ]

manifestPackagingDocs :: [FilePath]
manifestPackagingDocs =
  [ "docs/plugins.md"
  , "docs/plugin-dev/getting-started.md"
  , "docs/plugin-dev/manifest.md"
  , "docs/plugin-dev/index.md"
  ]

backendNeutralDocs :: [FilePath]
backendNeutralDocs =
  [ "docs/plugins.md"
  , "docs/plugin-dev/manifest.md"
  , "docs/plugin-dev/rpc-protocol.md"
  , "docs/internal/plugin-internals/manifest.md"
  , "docs/internal/plugin-internals/protocol.md"
  , "docs/internal/plugin-internals/rpc.md"
  ]

rpcMessageTypes :: [RPCMessageType]
rpcMessageTypes =
  [ MsgInvokeGenerator
  , MsgInvokeSimulation
  , MsgShutdown
  , MsgProgress
  , MsgLog
  , MsgGeneratorResult
  , MsgSimulationResult
  , MsgError
  , MsgHandshake
  , MsgHandshakeAck
  , MsgWorldChanged
  , MsgQueryResource
  , MsgQueryResult
  , MsgMutateResource
  , MsgMutateResult
  , MsgHeartbeat
  , MsgHealthCheck
  , MsgHealthStatus
  , MsgExternalDataSourceGrant
  , MsgExternalDataSourceRevoke
  , MsgExternalDataSourceStatusRequest
  , MsgExternalDataSourceStatus
  , MsgExternalDataSourceOperationResult
  , MsgStreamOpen
  , MsgStreamData
  , MsgStreamWindow
  , MsgStreamEnd
  , MsgStreamCancel
  , MsgStreamError
  ]

rpcMessageTags :: [Text]
rpcMessageTags = map rpcMessageTag rpcMessageTypes

rpcMessageTag :: RPCMessageType -> Text
rpcMessageTag ty = case toJSON ty of
  String tag -> tag
  other -> error ("RPCMessageType did not encode to a string: " <> show other)

tagMention :: Text -> Text
tagMention tag = "`" <> tag <> "`"

fieldMention :: Text -> Text
fieldMention field = "`" <> field <> "`"

jsonFieldMention :: Text -> Text
jsonFieldMention field = "\"" <> field <> "\""

objectPropertyNames :: Value -> [Text]
objectPropertyNames (Object props) = map Key.toText (KM.keys props)
objectPropertyNames _ = []

rpcPayloadSamples :: [(Text, Value)]
rpcPayloadSamples =
  [ ("RPCEnvelope", toJSON (RPCEnvelope MsgInvokeGenerator (object []) (Just 1)))
  , ("InvokeGenerator", toJSON InvokeGenerator
      { igPayloadVersion = 1
      , igStageId = "plugin:sample"
      , igSeed = 42
      , igConfig = Map.empty
      , igTerrain = object ["encoding" .= ("base64" :: Text)]
      , igInvocationScope = Nothing
      })
  , ("InvokeSimulation", toJSON InvokeSimulation
      { isPayloadVersion = 1
      , isNodeId = "sample"
      , isWorldTime = 10
      , isDeltaTicks = 1
      , isCalendar = object ["year" .= (1 :: Int), "dayOfYear" .= (1 :: Int), "hourOfDay" .= (0 :: Int)]
      , isConfig = Map.empty
      , isTerrain = Null
      , isOverlays = object []
      , isOwnOverlay = Null
      , isInvocationScope = Nothing
      })
  , ("PluginProgress", toJSON (PluginProgress "working" 0.5))
  , ("PluginLog", toJSON (PluginLog PluginLogInfo "ready"))
  , ("GeneratorResult", toJSON (GeneratorResult (object []) (Just (object [])) (Just (object []))))
  , ("SimulationResult", toJSON (SimulationResult (object []) (Just (object []))))
  , ("PluginError", toJSON (PluginError 1 "failed"))
  , ("Handshake", toJSON (Handshake currentProtocolVersion (Just "world") ["query", "mutate", "launch_auth"] (Just "nonce")))
  , ("HandshakeAck", toJSON (HandshakeAck currentProtocolVersion (Just "data") [] (Just "session") (Just "proof")))
  , ("WorldChanged", toJSON (WorldChanged (Just "world")))
  , ("Heartbeat", toJSON (Heartbeat "ok"))
  , ("HealthStatus", toJSON (HealthStatus True "ready"))
  , ("QueryResource", toJSON (QueryResource "resource" QueryAll (Just 50) (Just 0)))
  , ("QueryResult", toJSON (QueryResult "resource" [DataRecord Map.empty] (Just 1)))
  , ("MutateResource", toJSON (MutateResource "resource" (MutCreate (DataRecord Map.empty))))
  , ("MutateResult", toJSON (MutateResult False (Just "unavailable") Nothing (Just ExternalDataSourceUnavailable)))
  , ("RPCExternalDataSourceGrantMessage", toJSON sampleExternalGrant)
  , ("RPCExternalDataSourceGrantRevocation", toJSON sampleExternalRevocation)
  , ("RPCExternalDataSourceOperationResult", toJSON sampleExternalOperationResult)
  , ("RPCExternalDataSourceStatusRequest", toJSON sampleExternalStatusRequest)
  , ("RPCExternalDataSourceStatusEntry", toJSON sampleExternalStatusEntry)
  , ("RPCExternalDataSourceStatusReport", toJSON (RPCExternalDataSourceStatusReport [sampleExternalStatusEntry] (Just (object []))))
  ]

sampleExternalStatus :: RPCExternalDataSourceStatus
sampleExternalStatus = defaultRPCExternalDataSourceStatus
  { redssState = ExternalStatusReady
  , redssProviderId = Just "provider"
  , redssAvailability = Just ExternalAvailabilityAvailable
  , redssHealth = Just ExternalHealthHealthy
  , redssAccessMode = Just ExternalAccessModeReadOnly
  , redssCapabilityScope = [ExternalSourceQuery]
  }

sampleExternalGrant :: RPCExternalDataSourceGrantMessage
sampleExternalGrant = RPCExternalDataSourceGrantMessage
  { redsgmOperationId = Just "grant-op-1"
  , redsgmOperationEpoch = Just 1
  , redsgmProviderId = "provider"
  , redsgmConsumerId = Just "consumer"
  , redsgmSource = "source"
  , redsgmGrant = "grant"
  , redsgmAccess = [ExternalAccessRead]
  , redsgmResources = ["resource"]
  , redsgmCapabilityScope = [ExternalSourceQuery]
  , redsgmStatus = sampleExternalStatus
  , redsgmReference = Just (object [])
  , redsgmConfigRefs = []
  , redsgmDiagnostics = Just (object [])
  }

sampleExternalRevocation :: RPCExternalDataSourceGrantRevocation
sampleExternalRevocation = RPCExternalDataSourceGrantRevocation
  { redsrvOperationId = Just "revoke-op-1"
  , redsrvOperationEpoch = Just 2
  , redsrvProviderId = "provider"
  , redsrvConsumerId = Just "consumer"
  , redsrvSource = "source"
  , redsrvGrant = "grant"
  , redsrvReason = Just "revoked"
  , redsrvStatus = sampleExternalStatus { redssState = ExternalStatusUnavailable }
  , redsrvReference = Just (object [])
  , redsrvDiagnostics = Just (object [])
  }

sampleExternalOperationResult :: RPCExternalDataSourceOperationResult
sampleExternalOperationResult = RPCExternalDataSourceOperationResult
  { redsoOperationId = "grant-op-1"
  , redsoOperationEpoch = Just 1
  , redsoOperation = ExternalDataSourceGrantOperation
  , redsoProviderId = "provider"
  , redsoConsumerId = "consumer"
  , redsoSource = "source"
  , redsoGrant = "grant"
  , redsoAccepted = True
  , redsoApplied = True
  , redsoStatus = "applied"
  , redsoMessage = Just "grant applied"
  , redsoError = Nothing
  , redsoDiagnostics = Just (object [])
  }

sampleExternalStatusRequest :: RPCExternalDataSourceStatusRequest
sampleExternalStatusRequest = RPCExternalDataSourceStatusRequest
  { redssrProviderId = Just "provider"
  , redssrConsumerId = Just "consumer"
  , redssrSources = ["source"]
  , redssrGrants = ["grant"]
  , redssrIncludeDiagnostics = True
  , redssrReference = Just (object [])
  }

sampleExternalStatusEntry :: RPCExternalDataSourceStatusEntry
sampleExternalStatusEntry = RPCExternalDataSourceStatusEntry
  { redsstProviderId = "provider"
  , redsstConsumerId = Just "consumer"
  , redsstSource = "source"
  , redsstGrant = Just "grant"
  , redsstAccess = [ExternalAccessRead]
  , redsstResources = ["resource"]
  , redsstCapabilityScope = [ExternalSourceQuery]
  , redsstStatus = sampleExternalStatus
  , redsstReference = Just (object [])
  , redsstConfigRefs = []
  , redsstDiagnostics = Just (object [])
  }

manifestSchemaPropertyNames :: Value -> IO [Text]
manifestSchemaPropertyNames (Object root) =
  case KM.lookup (Key.fromText "properties") root of
    Just (Object props)
      | not (null (KM.keys props)) -> pure (map Key.toText (KM.keys props))
    _ -> expectationFailure "manifest schema did not expose top-level properties" >> pure []
manifestSchemaPropertyNames _ =
  expectationFailure "manifest schema was not a JSON object" >> pure []

extractGenerated :: Text -> Text -> Text
extractGenerated name doc =
  case Text.breakOn start doc of
    (_, rest) | Text.null rest -> error ("missing generated start marker: " <> Text.unpack name)
    (_, rest) ->
      let afterStart = Text.drop (Text.length start) rest
          (generated, afterGenerated) = Text.breakOn end afterStart
      in if Text.null afterGenerated
           then error ("missing generated end marker: " <> Text.unpack name)
           else Text.strip generated
  where
    start = "<!-- BEGIN GENERATED: " <> name <> " -->"
    end = "<!-- END GENERATED: " <> name <> " -->"

readDoc :: FilePath -> IO Text
readDoc relPath = do
  path <- findDocPath relPath
  TextEncoding.decodeUtf8 <$> BS.readFile path

findDocPath :: FilePath -> IO FilePath
findDocPath relPath = do
  cwd <- getCurrentDirectory
  let candidates = [cwd </> relPath, cwd </> ".." </> relPath]
  go candidates
  where
    go [] = expectationFailure ("missing documentation file: " <> relPath) >> pure relPath
    go (candidate:rest) = do
      exists <- doesFileExist candidate
      if exists then pure candidate else go rest
