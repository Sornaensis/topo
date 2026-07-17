{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Declarative invocation payload bounds and their pure resolution contract.
--
-- This module deliberately does not perform payload delivery or result
-- enforcement.  It turns a declaration, capability set, and exact invocation
-- facts into an immutable authorization descriptor that later host work can
-- enforce without re-interpreting the manifest.
module Topo.Plugin.RPC.Scope
  ( TerrainSection(..)
  , allTerrainSections
  , RPCChunkSelector(..)
  , RPCScopeBudgets(..)
  , RPCScopeInput(..)
  , RPCScopeOutput(..)
  , RPCInvocationScopeDecl(..)
  , RPCInvocationScopes(..)
  , legacyGeneratorScope
  , legacySimulationScope
  , RPCInvocationKind(..)
  , RPCDataOperation(..)
  , RPCDataResourceDeclaration(..)
  , RPCDataResourceRequest(..)
  , RPCInvocationContext(..)
  , ResolvedDataResourceScope(..)
  , ResolvedInvocationScope(..)
  , RPCInvocationScopeBinding(..)
  , ScopeError(..)
  , validateInvocationScopeDeclarations
  , resolveInvocationScope
  , resolvedInvocationScopeDigest
  , validateInvocationScopeBinding
  ) where

import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.ByteString as BS
import Data.Aeson
  ( FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject
  , withText
  )
import qualified Data.Text.Encoding as TextEncoding
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Generics (Generic)
import Topo.Plugin (Capability(..))

-- | Terrain codecs currently supported by the scoped RPC payload.
data TerrainSection
  = TerrainElevation
  | TerrainClimate
  | TerrainVegetation
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

allTerrainSections :: Set TerrainSection
allTerrainSections = Set.fromList [minBound .. maxBound]

instance FromJSON TerrainSection where
  parseJSON = withText "TerrainSection" $ \value -> case value of
    "terrain" -> pure TerrainElevation
    "climate" -> pure TerrainClimate
    "vegetation" -> pure TerrainVegetation
    _ -> fail ("unknown scoped terrain section: " <> Text.unpack value)

instance ToJSON TerrainSection where
  toJSON TerrainElevation = "terrain"
  toJSON TerrainClimate = "climate"
  toJSON TerrainVegetation = "vegetation"

-- | A selector whose result can be derived exactly from invocation facts.
data RPCChunkSelector
  = SelectAllInvocationChunks
  | SelectOverlayUnion ![Text]
  | SelectOverlayIntersection ![Text]
  | SelectCallerChunks
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCChunkSelector where
  parseJSON = withObject "RPCChunkSelector" $ \o -> do
    selectorType <- o .: "type"
    case (selectorType :: Text) of
      "all" -> pure SelectAllInvocationChunks
      "overlay_union" -> SelectOverlayUnion <$> o .: "overlays"
      "overlay_intersection" -> SelectOverlayIntersection <$> o .: "overlays"
      "caller" -> pure SelectCallerChunks
      _ -> fail ("unknown chunk selector: " <> Text.unpack selectorType)

instance ToJSON RPCChunkSelector where
  toJSON selector = case selector of
    SelectAllInvocationChunks -> object ["type" .= ("all" :: Text)]
    SelectOverlayUnion names -> object
      [ "type" .= ("overlay_union" :: Text), "overlays" .= names ]
    SelectOverlayIntersection names -> object
      [ "type" .= ("overlay_intersection" :: Text), "overlays" .= names ]
    SelectCallerChunks -> object ["type" .= ("caller" :: Text)]

-- | Aggregate decoded-byte ceilings.  Resolved ceilings are the minimum of
-- declaration and host/invocation limits.
data RPCScopeBudgets = RPCScopeBudgets
  { rsbTerrainBytes :: !Word64
  , rsbOverlayBytes :: !Word64
  , rsbOutputBytes :: !Word64
  } deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCScopeBudgets where
  parseJSON = withObject "RPCScopeBudgets" $ \o -> RPCScopeBudgets
    <$> o .: "terrainBytes"
    <*> o .: "overlayBytes"
    <*> o .: "outputBytes"

instance ToJSON RPCScopeBudgets where
  toJSON budgets = object
    [ "terrainBytes" .= rsbTerrainBytes budgets
    , "overlayBytes" .= rsbOverlayBytes budgets
    , "outputBytes" .= rsbOutputBytes budgets
    ]

-- | Maximum data an invocation may receive.
data RPCScopeInput = RPCScopeInput
  { rsiTerrainSections :: ![TerrainSection]
  , rsiChunkSelector :: !RPCChunkSelector
  , rsiDependencyOverlays :: ![Text]
  , rsiOwnOverlay :: !Bool
  } deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCScopeInput where
  parseJSON = withObject "RPCScopeInput" $ \o -> RPCScopeInput
    <$> o .: "terrainSections"
    <*> o .: "chunks"
    <*> o .: "dependencyOverlays"
    <*> o .: "ownOverlay"

instance ToJSON RPCScopeInput where
  toJSON input = object
    [ "terrainSections" .= rsiTerrainSections input
    , "chunks" .= rsiChunkSelector input
    , "dependencyOverlays" .= rsiDependencyOverlays input
    , "ownOverlay" .= rsiOwnOverlay input
    ]

-- | Maximum data an invocation may return.
data RPCScopeOutput = RPCScopeOutput
  { rsoTerrainSections :: ![TerrainSection]
  , rsoChunkSelector :: !RPCChunkSelector
  , rsoOwnedOverlay :: !Bool
  , rsoGeneratorMetadata :: !Bool
  } deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCScopeOutput where
  parseJSON = withObject "RPCScopeOutput" $ \o -> RPCScopeOutput
    <$> o .: "terrainSections"
    <*> o .: "chunks"
    <*> o .: "ownedOverlay"
    <*> o .: "generatorMetadata"

instance ToJSON RPCScopeOutput where
  toJSON output = object
    [ "terrainSections" .= rsoTerrainSections output
    , "chunks" .= rsoChunkSelector output
    , "ownedOverlay" .= rsoOwnedOverlay output
    , "generatorMetadata" .= rsoGeneratorMetadata output
    ]

data RPCInvocationScopeDecl = RPCInvocationScopeDecl
  { risdInput :: !RPCScopeInput
  , risdOutput :: !RPCScopeOutput
  , risdBudgets :: !RPCScopeBudgets
  } deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCInvocationScopeDecl where
  parseJSON = withObject "RPCInvocationScopeDecl" $ \o -> RPCInvocationScopeDecl
    <$> o .: "input"
    <*> o .: "output"
    <*> o .: "budgets"

instance ToJSON RPCInvocationScopeDecl where
  toJSON declaration = object
    [ "input" .= risdInput declaration
    , "output" .= risdOutput declaration
    , "budgets" .= risdBudgets declaration
    ]

-- | Versioned top-level manifest declaration.  Absence is represented by
-- 'Nothing' on the manifest, rather than by a permissive parser default.
data RPCInvocationScopes = RPCInvocationScopes
  { riscVersion :: !Int
  , riscGenerator :: !(Maybe RPCInvocationScopeDecl)
  , riscSimulation :: !(Maybe RPCInvocationScopeDecl)
  } deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCInvocationScopes where
  parseJSON = withObject "RPCInvocationScopes" $ \o -> RPCInvocationScopes
    <$> o .: "version"
    <*> o .:? "generator"
    <*> o .:? "simulation"

instance ToJSON RPCInvocationScopes where
  toJSON scopes = object $
    ["version" .= riscVersion scopes]
    <> ["generator" .= value | Just value <- [riscGenerator scopes]]
    <> ["simulation" .= value | Just value <- [riscSimulation scopes]]

-- | Constructor used by compatibility adapters to spell the protocol-v4
-- broad generator behavior explicitly for protocol-v5 negotiation.
legacyGeneratorScope :: RPCScopeBudgets -> RPCInvocationScopeDecl
legacyGeneratorScope budgets = RPCInvocationScopeDecl
  { risdInput = RPCScopeInput
      { rsiTerrainSections = Set.toAscList allTerrainSections
      , rsiChunkSelector = SelectAllInvocationChunks
      , rsiDependencyOverlays = []
      , rsiOwnOverlay = False
      }
  , risdOutput = RPCScopeOutput
      { rsoTerrainSections = Set.toAscList allTerrainSections
      , rsoChunkSelector = SelectAllInvocationChunks
      , rsoOwnedOverlay = True
      , rsoGeneratorMetadata = True
      }
  , risdBudgets = budgets
  }

-- | Constructor used by compatibility adapters to spell the protocol-v4
-- broad simulation behavior explicitly.
legacySimulationScope :: [Text] -> RPCScopeBudgets -> RPCInvocationScopeDecl
legacySimulationScope dependencies budgets = RPCInvocationScopeDecl
  { risdInput = RPCScopeInput
      { rsiTerrainSections = Set.toAscList allTerrainSections
      , rsiChunkSelector = SelectAllInvocationChunks
      , rsiDependencyOverlays = dependencies
      , rsiOwnOverlay = True
      }
  , risdOutput = RPCScopeOutput
      { rsoTerrainSections = Set.toAscList allTerrainSections
      , rsoChunkSelector = SelectAllInvocationChunks
      , rsoOwnedOverlay = True
      , rsoGeneratorMetadata = False
      }
  , risdBudgets = budgets
  }

data RPCInvocationKind
  = InvocationGenerator
  | InvocationSimulation
  | InvocationDataResource
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON RPCInvocationKind where
  toJSON InvocationGenerator = "generator"
  toJSON InvocationSimulation = "simulation"
  toJSON InvocationDataResource = "data_resource"

instance FromJSON RPCInvocationKind where
  parseJSON = withText "RPCInvocationKind" $ \value -> case value of
    "generator" -> pure InvocationGenerator
    "simulation" -> pure InvocationSimulation
    "data_resource" -> pure InvocationDataResource
    _ -> fail ("unknown invocation kind: " <> Text.unpack value)

data RPCDataOperation
  = DataList
  | DataGet
  | DataCreate
  | DataUpdate
  | DataDelete
  | DataQueryByHex
  | DataQueryByField
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance ToJSON RPCDataOperation where
  toJSON operation = toJSON (dataOperationText operation)

instance FromJSON RPCDataOperation where
  parseJSON = withText "RPCDataOperation" $ \value -> case value of
    "list" -> pure DataList
    "get" -> pure DataGet
    "create" -> pure DataCreate
    "update" -> pure DataUpdate
    "delete" -> pure DataDelete
    "queryByHex" -> pure DataQueryByHex
    "queryByField" -> pure DataQueryByField
    _ -> fail ("unknown data operation: " <> Text.unpack value)

data RPCDataResourceDeclaration = RPCDataResourceDeclaration
  { rdrdName :: !Text
  , rdrdOperations :: !(Set RPCDataOperation)
  , rdrdMaxPageSize :: !Int
  } deriving (Eq, Ord, Show, Read, Generic)

data RPCDataResourceRequest = RPCDataResourceRequest
  { rdrrResource :: !Text
  , rdrrOperation :: !RPCDataOperation
  , rdrrPageOffset :: !(Maybe Int)
  , rdrrPageSize :: !(Maybe Int)
  , rdrrQueryChunkId :: !(Maybe Int)
  , rdrrQueryTileIndex :: !(Maybe Int)
  } deriving (Eq, Ord, Show, Read, Generic)

-- | Exact facts supplied by one host invocation.
data RPCInvocationContext = RPCInvocationContext
  { ricKind :: !RPCInvocationKind
  , ricWorldChunkIds :: !IntSet
  , ricCallerChunkIds :: !(Maybe IntSet)
  , ricAllowsCallerChunks :: !Bool
    -- ^ True only for an invocation type that genuinely supplies a target.
  , ricDependencyOverlayNames :: !(Set Text)
  , ricOverlayChunkIds :: !(Map Text IntSet)
  , ricOwnedOverlayName :: !(Maybe Text)
  , ricOwnOverlayChunkIds :: !IntSet
  , ricDataResourceDeclarations :: !(Map Text RPCDataResourceDeclaration)
  , ricDataResourceRequest :: !(Maybe RPCDataResourceRequest)
  , ricAvailableBudgets :: !RPCScopeBudgets
  } deriving (Eq, Show, Read, Generic)

data ResolvedDataResourceScope = ResolvedDataResourceScope
  { rdrsResource :: !Text
  , rdrsOperation :: !RPCDataOperation
  , rdrsPageOffset :: !(Maybe Int)
  , rdrsPageSize :: !(Maybe Int)
  , rdrsQueryChunkId :: !(Maybe Int)
  , rdrsQueryTileIndex :: !(Maybe Int)
  } deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON ResolvedDataResourceScope where
  toJSON scope = object
    [ "resource" .= rdrsResource scope
    , "operation" .= rdrsOperation scope
    , "pageOffset" .= rdrsPageOffset scope
    , "pageSize" .= rdrsPageSize scope
    , "queryChunkId" .= rdrsQueryChunkId scope
    , "queryTileIndex" .= rdrsQueryTileIndex scope
    ]

instance FromJSON ResolvedDataResourceScope where
  parseJSON = withObject "ResolvedDataResourceScope" $ \o -> ResolvedDataResourceScope
    <$> o .: "resource"
    <*> o .: "operation"
    <*> o .:? "pageOffset"
    <*> o .:? "pageSize"
    <*> o .:? "queryChunkId"
    <*> o .:? "queryTileIndex"

-- | Exact immutable authorization produced by 'resolveInvocationScope'.
data ResolvedInvocationScope = ResolvedInvocationScope
  { risScopeId :: !Text
  , risKind :: !RPCInvocationKind
  , risTerrainInputSections :: !(Set TerrainSection)
  , risTerrainInputChunkIds :: !IntSet
  , risDependencyOverlayChunkIds :: !(Map Text IntSet)
  , risOwnOverlayReadChunkIds :: !IntSet
  , risTerrainOutputSections :: !(Set TerrainSection)
  , risTerrainOutputChunkIds :: !IntSet
  , risOwnedOverlayIdentity :: !(Maybe Text)
  , risOwnOverlayWriteChunkIds :: !IntSet
  , risGeneratorMetadataOutput :: !Bool
  , risDataResource :: !(Maybe ResolvedDataResourceScope)
  , risBudgets :: !RPCScopeBudgets
  } deriving (Eq, Show, Read, Generic)

instance ToJSON ResolvedInvocationScope where
  toJSON scope = object
    [ "scopeId" .= risScopeId scope
    , "kind" .= risKind scope
    , "terrainInputSections" .= Set.toAscList (risTerrainInputSections scope)
    , "terrainInputChunkIds" .= IntSet.toAscList (risTerrainInputChunkIds scope)
    , "dependencyOverlayChunkIds" .= fmap IntSet.toAscList (risDependencyOverlayChunkIds scope)
    , "ownOverlayReadChunkIds" .= IntSet.toAscList (risOwnOverlayReadChunkIds scope)
    , "terrainOutputSections" .= Set.toAscList (risTerrainOutputSections scope)
    , "terrainOutputChunkIds" .= IntSet.toAscList (risTerrainOutputChunkIds scope)
    , "ownedOverlayIdentity" .= risOwnedOverlayIdentity scope
    , "ownOverlayWriteChunkIds" .= IntSet.toAscList (risOwnOverlayWriteChunkIds scope)
    , "generatorMetadataOutput" .= risGeneratorMetadataOutput scope
    , "dataResource" .= risDataResource scope
    , "budgets" .= risBudgets scope
    ]

instance FromJSON ResolvedInvocationScope where
  parseJSON = withObject "ResolvedInvocationScope" $ \o -> do
    scopeId <- o .: "scopeId"
    kind <- o .: "kind"
    inputSections <- Set.fromList <$> o .: "terrainInputSections"
    inputChunks <- IntSet.fromList <$> o .: "terrainInputChunkIds"
    overlayChunks <- fmap IntSet.fromList <$> o .: "dependencyOverlayChunkIds"
    ownRead <- IntSet.fromList <$> o .: "ownOverlayReadChunkIds"
    outputSections <- Set.fromList <$> o .: "terrainOutputSections"
    outputChunks <- IntSet.fromList <$> o .: "terrainOutputChunkIds"
    ownedIdentity <- o .:? "ownedOverlayIdentity"
    ownWrite <- IntSet.fromList <$> o .: "ownOverlayWriteChunkIds"
    metadataOutput <- o .: "generatorMetadataOutput"
    dataResource <- o .:? "dataResource"
    budgets <- o .: "budgets"
    pure ResolvedInvocationScope
      { risScopeId = scopeId
      , risKind = kind
      , risTerrainInputSections = inputSections
      , risTerrainInputChunkIds = inputChunks
      , risDependencyOverlayChunkIds = overlayChunks
      , risOwnOverlayReadChunkIds = ownRead
      , risTerrainOutputSections = outputSections
      , risTerrainOutputChunkIds = outputChunks
      , risOwnedOverlayIdentity = ownedIdentity
      , risOwnOverlayWriteChunkIds = ownWrite
      , risGeneratorMetadataOutput = metadataOutput
      , risDataResource = dataResource
      , risBudgets = budgets
      }

-- | Optional invocation wire binding. Protocol-v4 decoders ignore this new
-- field; protocol-v5 callers require it and bind the reference to the inline
-- descriptor digest.
data RPCInvocationScopeBinding = RPCInvocationScopeBinding
  { risbScopeId :: !Text
  , risbDescriptor :: !(Maybe ResolvedInvocationScope)
  } deriving (Eq, Show, Read, Generic)

instance ToJSON RPCInvocationScopeBinding where
  toJSON binding = object $
    ["scopeId" .= risbScopeId binding]
    <> ["descriptor" .= descriptor | Just descriptor <- [risbDescriptor binding]]

instance FromJSON RPCInvocationScopeBinding where
  parseJSON = withObject "RPCInvocationScopeBinding" $ \o -> RPCInvocationScopeBinding
    <$> o .: "scopeId"
    <*> o .:? "descriptor"

data ScopeError = ScopeError
  { sePath :: !Text
  , seMessage :: !Text
  } deriving (Eq, Ord, Show, Read, Generic)

-- | Manifest-level structural and compatibility validation.  The returned
-- paths are suitable for conversion to manifest diagnostics.
validateInvocationScopeDeclarations
  :: Int
  -> Bool
  -> Maybe [Text]
  -> Bool
  -> Maybe RPCInvocationScopes
  -> [ScopeError]
validateInvocationScopeDeclarations protocolMin hasGenerator simulationDependencies hasOverlay scopes =
  requiredErrors <> case scopes of
    Nothing -> []
    Just declarations ->
      [ ScopeError "invocationScopes.version" "only invocation scope version 1 is supported"
      | riscVersion declarations /= 1
      ]
      <> participationErrors declarations
      <> maybe [] (\declaration -> validateDeclaration
            (protocolMin < 5 && isLegacyGeneratorDeclaration hasOverlay declaration)
            InvocationGenerator [] hasOverlay "invocationScopes.generator" declaration)
          (riscGenerator declarations)
      <> maybe [] (validateDeclaration False InvocationSimulation dependencies hasOverlay "invocationScopes.simulation") (riscSimulation declarations)
  where
    dependencies = fromMaybe [] simulationDependencies
    requiredErrors
      | protocolMin < 5 = []
      | otherwise =
          [ ScopeError "invocationScopes.generator" "protocol 5 generator participation requires an explicit scope"
          | hasGenerator, maybe True ((== Nothing) . riscGenerator) scopes
          ]
          <> [ ScopeError "invocationScopes.simulation" "protocol 5 simulation participation requires an explicit scope"
             | simulationDependencies /= Nothing
             , maybe True ((== Nothing) . riscSimulation) scopes
             ]
    participationErrors declarations =
      [ ScopeError "invocationScopes.generator" "scope declared without generator participation"
      | not hasGenerator, riscGenerator declarations /= Nothing
      ]
      <> [ ScopeError "invocationScopes.simulation" "scope declared without simulation participation"
         | simulationDependencies == Nothing, riscSimulation declarations /= Nothing
         ]

isLegacyGeneratorDeclaration :: Bool -> RPCInvocationScopeDecl -> Bool
isLegacyGeneratorDeclaration hasOverlay declaration = declaration == expected
  where
    broad = legacyGeneratorScope (risdBudgets declaration)
    expected = broad
      { risdOutput = (risdOutput broad) { rsoOwnedOverlay = hasOverlay }
      }

validateDeclaration :: Bool -> RPCInvocationKind -> [Text] -> Bool -> Text -> RPCInvocationScopeDecl -> [ScopeError]
validateDeclaration allowLegacyGeneratorMetadata kind dependencies hasOverlay base declaration = concat
  [ duplicatesAt (base <> ".input.terrainSections") (map terrainSectionText (rsiTerrainSections input))
  , duplicatesAt (base <> ".input.dependencyOverlays") (rsiDependencyOverlays input)
  , duplicatesAt (base <> ".output.terrainSections") (map terrainSectionText (rsoTerrainSections output))
  , validateSelector kind allowedOverlayNames (base <> ".input.chunks") (rsiChunkSelector input)
  , validateSelector kind allowedOverlayNames (base <> ".output.chunks") (rsoChunkSelector output)
  , [ ScopeError (base <> ".input.dependencyOverlays") "generator scopes cannot request dependency overlays"
    | kind == InvocationGenerator, not (null (rsiDependencyOverlays input))
    ]
  , [ ScopeError (base <> ".input.ownOverlay")
        "scoped generator own-overlay input is unavailable because generator invocations carry no own-overlay field"
    | kind == InvocationGenerator, rsiOwnOverlay input
    ]
  , [ ScopeError (base <> ".input.dependencyOverlays") ("overlay is not a simulation dependency: " <> name)
    | kind == InvocationSimulation, name <- rsiDependencyOverlays input, name `notElem` dependencies
    ]
  , [ ScopeError (base <> ".input.ownOverlay") "own-overlay access requires an overlay declaration"
    | rsiOwnOverlay input, not hasOverlay
    ]
  , [ ScopeError (base <> ".output.ownedOverlay") "owned-overlay output requires an overlay declaration"
    | rsoOwnedOverlay output, not hasOverlay
    ]
  , [ ScopeError (base <> ".input.ownOverlay")
        "simulation scopes must receive the complete plugin-owned overlay"
    | kind == InvocationSimulation, not (rsiOwnOverlay input)
    ]
  , [ ScopeError (base <> ".output.ownedOverlay")
        "simulation scopes must authorize whole plugin-owned overlay output"
    | kind == InvocationSimulation, not (rsoOwnedOverlay output)
    ]
  , [ ScopeError (base <> ".output.generatorMetadata") "simulation scopes cannot emit generator metadata"
    | kind == InvocationSimulation, rsoGeneratorMetadata output
    ]
  , [ ScopeError (base <> ".output.generatorMetadata")
        "generator metadata output is unavailable until the host defines a bounded consumer"
    | kind == InvocationGenerator, rsoGeneratorMetadata output, not allowLegacyGeneratorMetadata
    ]
  ]
  where
    input = risdInput declaration
    output = risdOutput declaration
    allowedOverlayNames = Set.fromList (dependencies <> ["$own" | hasOverlay])

validateSelector :: RPCInvocationKind -> Set Text -> Text -> RPCChunkSelector -> [ScopeError]
validateSelector kind allowed path selector = case selector of
  SelectAllInvocationChunks -> []
  SelectCallerChunks ->
    [ScopeError path "caller chunk selector is unavailable for global generator/simulation invocations"]
  SelectOverlayUnion names -> validateOverlaySelector names
  SelectOverlayIntersection names -> validateOverlaySelector names
  where
    validateOverlaySelector names =
      [ScopeError path "overlay selector is unavailable for generator invocations" | kind == InvocationGenerator]
      <> [ScopeError path "overlay selector must name at least one overlay" | null names]
      <> duplicatesAt (path <> ".overlays") names
      <> [ScopeError path ("selector names unavailable overlay: " <> name) | name <- names, Set.notMember name allowed]

duplicatesAt :: Text -> [Text] -> [ScopeError]
duplicatesAt path values =
  [ScopeError path ("duplicate value: " <> value) | value <- duplicateValues values]

duplicateValues :: Ord a => [a] -> [a]
duplicateValues values = Map.keys (Map.filter (> (1 :: Int)) counts)
  where counts = Map.fromListWith (+) [(value, 1 :: Int) | value <- values]

-- | Resolve declaration ∩ capabilities ∩ exact invocation facts.  Protocol-v4
-- omission selects an explicit legacy constructor; protocol-v5 omission fails.
resolveInvocationScope
  :: Int
  -> Maybe RPCInvocationScopeDecl
  -> [Capability]
  -> RPCInvocationContext
  -> Either ScopeError ResolvedInvocationScope
resolveInvocationScope negotiatedVersion maybeDeclaration capabilities context = case ricKind context of
  InvocationDataResource -> resolveDataScope capabilities context
  kind -> do
    declaration <- case maybeDeclaration of
      Just value -> Right value
      Nothing
        | negotiatedVersion <= 4 -> Right (legacyFor kind context)
        | otherwise -> Left (ScopeError "invocation_scope" "protocol 5 requires an explicit invocation scope")
    validateResolverDeclaration kind context declaration
    if maybeDeclaration /= Nothing
        && kind == InvocationGenerator
        && rsoGeneratorMetadata (risdOutput declaration)
      then Left (ScopeError "output.generatorMetadata"
        "generator metadata output is unavailable until the host defines a bounded consumer")
      else Right ()
    if maybeDeclaration /= Nothing
        && kind == InvocationGenerator
        && rsiOwnOverlay (risdInput declaration)
      then Left (ScopeError "input.ownOverlay"
        "scoped generator own-overlay input is unavailable because generator invocations carry no own-overlay field")
      else Right ()
    if maybeDeclaration /= Nothing && kind == InvocationSimulation
      then if not (rsiOwnOverlay (risdInput declaration))
        then Left (ScopeError "input.ownOverlay"
          "simulation scopes must receive the complete plugin-owned overlay")
        else if not (rsoOwnedOverlay (risdOutput declaration))
          then Left (ScopeError "output.ownedOverlay"
            "simulation scopes must authorize whole plugin-owned overlay output")
          else Right ()
      else Right ()
    selectedInputChunks <- resolveSelector capabilities context (rsiChunkSelector (risdInput declaration))
    selectedOutputChunks <- resolveSelector capabilities context (rsoChunkSelector (risdOutput declaration))
    let canTerrainRead = hasAny [CapReadTerrain, CapReadWorld]
        canOverlayRead = hasAny [CapReadOverlay, CapReadWorld]
        canOverlayWrite = hasAny [CapWriteOverlay, CapWriteWorld]
        canTerrainWrite = kind == InvocationGenerator || hasAny [CapWriteTerrain, CapWriteWorld]
        inputSections
          | canTerrainRead = Set.fromList (rsiTerrainSections (risdInput declaration))
          | otherwise = Set.empty
        inputChunks
          | Set.null inputSections = IntSet.empty
          | otherwise = selectedInputChunks `IntSet.intersection` ricWorldChunkIds context
        requestedDependencies = Set.fromList (rsiDependencyOverlays (risdInput declaration))
        dependencyChunks
          | not canOverlayRead = Map.empty
          | otherwise = Map.map (`IntSet.intersection` selectedInputChunks)
              (Map.restrictKeys (ricOverlayChunkIds context)
                (requestedDependencies `Set.intersection` ricDependencyOverlayNames context))
        -- Whole-overlay replacement is the only negotiated own-overlay form.
        -- Its payload therefore remains complete even when the selector narrows
        -- terrain and dependency-overlay reads.
        ownRead
          | rsiOwnOverlay (risdInput declaration) && (canOverlayRead || canOverlayWrite) =
              ricOwnOverlayChunkIds context
          | otherwise = IntSet.empty
        outputSections
          | canTerrainWrite = Set.fromList (rsoTerrainSections (risdOutput declaration))
          | otherwise = Set.empty
        outputChunks
          | Set.null outputSections = IntSet.empty
          | otherwise = selectedOutputChunks `IntSet.intersection` ricWorldChunkIds context
        ownedIdentity
          | rsoOwnedOverlay (risdOutput declaration) && canOverlayWrite = ricOwnedOverlayName context
          | otherwise = Nothing
        ownWrite = case ownedIdentity of
          Nothing -> IntSet.empty
          Just _ -> ricWorldChunkIds context
        budgets = intersectBudgets (risdBudgets declaration) (ricAvailableBudgets context)
        unresolved = ResolvedInvocationScope
          { risScopeId = ""
          , risKind = kind
          , risTerrainInputSections = inputSections
          , risTerrainInputChunkIds = inputChunks
          , risDependencyOverlayChunkIds = dependencyChunks
          , risOwnOverlayReadChunkIds = ownRead
          , risTerrainOutputSections = outputSections
          , risTerrainOutputChunkIds = outputChunks
          , risOwnedOverlayIdentity = ownedIdentity
          , risOwnOverlayWriteChunkIds = ownWrite
          , risGeneratorMetadataOutput = kind == InvocationGenerator && rsoGeneratorMetadata (risdOutput declaration)
          , risDataResource = Nothing
          , risBudgets = budgets
          }
        digest = resolvedInvocationScopeDigest unresolved
    Right unresolved { risScopeId = digest }
  where
    hasAny requested = any (`elem` capabilities) requested

validateResolverDeclaration
  :: RPCInvocationKind
  -> RPCInvocationContext
  -> RPCInvocationScopeDecl
  -> Either ScopeError ()
validateResolverDeclaration kind context declaration =
  case errors of
    firstError:_ -> Left firstError
    [] -> Right ()
  where
    input = risdInput declaration
    output = risdOutput declaration
    duplicateErrors =
      duplicatesAt "input.terrainSections" (map terrainSectionText (rsiTerrainSections input))
      <> duplicatesAt "input.dependencyOverlays" (rsiDependencyOverlays input)
      <> duplicatesAt "output.terrainSections" (map terrainSectionText (rsoTerrainSections output))
    unavailableDependencies =
      [ ScopeError "input.dependencyOverlays" ("overlay is not an invocation dependency: " <> name)
      | name <- rsiDependencyOverlays input
      , Set.notMember name (ricDependencyOverlayNames context)
      ]
    selectorErrors selector = case selector of
      SelectOverlayUnion names -> overlayErrors names
      SelectOverlayIntersection names -> overlayErrors names
      SelectCallerChunks ->
        [ScopeError "chunks" "caller chunk selector is unavailable to this invocation kind"
        | not (ricAllowsCallerChunks context)]
        <> [ScopeError "chunks" "caller chunk IDs are not supplied by this invocation"
           | ricCallerChunkIds context == Nothing]
      SelectAllInvocationChunks -> []
    overlayErrors names =
      [ScopeError "chunks.overlays" "overlay selectors are unavailable for generator invocations"
      | kind == InvocationGenerator]
      <> [ScopeError "chunks.overlays" "overlay selector requires at least one overlay" | null names]
      <> duplicatesAt "chunks.overlays" names
      <> [ ScopeError "chunks.overlays" ("overlay is unavailable to this invocation: " <> name)
         | name <- names
         , name /= "$own"
         , Set.notMember name (ricDependencyOverlayNames context)
         ]
      <> [ ScopeError "chunks.overlays" "own overlay is unavailable to this invocation"
         | "$own" `elem` names, ricOwnedOverlayName context == Nothing
         ]
    errors = duplicateErrors <> unavailableDependencies
      <> [ScopeError "input.dependencyOverlays" "generator invocations cannot request dependency overlays"
         | kind == InvocationGenerator, not (null (rsiDependencyOverlays input))]
      <> [ScopeError "output.generatorMetadata" "simulation invocations cannot emit generator metadata"
         | kind == InvocationSimulation, rsoGeneratorMetadata output]
      <> selectorErrors (rsiChunkSelector input)
      <> selectorErrors (rsoChunkSelector output)
      <> [ScopeError "input.ownOverlay" "own overlay is unavailable to this invocation"
         | rsiOwnOverlay input, ricOwnedOverlayName context == Nothing]
      <> [ScopeError "output.ownedOverlay" "owned overlay is unavailable to this invocation"
         | rsoOwnedOverlay output, ricOwnedOverlayName context == Nothing]
      <> [ScopeError "ownedOverlay" "owned overlay identity must be non-empty"
         | Just name <- [ricOwnedOverlayName context], Text.null name]

legacyFor :: RPCInvocationKind -> RPCInvocationContext -> RPCInvocationScopeDecl
legacyFor InvocationGenerator context =
  let broad = legacyGeneratorScope (ricAvailableBudgets context)
  in broad
      { risdOutput = (risdOutput broad)
          { rsoOwnedOverlay = ricOwnedOverlayName context /= Nothing }
      }
legacyFor InvocationSimulation context = legacySimulationScope
  (Set.toAscList (ricDependencyOverlayNames context)) (ricAvailableBudgets context)
legacyFor InvocationDataResource context = legacyGeneratorScope (ricAvailableBudgets context)

resolveSelector :: [Capability] -> RPCInvocationContext -> RPCChunkSelector -> Either ScopeError IntSet
resolveSelector capabilities context selector = case selector of
  SelectAllInvocationChunks -> Right (ricWorldChunkIds context)
  SelectCallerChunks
    | not (ricAllowsCallerChunks context) ->
        Left (ScopeError "chunks" "caller chunk selector is unavailable to this invocation kind")
    | otherwise -> case ricCallerChunkIds context of
        Just chunks -> Right chunks
        Nothing -> Left (ScopeError "chunks" "caller chunk IDs are not supplied by this invocation")
  SelectOverlayUnion names -> unionOverlayChunks capabilities context names
  SelectOverlayIntersection names -> intersectionOverlayChunks capabilities context names

selectorOverlayChunks :: [Capability] -> RPCInvocationContext -> Text -> Either ScopeError IntSet
selectorOverlayChunks capabilities context "$own"
  | ricOwnedOverlayName context == Nothing = Left (ScopeError "chunks.overlays" "own overlay is unavailable")
  | not (hasAny [CapReadOverlay, CapReadWorld, CapWriteOverlay, CapWriteWorld]) =
      Left (ScopeError "chunks.overlays" "own-overlay chunk selector requires owned-overlay access")
  | otherwise = Right (ricOwnOverlayChunkIds context)
  where hasAny requested = any (`elem` capabilities) requested
selectorOverlayChunks capabilities context name
  | Set.notMember name (ricDependencyOverlayNames context) =
      Left (ScopeError "chunks.overlays" ("overlay is not an invocation dependency: " <> name))
  | not (any (`elem` capabilities) [CapReadOverlay, CapReadWorld]) =
      Left (ScopeError "chunks.overlays" "dependency-overlay chunk selector requires readOverlay/readWorld")
  | otherwise = case Map.lookup name (ricOverlayChunkIds context) of
      Nothing -> Right IntSet.empty
      Just chunks -> Right chunks

unionOverlayChunks :: [Capability] -> RPCInvocationContext -> [Text] -> Either ScopeError IntSet
unionOverlayChunks _ _ [] = Left (ScopeError "chunks.overlays" "overlay union requires at least one overlay")
unionOverlayChunks capabilities context names = do
  chunks <- traverse (selectorOverlayChunks capabilities context) (Set.toAscList (Set.fromList names))
  Right (IntSet.unions chunks)

intersectionOverlayChunks :: [Capability] -> RPCInvocationContext -> [Text] -> Either ScopeError IntSet
intersectionOverlayChunks _ _ [] = Left (ScopeError "chunks.overlays" "overlay intersection requires at least one overlay")
intersectionOverlayChunks capabilities context names = do
  chunks <- traverse (selectorOverlayChunks capabilities context) (Set.toAscList (Set.fromList names))
  case chunks of
    [] -> Left (ScopeError "chunks.overlays" "overlay intersection requires at least one overlay")
    firstChunk:rest -> Right (foldl IntSet.intersection firstChunk rest)

resolveDataScope :: [Capability] -> RPCInvocationContext -> Either ScopeError ResolvedInvocationScope
resolveDataScope capabilities context = do
  request <- maybe (Left (ScopeError "dataResource" "data-resource invocation is missing its exact request")) Right
    (ricDataResourceRequest context)
  declaration <- maybe
    (Left (ScopeError "dataResource.resource" "requested resource is not declared"))
    Right
    (Map.lookup (rdrrResource request) (ricDataResourceDeclarations context))
  if rdrdName declaration /= rdrrResource request
    then Left (ScopeError "dataResource.resource" "resource map key does not match its declared name")
    else Right ()
  if Text.null (rdrdName declaration)
    then Left (ScopeError "dataResource.resource" "resource name must be non-empty")
    else Right ()
  if Set.notMember (rdrrOperation request) (rdrdOperations declaration)
    then Left (ScopeError "dataResource.operation" "requested operation is not declared for the resource")
    else Right ()
  let writeOperation = rdrrOperation request `elem` [DataCreate, DataUpdate, DataDelete]
      requiredCapability = if writeOperation then CapDataWrite else CapDataRead
  if requiredCapability `notElem` capabilities
    then Left (ScopeError "dataResource.operation" "required data capability is not granted")
    else Right ()
  validatePage declaration request
  validateQueryHex request
  let exactData = ResolvedDataResourceScope
        { rdrsResource = rdrrResource request
        , rdrsOperation = rdrrOperation request
        , rdrsPageOffset = rdrrPageOffset request
        , rdrsPageSize = rdrrPageSize request
        , rdrsQueryChunkId = rdrrQueryChunkId request
        , rdrsQueryTileIndex = rdrrQueryTileIndex request
        }
      unresolved = ResolvedInvocationScope
        { risScopeId = ""
        , risKind = InvocationDataResource
        , risTerrainInputSections = Set.empty
        , risTerrainInputChunkIds = IntSet.empty
        , risDependencyOverlayChunkIds = Map.empty
        , risOwnOverlayReadChunkIds = IntSet.empty
        , risTerrainOutputSections = Set.empty
        , risTerrainOutputChunkIds = IntSet.empty
        , risOwnedOverlayIdentity = Nothing
        , risOwnOverlayWriteChunkIds = IntSet.empty
        , risGeneratorMetadataOutput = False
        , risDataResource = Just exactData
        , risBudgets = RPCScopeBudgets
            { rsbTerrainBytes = 0
            , rsbOverlayBytes = 0
            , rsbOutputBytes = rsbOutputBytes (ricAvailableBudgets context)
            }
        }
  Right unresolved { risScopeId = resolvedInvocationScopeDigest unresolved }

validatePage :: RPCDataResourceDeclaration -> RPCDataResourceRequest -> Either ScopeError ()
validatePage declaration request = do
  case rdrrPageOffset request of
    Just offset | offset < 0 -> Left (ScopeError "dataResource.pageOffset" "page offset must be non-negative")
    _ -> Right ()
  case rdrrPageSize request of
    Just size | size <= 0 -> Left (ScopeError "dataResource.pageSize" "page size must be positive")
    Just size | size > rdrdMaxPageSize declaration -> Left (ScopeError "dataResource.pageSize" "page size exceeds the declared resource maximum")
    _ -> Right ()

validateQueryHex :: RPCDataResourceRequest -> Either ScopeError ()
validateQueryHex request
  | rdrrOperation request == DataQueryByHex = case (rdrrQueryChunkId request, rdrrQueryTileIndex request) of
      (Just chunkId, Just tileIndex)
        | chunkId >= 0 && tileIndex >= 0 -> Right ()
      _ -> Left (ScopeError "dataResource.queryByHex" "QueryByHex requires exact non-negative chunk and tile IDs")
  | rdrrQueryChunkId request /= Nothing || rdrrQueryTileIndex request /= Nothing =
      Left (ScopeError "dataResource.queryByHex" "chunk/tile IDs are only valid for QueryByHex")
  | otherwise = Right ()

intersectBudgets :: RPCScopeBudgets -> RPCScopeBudgets -> RPCScopeBudgets
intersectBudgets declared available = RPCScopeBudgets
  { rsbTerrainBytes = min (rsbTerrainBytes declared) (rsbTerrainBytes available)
  , rsbOverlayBytes = min (rsbOverlayBytes declared) (rsbOverlayBytes available)
  , rsbOutputBytes = min (rsbOutputBytes declared) (rsbOutputBytes available)
  }

-- | Stable SHA-256 over a canonical, sorted textual descriptor. The scope ID
-- itself is excluded, so inline descriptors can be verified independently.
resolvedInvocationScopeDigest :: ResolvedInvocationScope -> Text
resolvedInvocationScopeDigest scope = Text.pack (show digest)
  where
    digest :: Digest SHA256
    digest = hash (TextEncoding.encodeUtf8 (canonicalScope scope))

canonicalScope :: ResolvedInvocationScope -> Text
canonicalScope scope = frameList
  [ invocationKindText (risKind scope)
  , frameList (map terrainSectionText (Set.toAscList (risTerrainInputSections scope)))
  , frameList (map showText (IntSet.toAscList (risTerrainInputChunkIds scope)))
  , frameList
      [frameList [name, frameList (map showText (IntSet.toAscList chunks))]
      | (name, chunks) <- Map.toAscList (risDependencyOverlayChunkIds scope)]
  , frameList (map showText (IntSet.toAscList (risOwnOverlayReadChunkIds scope)))
  , frameList (map terrainSectionText (Set.toAscList (risTerrainOutputSections scope)))
  , frameList (map showText (IntSet.toAscList (risTerrainOutputChunkIds scope)))
  , canonicalMaybeText (risOwnedOverlayIdentity scope)
  , frameList (map showText (IntSet.toAscList (risOwnOverlayWriteChunkIds scope)))
  , if risGeneratorMetadataOutput scope then "1" else "0"
  , maybe "0" (("1" <>) . canonicalData) (risDataResource scope)
  , frameList (map showText
      [ rsbTerrainBytes (risBudgets scope)
      , rsbOverlayBytes (risBudgets scope)
      , rsbOutputBytes (risBudgets scope)
      ])
  ]

canonicalData :: ResolvedDataResourceScope -> Text
canonicalData scope = frameList
  [ rdrsResource scope
  , dataOperationText (rdrsOperation scope)
  , canonicalMaybeShow (rdrsPageOffset scope)
  , canonicalMaybeShow (rdrsPageSize scope)
  , canonicalMaybeShow (rdrsQueryChunkId scope)
  , canonicalMaybeShow (rdrsQueryTileIndex scope)
  ]

canonicalMaybeText :: Maybe Text -> Text
canonicalMaybeText Nothing = "0"
canonicalMaybeText (Just value) = "1" <> frameList [value]

canonicalMaybeShow :: Show a => Maybe a -> Text
canonicalMaybeShow Nothing = "0"
canonicalMaybeShow (Just value) = "1" <> frameList [showText value]

frameList :: [Text] -> Text
frameList = Text.concat . map frame
  where
    frame value = showText (BS.length (TextEncoding.encodeUtf8 value)) <> ":" <> value

-- | Validate protocol binding semantics. Protocol-v4 permits omission;
-- protocol-v5 requires a binding and checks both reference and descriptor ID.
validateInvocationScopeBinding
  :: Int
  -> Maybe ResolvedInvocationScope
  -> Maybe RPCInvocationScopeBinding
  -> Either ScopeError ()
validateInvocationScopeBinding protocolVersion expected binding
  | protocolVersion <= 4 = validatePresent binding
  | otherwise = case (expected, binding) of
      (Nothing, _) -> Left (ScopeError "invocation_scope" "protocol 5 binding validation requires the host-resolved scope")
      (_, Nothing) -> Left (ScopeError "invocation_scope" "protocol 5 invocation is missing its scope binding")
      (_, Just value) -> validatePresent (Just value)
  where
    validatePresent Nothing = Right ()
    validatePresent (Just value)
      | Text.null (risbScopeId value) = Left (ScopeError "invocation_scope.scopeId" "scope ID must be non-empty")
      | Just expectedScope <- expected
      , risbScopeId value /= risScopeId expectedScope =
          Left (ScopeError "invocation_scope.scopeId" "scope reference does not match the host-resolved invocation scope")
      | otherwise = case risbDescriptor value of
          Nothing -> Right ()
          Just descriptor
            | maybe False (/= descriptor) expected -> Left (ScopeError "invocation_scope.descriptor" "inline descriptor does not match the host-resolved invocation scope")
            | maybe False Text.null (risOwnedOverlayIdentity descriptor) -> Left (ScopeError "invocation_scope.descriptor.ownedOverlayIdentity" "owned overlay identity must be non-empty")
            | risbScopeId value /= risScopeId descriptor -> Left (ScopeError "invocation_scope" "scope reference does not match inline descriptor")
            | resolvedInvocationScopeDigest descriptor /= risScopeId descriptor -> Left (ScopeError "invocation_scope.descriptor" "inline descriptor digest is invalid")
            | otherwise -> Right ()

terrainSectionText :: TerrainSection -> Text
terrainSectionText TerrainElevation = "terrain"
terrainSectionText TerrainClimate = "climate"
terrainSectionText TerrainVegetation = "vegetation"

invocationKindText :: RPCInvocationKind -> Text
invocationKindText InvocationGenerator = "generator"
invocationKindText InvocationSimulation = "simulation"
invocationKindText InvocationDataResource = "data_resource"

dataOperationText :: RPCDataOperation -> Text
dataOperationText DataList = "list"
dataOperationText DataGet = "get"
dataOperationText DataCreate = "create"
dataOperationText DataUpdate = "update"
dataOperationText DataDelete = "delete"
dataOperationText DataQueryByHex = "queryByHex"
dataOperationText DataQueryByField = "queryByField"

showText :: Show a => a -> Text
showText = Text.pack . show
