{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Plugin manifest parsing and validation.
--
-- A plugin manifest is a JSON file (@manifest.json@) that declares
-- the plugin's identity, capabilities, pipeline participation
-- (generator and\/or simulation), overlay schema location, user-facing
-- configuration parameters, and backend-neutral external data-source
-- declarations/grants whose backing migrations, schemas, connection details,
-- and consistency rules stay owned by providers or external systems.
--
-- = Example manifest
--
-- @
-- {
--   "manifestVersion": 3,
--   "name": "civilization",
--   "version": "1.0.0",
--   "runtime": { "protocol": { "min": 4, "max": 4 } },
--   "description": "Civilization simulation overlay",
--   "generator": { "insertAfter": "biomes", "requires": ["biomes","rivers"] },
--   "simulation": { "dependencies": ["weather"], "interval_ticks": 1, "phase_ticks": 0, "catch_up": "run_once_if_due" },
--   "overlay": { "schemaFile": "civilization.toposchema" },
--   "capabilities": ["readTerrain","readOverlay","writeOverlay","log"],
--   "config": { "parameters": [...] },
--   "externalDataSources": [...]
-- }
-- @
module Topo.Plugin.RPC.Manifest
  ( -- * Manifest types
    RPCManifest(..)
  , manifestV3
  , RPCManifestRuntime(..)
  , defaultRPCManifestRuntime
  , RPCUIHints(..)
  , defaultRPCUIHints
  , RPCGeneratorDecl(..)
  , RPCSimulationDecl(..)
  , RPCOverlayDecl(..)
  , RPCStartPolicy(..)
  , RPCRestartMode(..)
  , defaultRPCStartPolicy
  , Capability(..)
  , RPCCapability
  , RPCParamSpec(..)
  , RPCParamType(..)
  , RPCParamValidationError(..)
  , rpcParamDefaults
  , sanitizeRPCParamMap
  , sanitizeRPCManifestParams
  , validateRPCParamUpdate
  , validateRPCParamValue
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceConfigOrigin(..)
  , RPCExternalDataSourceConfigRef(..)
  , externalAccessRequiredCapabilities
  , RPCExternalDataSourceStatusState(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceStatus(..)
  , defaultRPCExternalDataSourceStatus
  , externalDataSourceStatusCurrent
  , observeExternalDataSourceStatus
  , staleExternalDataSourceStatus
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceRef(..)
    -- * Schema and examples
  , manifestV3Schema
  , manifestV3ProviderExample
  , manifestV3ConsumerExample
    -- * Parsing
  , parseManifest
  , parseManifestFile
    -- * Validation
  , ManifestError(..)
  , manifestErrorMessage
  , renderManifestErrors
  , validateManifest
  , validateHandshakeDataResources
    -- * Queries
  , manifestWritesTerrain
  , manifestHasGenerator
  , manifestHasSimulation
  , manifestHasOverlay
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.:?)
  , (.=)
  , object
  , withObject
  , withText
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Topo.Plugin (Capability(..))
import Topo.Plugin.DataResource
  ( DataFieldDef(..)
  , DataOperations(..)
  , DataPagination(..)
  , DataResourceError(..)
  , DataResourceSchema(..)
  , validateDataResource
  )
import Topo.Plugin.RPC.Protocol (currentProtocolVersion)
import Topo.Simulation.Schedule
  ( SimulationCatchUpPolicy(..)
  , SimulationScheduleDecl(..)
  , catchUpPolicyText
  , defaultScheduleDecl
  , scheduleDeclError
  )

------------------------------------------------------------------------
-- Capability
------------------------------------------------------------------------

-- | Capabilities a plugin may request.  The host validates these
-- against the manifest and only provides matching data.
type RPCCapability = Capability

------------------------------------------------------------------------
-- Parameter specification
------------------------------------------------------------------------

-- | Scalar type for a plugin parameter.
data RPCParamType
  = ParamFloat
  | ParamInt
  | ParamBool
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCParamType where
  parseJSON = withText "RPCParamType" $ \t -> case t of
    "float" -> pure ParamFloat
    "int"   -> pure ParamInt
    "bool"  -> pure ParamBool
    _       -> fail ("unknown parameter type: " <> Text.unpack t)

instance ToJSON RPCParamType where
  toJSON ParamFloat = "float"
  toJSON ParamInt   = "int"
  toJSON ParamBool  = "bool"

-- | A user-facing configuration parameter declared by a plugin.
-- These are rendered as sliders\/checkboxes in topo-seer.
data RPCParamSpec = RPCParamSpec
  { rpsName    :: !Text
    -- ^ Internal parameter name (used as key in config maps).
  , rpsLabel   :: !Text
    -- ^ Human-readable label for UI display.
  , rpsType    :: !RPCParamType
    -- ^ Scalar type.
  , rpsRange   :: !(Maybe (Value, Value))
    -- ^ Optional @(min, max)@ range for numeric parameters.
  , rpsDefault :: !Value
    -- ^ Default value (JSON scalar).
  , rpsTooltip :: !Text
    -- ^ Tooltip text for UI hover.
  } deriving (Eq, Show, Generic)

instance FromJSON RPCParamSpec where
  parseJSON = withObject "RPCParamSpec" $ \o -> do
    name    <- o .:  "name"
    label   <- o .:  "label"
    ty      <- o .:  "type"
    rangeValue <- o .:? "range"
    def     <- o .:  "default"
    tooltip <- o .:? "tooltip"
    parsedRange <- case rangeValue of
      Nothing -> pure Nothing
      Just (Aeson.Array arr)
        | [lo, hi] <- toList arr -> pure (Just (lo, hi))
      Just _ -> fail "range must be an array with exactly two values"
    pure RPCParamSpec
      { rpsName    = name
      , rpsLabel   = label
      , rpsType    = ty
      , rpsRange   = parsedRange
      , rpsDefault = def
      , rpsTooltip = maybe "" id tooltip
      }
    where
      toList :: Aeson.Array -> [Value]
      toList = foldr (:) []

instance ToJSON RPCParamSpec where
  toJSON rps = object $
    [ "name"    .= rpsName rps
    , "label"   .= rpsLabel rps
    , "type"    .= rpsType rps
    , "default" .= rpsDefault rps
    ] <>
    [ "range"   .= Aeson.Array (fromList [lo, hi])
    | Just (lo, hi) <- [rpsRange rps]
    ] <>
    [ "tooltip" .= rpsTooltip rps
    | rpsTooltip rps /= ""
    ]
    where
      fromList :: [Value] -> Aeson.Array
      fromList = foldMap (\v -> pure v)

-- | Runtime parameter validation failure with a transport-neutral detail path.
data RPCParamValidationError = RPCParamValidationError
  { rpvPath :: ![Text]
  , rpvCode :: !Text
  , rpvMessage :: !Text
  } deriving (Eq, Show, Generic)

-- | Manifest defaults keyed by parameter name.
rpcParamDefaults :: [RPCParamSpec] -> Map Text Value
rpcParamDefaults specs = Map.fromList
  [ (rpsName spec, rpsDefault spec)
  | spec <- specs
  ]

-- | Validate a single parameter value against its manifest declaration.
validateRPCParamValue :: RPCParamSpec -> Value -> Either RPCParamValidationError Value
validateRPCParamValue spec value = case rpsType spec of
  ParamBool -> case value of
    Bool _ -> Right value
    _ -> Left (valueValidationError spec "invalid_type" "expected a boolean value")
  ParamFloat -> case numericValue value of
    Nothing -> Left (valueValidationError spec "invalid_type" "expected a numeric value")
    Just n -> validateFloatRange spec n value
  ParamInt -> case integerValue value of
    Nothing -> Left (valueValidationError spec code message)
      where
        (code, message) = case value of
          Number _ -> ("invalid_integer", "expected an integral numeric value")
          _ -> ("invalid_type", "expected an integral numeric value")
    Just n -> validateIntRange spec n value

-- | Validate an update by parameter name, reporting unknown names on @param@.
validateRPCParamUpdate :: [RPCParamSpec] -> Text -> Value -> Either RPCParamValidationError Value
validateRPCParamUpdate specs paramName value =
  case findParamSpec paramName specs of
    Nothing -> Left RPCParamValidationError
      { rpvPath = ["param"]
      , rpvCode = "unknown_param"
      , rpvMessage = "unknown plugin parameter: " <> paramName
      }
    Just spec -> validateRPCParamValue spec value

-- | Sanitize a value map to exactly the known manifest keys, falling back to
-- defaults for missing or invalid values and dropping unknown saved keys.
sanitizeRPCParamMap :: [RPCParamSpec] -> Map Text Value -> Map Text Value
sanitizeRPCParamMap specs params = Map.fromList
  [ (rpsName spec, sanitizeOne spec)
  | spec <- specs
  ]
  where
    sanitizeOne spec =
      let candidate = Map.findWithDefault (rpsDefault spec) (rpsName spec) params
      in case validateRPCParamValue spec candidate of
        Right sanitized -> sanitized
        Left _ -> rpsDefault spec

-- | Sanitize a parameter map using the parameter declarations in a manifest.
sanitizeRPCManifestParams :: RPCManifest -> Map Text Value -> Map Text Value
sanitizeRPCManifestParams manifest = sanitizeRPCParamMap (rmParameters manifest)

findParamSpec :: Text -> [RPCParamSpec] -> Maybe RPCParamSpec
findParamSpec _ [] = Nothing
findParamSpec paramName (spec:rest)
  | rpsName spec == paramName = Just spec
  | otherwise = findParamSpec paramName rest

validateFloatRange spec n value = case rpsRange spec of
  Nothing -> Right value
  Just (loValue, hiValue) -> case (numericValue loValue, numericValue hiValue) of
    (Just lo, Just hi)
      | n >= lo && n <= hi -> Right value
      | otherwise -> Left (valueValidationError spec "out_of_range" (rangeMessage lo hi))
    _ -> Left (valueValidationError spec "invalid_spec" "parameter range bounds must be numeric")

validateIntRange :: RPCParamSpec -> Integer -> Value -> Either RPCParamValidationError Value
validateIntRange spec n value = case rpsRange spec of
  Nothing -> Right value
  Just (loValue, hiValue) -> case (integerValue loValue, integerValue hiValue) of
    (Just lo, Just hi)
      | n >= lo && n <= hi -> Right value
      | otherwise -> Left (valueValidationError spec "out_of_range" (rangeMessage lo hi))
    _ -> Left (valueValidationError spec "invalid_spec" "integer parameter range bounds must be integral numbers")

valueValidationError :: RPCParamSpec -> Text -> Text -> RPCParamValidationError
valueValidationError spec code message = RPCParamValidationError
  { rpvPath = ["value"]
  , rpvCode = code
  , rpvMessage = "parameter '" <> rpsName spec <> "' " <> message
  }

rangeMessage :: Show a => a -> a -> Text
rangeMessage lo hi = "must be within inclusive range [" <> Text.pack (show lo) <> ", " <> Text.pack (show hi) <> "]"

numericValue (Number n) = Just n
numericValue _ = Nothing

integerValue :: Value -> Maybe Integer
integerValue value@(Number _) = case (Aeson.fromJSON value :: Aeson.Result Integer) of
  Aeson.Success n -> Just n
  Aeson.Error _ -> Nothing
integerValue _ = Nothing

------------------------------------------------------------------------
-- Generator declaration
------------------------------------------------------------------------

-- | Generator pipeline participation.
data RPCGeneratorDecl = RPCGeneratorDecl
  { rgdInsertAfter :: !Text
    -- ^ Canonical stage name after which this plugin's generator stage
    -- is inserted (e.g. @\"biomes\"@).
  , rgdRequires    :: ![Text]
    -- ^ Canonical stage names that must have run before this plugin
    -- (dependency declarations).
  } deriving (Eq, Show, Generic)

instance FromJSON RPCGeneratorDecl where
  parseJSON = withObject "RPCGeneratorDecl" $ \o ->
    RPCGeneratorDecl
      <$> o .:  "insertAfter"
      <*> (o .:? "requires" >>= pure . maybe [] id)

instance ToJSON RPCGeneratorDecl where
  toJSON rgd = object
    [ "insertAfter" .= rgdInsertAfter rgd
    , "requires"    .= rgdRequires rgd
    ]

------------------------------------------------------------------------
-- Simulation declaration
------------------------------------------------------------------------

-- | Simulation DAG participation.
data RPCSimulationDecl = RPCSimulationDecl
  { rsdDependencies :: ![Text]
    -- ^ Simulation node IDs that must tick before this plugin's sim node.
  , rsdSchedule :: !SimulationScheduleDecl
    -- ^ Static cadence declaration for this plugin's sim node.
  } deriving (Eq, Show, Generic)

instance FromJSON RPCSimulationDecl where
  parseJSON = withObject "RPCSimulationDecl" $ \o -> do
    dependencies <- o .:? "dependencies" >>= pure . maybe [] id
    interval <- optionalScheduleField o ["interval_ticks", "intervalTicks"] (schedDeclIntervalTicks defaultScheduleDecl)
    phase <- optionalScheduleField o ["phase_ticks", "phaseTicks"] (schedDeclPhaseTicks defaultScheduleDecl)
    catchUp <- optionalScheduleCatchUp o ["catch_up", "catchUp"] (schedDeclCatchUpPolicy defaultScheduleDecl)
    pure RPCSimulationDecl
      { rsdDependencies = dependencies
      , rsdSchedule = SimulationScheduleDecl
          { schedDeclIntervalTicks = interval
          , schedDeclPhaseTicks = phase
          , schedDeclCatchUpPolicy = catchUp
          }
      }

instance ToJSON RPCSimulationDecl where
  toJSON rsd = object
    [ "dependencies" .= rsdDependencies rsd
    , "interval_ticks" .= schedDeclIntervalTicks schedule
    , "phase_ticks" .= schedDeclPhaseTicks schedule
    , "catch_up" .= catchUpPolicyText (schedDeclCatchUpPolicy schedule)
    ]
    where
      schedule = rsdSchedule rsd

------------------------------------------------------------------------
-- Overlay declaration
------------------------------------------------------------------------

-- | Overlay schema reference.
data RPCOverlayDecl = RPCOverlayDecl
  { rodSchemaFile :: !Text
    -- ^ Path to the @.toposchema@ file (relative to plugin directory).
  } deriving (Eq, Show, Generic)

instance FromJSON RPCOverlayDecl where
  parseJSON = withObject "RPCOverlayDecl" $ \o ->
    RPCOverlayDecl <$> o .: "schemaFile"

instance ToJSON RPCOverlayDecl where
  toJSON rod = object [ "schemaFile" .= rodSchemaFile rod ]

------------------------------------------------------------------------
-- Start/restart policy
------------------------------------------------------------------------

-- | How the host should restart a plugin subprocess after startup or
-- runtime failure.
data RPCRestartMode
  = RestartNever
  | RestartOnFailure
  | RestartAlways
  deriving (Eq, Ord, Show, Generic)

instance FromJSON RPCRestartMode where
  parseJSON = withText "RPCRestartMode" $ \raw ->
    case normalizeRestartMode raw of
      "never"      -> pure RestartNever
      "on_failure" -> pure RestartOnFailure
      "always"     -> pure RestartAlways
      _            -> fail ("unknown restart mode: " <> Text.unpack raw)

instance ToJSON RPCRestartMode where
  toJSON RestartNever     = "never"
  toJSON RestartOnFailure = "on_failure"
  toJSON RestartAlways    = "always"

normalizeRestartMode :: Text -> Text
normalizeRestartMode = Text.replace "-" "_" . Text.toLower

-- | Host-side process policy for a plugin.  Timeouts and backoff values are
-- milliseconds; a positive default is used so plugin failures cannot block the
-- host indefinitely.
data RPCStartPolicy = RPCStartPolicy
  { rspAutoStart         :: !Bool
  , rspRestartMode       :: !RPCRestartMode
  , rspMaxRestarts       :: !Int
  , rspRestartWindowMs   :: !Int
  , rspStartupTimeoutMs  :: !Int
  , rspRequestTimeoutMs  :: !Int
  , rspShutdownTimeoutMs :: !Int
  , rspBackoffMs         :: !Int
  } deriving (Eq, Show, Generic)

defaultRPCStartPolicy :: RPCStartPolicy
defaultRPCStartPolicy = RPCStartPolicy
  { rspAutoStart = True
  , rspRestartMode = RestartOnFailure
  , rspMaxRestarts = 3
  , rspRestartWindowMs = 60000
  , rspStartupTimeoutMs = 1000
  , rspRequestTimeoutMs = 5000
  , rspShutdownTimeoutMs = 1000
  , rspBackoffMs = 100
  }

instance FromJSON RPCStartPolicy where
  parseJSON = withObject "RPCStartPolicy" $ \o -> do
    autoStart <- optionalPolicyField o ["auto_start", "autoStart"] (rspAutoStart defaultRPCStartPolicy)
    restartMode <- optionalPolicyField o ["restart", "restart_mode", "restartMode"] (rspRestartMode defaultRPCStartPolicy)
    maxRestarts <- optionalPolicyField o ["max_restarts", "maxRestarts"] (rspMaxRestarts defaultRPCStartPolicy) >>= nonNegative "max_restarts"
    restartWindowMs <- optionalPolicyField o ["restart_window_ms", "restartWindowMs"] (rspRestartWindowMs defaultRPCStartPolicy) >>= positive "restart_window_ms"
    startupTimeoutMs <- optionalPolicyField o ["startup_timeout_ms", "startupTimeoutMs"] (rspStartupTimeoutMs defaultRPCStartPolicy) >>= positive "startup_timeout_ms"
    requestTimeoutMs <- optionalPolicyField o ["request_timeout_ms", "requestTimeoutMs"] (rspRequestTimeoutMs defaultRPCStartPolicy) >>= positive "request_timeout_ms"
    shutdownTimeoutMs <- optionalPolicyField o ["shutdown_timeout_ms", "shutdownTimeoutMs"] (rspShutdownTimeoutMs defaultRPCStartPolicy) >>= positive "shutdown_timeout_ms"
    backoffMs <- optionalPolicyField o ["backoff_ms", "backoffMs"] (rspBackoffMs defaultRPCStartPolicy) >>= nonNegative "backoff_ms"
    pure RPCStartPolicy
      { rspAutoStart = autoStart
      , rspRestartMode = restartMode
      , rspMaxRestarts = maxRestarts
      , rspRestartWindowMs = restartWindowMs
      , rspStartupTimeoutMs = startupTimeoutMs
      , rspRequestTimeoutMs = requestTimeoutMs
      , rspShutdownTimeoutMs = shutdownTimeoutMs
      , rspBackoffMs = backoffMs
      }

instance ToJSON RPCStartPolicy where
  toJSON policy = object
    [ "auto_start"          .= rspAutoStart policy
    , "restart_mode"        .= rspRestartMode policy
    , "max_restarts"        .= rspMaxRestarts policy
    , "restart_window_ms"   .= rspRestartWindowMs policy
    , "startup_timeout_ms"  .= rspStartupTimeoutMs policy
    , "request_timeout_ms"  .= rspRequestTimeoutMs policy
    , "shutdown_timeout_ms" .= rspShutdownTimeoutMs policy
    , "backoff_ms"          .= rspBackoffMs policy
    ]

optionalPolicyField :: FromJSON a => Aeson.Object -> [Text] -> a -> Parser a
optionalPolicyField o aliases fallback =
  case lookupAliasedField o aliases of
    Nothing -> pure fallback
    Just (_, value) -> parseJSON value

optionalNonNullField :: FromJSON a => Aeson.Object -> [Text] -> Parser (Maybe a)
optionalNonNullField o aliases =
  case lookupAliasedField o aliases of
    Nothing -> pure Nothing
    Just (alias, Null) -> fail (Text.unpack alias <> " must not be null")
    Just (_, value) -> Just <$> parseJSON value

optionalNonNullListField :: FromJSON a => Aeson.Object -> [Text] -> Parser [a]
optionalNonNullListField o aliases =
  case lookupAliasedField o aliases of
    Nothing -> pure []
    Just (alias, Null) -> fail (Text.unpack alias <> " must not be null")
    Just (_, value) -> parseJSON value

optionalScheduleField :: Aeson.Object -> [Text] -> Word64 -> Parser Word64
optionalScheduleField o aliases fallback =
  case lookupAliasedField o aliases of
    Nothing -> pure fallback
    Just (alias, Null) -> fail (Text.unpack alias <> " must not be null")
    Just (_, value) -> parseJSON value

optionalScheduleCatchUp :: Aeson.Object -> [Text] -> SimulationCatchUpPolicy -> Parser SimulationCatchUpPolicy
optionalScheduleCatchUp o aliases fallback =
  case lookupAliasedField o aliases of
    Nothing -> pure fallback
    Just (alias, Null) -> fail (Text.unpack alias <> " must not be null")
    Just (_, value) -> parseJSON value >>= parseCatchUpPolicyText

parseCatchUpPolicyText :: Text -> Parser SimulationCatchUpPolicy
parseCatchUpPolicyText raw =
  case Text.replace "-" "_" (Text.toLower raw) of
    "run_once_if_due" -> pure RunOnceIfDue
    "runonceifdue" -> pure RunOnceIfDue
    "skip_missed" -> pure SkipMissed
    "skipmissed" -> pure SkipMissed
    normalized -> fail ("unknown simulation catch_up policy: " <> Text.unpack normalized)

lookupAliasedField :: Aeson.Object -> [Text] -> Maybe (Text, Value)
lookupAliasedField o aliases =
  listToMaybe [(alias, value) | alias <- aliases, Just value <- [KM.lookup (Key.fromText alias) o]]

nonNegative :: String -> Int -> Parser Int
nonNegative field value
  | value >= 0 = pure value
  | otherwise = fail (field <> " must be non-negative")

positive :: String -> Int -> Parser Int
positive field value
  | value > 0 = pure value
  | otherwise = fail (field <> " must be positive")

------------------------------------------------------------------------
-- Manifest v3 metadata, UI hints, and external data sources
------------------------------------------------------------------------

-- | Current plugin manifest contract version.
manifestV3 :: Int
manifestV3 = 3

-- | Runtime and RPC protocol bounds declared by a plugin manifest.
--
-- Protocol bounds are integer message-contract versions.  Topo version bounds
-- are opaque version strings so the manifest stays independent of a package or
-- release-version parser.
data RPCManifestRuntime = RPCManifestRuntime
  { rmrProtocolMin :: !Int
  , rmrProtocolMax :: !Int
  , rmrTopoMin     :: !(Maybe Text)
  , rmrTopoMax     :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

defaultRPCManifestRuntime :: RPCManifestRuntime
defaultRPCManifestRuntime = RPCManifestRuntime
  { rmrProtocolMin = currentProtocolVersion
  , rmrProtocolMax = currentProtocolVersion
  , rmrTopoMin = Nothing
  , rmrTopoMax = Nothing
  }

instance FromJSON RPCManifestRuntime where
  parseJSON = withObject "RPCManifestRuntime" $ \o -> do
    protocol <- o .: "protocol"
    (pmin, pmax) <- withObject "runtime.protocol" (\p -> do
        pmin <- p .: "min"
        pmax <- p .: "max"
        pure (pmin, pmax)
      ) protocol
    topo <- o .:? "topo"
    (tmin, tmax) <- case topo of
      Nothing -> pure (Nothing, Nothing)
      Just value -> withObject "runtime.topo" (\t -> do
          tmin <- t .:? "min"
          tmax <- t .:? "max"
          pure (tmin, tmax)
        ) value
    pure RPCManifestRuntime
      { rmrProtocolMin = pmin
      , rmrProtocolMax = pmax
      , rmrTopoMin = tmin
      , rmrTopoMax = tmax
      }

instance ToJSON RPCManifestRuntime where
  toJSON runtime = object $
    [ "protocol" .= object
        [ "min" .= rmrProtocolMin runtime
        , "max" .= rmrProtocolMax runtime
        ]
    ] <>
    [ "topo" .= object (topoMin <> topoMax)
    | not (null (topoMin <> topoMax))
    ]
    where
      topoMin = ["min" .= v | Just v <- [rmrTopoMin runtime]]
      topoMax = ["max" .= v | Just v <- [rmrTopoMax runtime]]

-- | Optional user-interface hints for manifests and manifest sub-declarations.
data RPCUIHints = RPCUIHints
  { ruiDisplayName :: !(Maybe Text)
  , ruiCategory    :: !(Maybe Text)
  , ruiTags        :: ![Text]
  , ruiIcon        :: !(Maybe Text)
  , ruiDocsUrl     :: !(Maybe Text)
  , ruiOrder       :: !(Maybe Int)
  } deriving (Eq, Show, Generic)

defaultRPCUIHints :: RPCUIHints
defaultRPCUIHints = RPCUIHints
  { ruiDisplayName = Nothing
  , ruiCategory = Nothing
  , ruiTags = []
  , ruiIcon = Nothing
  , ruiDocsUrl = Nothing
  , ruiOrder = Nothing
  }

instance FromJSON RPCUIHints where
  parseJSON = withObject "RPCUIHints" $ \o ->
    RPCUIHints
      <$> o .:? "displayName"
      <*> o .:? "category"
      <*> (o .:? "tags" >>= pure . maybe [] id)
      <*> o .:? "icon"
      <*> o .:? "docsUrl"
      <*> o .:? "order"

instance ToJSON RPCUIHints where
  toJSON ui = object $
    [ "displayName" .= v | Just v <- [ruiDisplayName ui] ] <>
    [ "category" .= v | Just v <- [ruiCategory ui] ] <>
    [ "tags" .= ruiTags ui | not (null (ruiTags ui)) ] <>
    [ "icon" .= v | Just v <- [ruiIcon ui] ] <>
    [ "docsUrl" .= v | Just v <- [ruiDocsUrl ui] ] <>
    [ "order" .= v | Just v <- [ruiOrder ui] ]

-- | Backend-neutral capabilities offered by a provider-owned external data
-- source.  These describe host/plugin coordination only.  Even @migrate@ is a
-- provider-advertised capability/status boundary; topo core does not define
-- backend migration tables, schema evolution rules, connection details, or
-- consistency policy.
data RPCExternalDataSourceCapability
  = ExternalSourceQuery
  | ExternalSourceMutate
  | ExternalSourceSubscribe
  | ExternalSourceMigrate
  | ExternalSourceHealth
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCExternalDataSourceCapability where
  parseJSON = withText "RPCExternalDataSourceCapability" $ \t -> case t of
    "query"     -> pure ExternalSourceQuery
    "mutate"    -> pure ExternalSourceMutate
    "subscribe" -> pure ExternalSourceSubscribe
    "migrate"   -> pure ExternalSourceMigrate
    "health"    -> pure ExternalSourceHealth
    _            -> fail ("unknown external data source capability: " <> Text.unpack t)

instance ToJSON RPCExternalDataSourceCapability where
  toJSON ExternalSourceQuery     = "query"
  toJSON ExternalSourceMutate    = "mutate"
  toJSON ExternalSourceSubscribe = "subscribe"
  toJSON ExternalSourceMigrate   = "migrate"
  toJSON ExternalSourceHealth    = "health"

-- | Access requested by a consumer reference to an external data source.
data RPCExternalDataSourceAccess
  = ExternalAccessRead
  | ExternalAccessWrite
  | ExternalAccessAdmin
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCExternalDataSourceAccess where
  parseJSON = withText "RPCExternalDataSourceAccess" $ \t -> case t of
    "read"  -> pure ExternalAccessRead
    "write" -> pure ExternalAccessWrite
    "admin" -> pure ExternalAccessAdmin
    _       -> fail ("unknown external data source access: " <> Text.unpack t)

instance ToJSON RPCExternalDataSourceAccess where
  toJSON ExternalAccessRead  = "read"
  toJSON ExternalAccessWrite = "write"
  toJSON ExternalAccessAdmin = "admin"

-- | Origin for an opaque external data-source configuration reference.
--
-- The origin says who supplies the binding key; topo stores and reports the
-- key without interpreting it as a file path, connection string, backend, or
-- provider implementation detail.
data RPCExternalDataSourceConfigOrigin
  = ExternalConfigUser
  | ExternalConfigProvider
  | ExternalConfigEnvironment
  | ExternalConfigDeployment
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCExternalDataSourceConfigOrigin where
  parseJSON = withText "RPCExternalDataSourceConfigOrigin" $ \t -> case t of
    "user"        -> pure ExternalConfigUser
    "provider"    -> pure ExternalConfigProvider
    "environment" -> pure ExternalConfigEnvironment
    "deployment"  -> pure ExternalConfigDeployment
    _             -> fail ("unknown external data source config origin: " <> Text.unpack t)

instance ToJSON RPCExternalDataSourceConfigOrigin where
  toJSON ExternalConfigUser        = "user"
  toJSON ExternalConfigProvider    = "provider"
  toJSON ExternalConfigEnvironment = "environment"
  toJSON ExternalConfigDeployment  = "deployment"

-- | Opaque, backend-neutral configuration reference for an external data
-- source, grant, or consumer binding.
--
-- 'redscrKey' is an identifier in the named origin (for example a user config
-- key, provider handle, environment variable name, or deployment binding). Topo
-- may persist the key, compatibility marker, and opaque metadata for world
-- compatibility checks, but it must not interpret backend-specific layouts,
-- migrations, schemas, or connection details from the reference.
data RPCExternalDataSourceConfigRef = RPCExternalDataSourceConfigRef
  { redscrName          :: !Text
  , redscrOrigin        :: !RPCExternalDataSourceConfigOrigin
  , redscrKey           :: !Text
  , redscrRequired      :: !Bool
  , redscrCompatibility :: !(Maybe Text)
  , redscrMetadata      :: !(Maybe Value)
  } deriving (Eq, Show, Generic)

instance FromJSON RPCExternalDataSourceConfigRef where
  parseJSON = withObject "RPCExternalDataSourceConfigRef" $ \o ->
    RPCExternalDataSourceConfigRef
      <$> o .: "name"
      <*> o .: "origin"
      <*> o .: "key"
      <*> (o .:? "required" >>= pure . maybe True id)
      <*> optionalNonNullField o ["compatibility"]
      <*> optionalNonNullField o ["metadata"]

instance ToJSON RPCExternalDataSourceConfigRef where
  toJSON configRef = object $
    [ "name" .= redscrName configRef
    , "origin" .= redscrOrigin configRef
    , "key" .= redscrKey configRef
    , "required" .= redscrRequired configRef
    ] <>
    [ "compatibility" .= compatibility | Just compatibility <- [redscrCompatibility configRef] ] <>
    [ "metadata" .= metadata | Just metadata <- [redscrMetadata configRef] ]

-- | Backend-neutral source capabilities required before topo may broker an
-- access grant.  This gate is intentionally generic: providers still own the
-- concrete backend authorization, locking, and writer policy behind the grant.
externalAccessRequiredCapabilities :: RPCExternalDataSourceAccess -> [RPCExternalDataSourceCapability]
externalAccessRequiredCapabilities ExternalAccessRead = [ExternalSourceQuery]
externalAccessRequiredCapabilities ExternalAccessWrite = [ExternalSourceMutate]
externalAccessRequiredCapabilities ExternalAccessAdmin = [ExternalSourceMigrate]

-- | Declarative status for an external data source or a consumer reference.
--
-- Topo may surface these states and messages in diagnostics, but the owning
-- provider, adapter, or external system remains responsible for the concrete
-- migration, schema, connection, and consistency details behind them.
data RPCExternalDataSourceStatusState
  = ExternalStatusUnknown
  | ExternalStatusUnconfigured
  | ExternalStatusReady
  | ExternalStatusDegraded
  | ExternalStatusUnavailable
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCExternalDataSourceStatusState where
  parseJSON = withText "RPCExternalDataSourceStatusState" $ \t -> case t of
    "unknown"      -> pure ExternalStatusUnknown
    "unconfigured" -> pure ExternalStatusUnconfigured
    "ready"        -> pure ExternalStatusReady
    "degraded"     -> pure ExternalStatusDegraded
    "unavailable"  -> pure ExternalStatusUnavailable
    _              -> fail ("unknown external data source status: " <> Text.unpack t)

instance ToJSON RPCExternalDataSourceStatusState where
  toJSON ExternalStatusUnknown      = "unknown"
  toJSON ExternalStatusUnconfigured = "unconfigured"
  toJSON ExternalStatusReady        = "ready"
  toJSON ExternalStatusDegraded     = "degraded"
  toJSON ExternalStatusUnavailable  = "unavailable"

-- | Backend-neutral availability metadata for an external data source status.
data RPCExternalDataSourceAvailability
  = ExternalAvailabilityUnknown
  | ExternalAvailabilityAvailable
  | ExternalAvailabilityDegraded
  | ExternalAvailabilityUnavailable
  | ExternalAvailabilityUnconfigured
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCExternalDataSourceAvailability where
  parseJSON = withText "RPCExternalDataSourceAvailability" $ \t -> case t of
    "unknown"      -> pure ExternalAvailabilityUnknown
    "available"    -> pure ExternalAvailabilityAvailable
    "degraded"     -> pure ExternalAvailabilityDegraded
    "unavailable"  -> pure ExternalAvailabilityUnavailable
    "unconfigured" -> pure ExternalAvailabilityUnconfigured
    _              -> fail ("unknown external data source availability: " <> Text.unpack t)

instance ToJSON RPCExternalDataSourceAvailability where
  toJSON ExternalAvailabilityUnknown      = "unknown"
  toJSON ExternalAvailabilityAvailable    = "available"
  toJSON ExternalAvailabilityDegraded     = "degraded"
  toJSON ExternalAvailabilityUnavailable  = "unavailable"
  toJSON ExternalAvailabilityUnconfigured = "unconfigured"

-- | Backend-neutral health metadata reported by the provider or adapter.
data RPCExternalDataSourceHealth
  = ExternalHealthUnknown
  | ExternalHealthHealthy
  | ExternalHealthDegraded
  | ExternalHealthUnhealthy
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCExternalDataSourceHealth where
  parseJSON = withText "RPCExternalDataSourceHealth" $ \t -> case t of
    "unknown"   -> pure ExternalHealthUnknown
    "healthy"   -> pure ExternalHealthHealthy
    "degraded"  -> pure ExternalHealthDegraded
    "unhealthy" -> pure ExternalHealthUnhealthy
    _           -> fail ("unknown external data source health: " <> Text.unpack t)

instance ToJSON RPCExternalDataSourceHealth where
  toJSON ExternalHealthUnknown   = "unknown"
  toJSON ExternalHealthHealthy   = "healthy"
  toJSON ExternalHealthDegraded  = "degraded"
  toJSON ExternalHealthUnhealthy = "unhealthy"

-- | Backend-neutral access policy summary for the current status snapshot.
data RPCExternalDataSourceAccessMode
  = ExternalAccessModeReadOnly
  | ExternalAccessModeReadWrite
  | ExternalAccessModeAdmin
  | ExternalAccessModeDisabled
  | ExternalAccessModeProviderManaged
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCExternalDataSourceAccessMode where
  parseJSON = withText "RPCExternalDataSourceAccessMode" $ \t -> case t of
    "read_only"        -> pure ExternalAccessModeReadOnly
    "read_write"       -> pure ExternalAccessModeReadWrite
    "admin"            -> pure ExternalAccessModeAdmin
    "disabled"         -> pure ExternalAccessModeDisabled
    "provider_managed" -> pure ExternalAccessModeProviderManaged
    _                  -> fail ("unknown external data source access mode: " <> Text.unpack t)

instance ToJSON RPCExternalDataSourceAccessMode where
  toJSON ExternalAccessModeReadOnly        = "read_only"
  toJSON ExternalAccessModeReadWrite       = "read_write"
  toJSON ExternalAccessModeAdmin           = "admin"
  toJSON ExternalAccessModeDisabled        = "disabled"
  toJSON ExternalAccessModeProviderManaged = "provider_managed"

data RPCExternalDataSourceStatus = RPCExternalDataSourceStatus
  { redssState           :: !RPCExternalDataSourceStatusState
  , redssMessage         :: !(Maybe Text)
  , redssProviderId      :: !(Maybe Text)
  , redssAvailability    :: !(Maybe RPCExternalDataSourceAvailability)
  , redssHealth          :: !(Maybe RPCExternalDataSourceHealth)
  , redssAccessMode      :: !(Maybe RPCExternalDataSourceAccessMode)
  , redssCapabilityScope :: ![RPCExternalDataSourceCapability]
  , redssVersion         :: !(Maybe Text)
  , redssCompatibility   :: !(Maybe Text)
  , redssDiagnostics     :: !(Maybe Value)
  , redssObservedAt      :: !(Maybe UTCTime)
  , redssFresh           :: !Bool
  } deriving (Eq, Show, Generic)

defaultRPCExternalDataSourceStatus :: RPCExternalDataSourceStatus
defaultRPCExternalDataSourceStatus = RPCExternalDataSourceStatus
  { redssState = ExternalStatusUnknown
  , redssMessage = Nothing
  , redssProviderId = Nothing
  , redssAvailability = Nothing
  , redssHealth = Nothing
  , redssAccessMode = Nothing
  , redssCapabilityScope = []
  , redssVersion = Nothing
  , redssCompatibility = Nothing
  , redssDiagnostics = Nothing
  , redssObservedAt = Nothing
  , redssFresh = True
  }

instance FromJSON RPCExternalDataSourceStatus where
  parseJSON = withObject "RPCExternalDataSourceStatus" $ \o ->
    RPCExternalDataSourceStatus
      <$> o .: "state"
      <*> o .:? "message"
      <*> optionalNonNullField o ["providerId", "provider_id"]
      <*> optionalNonNullField o ["availability"]
      <*> optionalNonNullField o ["health"]
      <*> optionalNonNullField o ["accessMode", "access_mode"]
      <*> optionalNonNullListField o ["capabilityScope", "capability_scope"]
      <*> optionalNonNullField o ["version"]
      <*> optionalNonNullField o ["compatibility"]
      <*> optionalNonNullField o ["diagnostics"]
      <*> optionalNonNullField o ["observedAt", "observed_at"]
      <*> optionalPolicyField o ["fresh"] True

instance ToJSON RPCExternalDataSourceStatus where
  toJSON status = object $
    [ "state" .= redssState status ] <>
    [ "message" .= msg | Just msg <- [redssMessage status] ] <>
    [ "providerId" .= providerId | Just providerId <- [redssProviderId status] ] <>
    [ "availability" .= availability | Just availability <- [redssAvailability status] ] <>
    [ "health" .= health | Just health <- [redssHealth status] ] <>
    [ "accessMode" .= accessMode | Just accessMode <- [redssAccessMode status] ] <>
    [ "capabilityScope" .= redssCapabilityScope status | not (null (redssCapabilityScope status)) ] <>
    [ "version" .= version | Just version <- [redssVersion status] ] <>
    [ "compatibility" .= compatibility | Just compatibility <- [redssCompatibility status] ] <>
    [ "diagnostics" .= diagnostics | Just diagnostics <- [redssDiagnostics status] ] <>
    [ "observedAt" .= observedAt | Just observedAt <- [redssObservedAt status] ] <>
    [ "fresh" .= redssFresh status | not (redssFresh status) ]

-- | A status is current only after the host has observed it in the latest
-- successful provider report. Manifest-declared ready statuses remain
-- diagnostics until a report stamps them with 'observeExternalDataSourceStatus'.
externalDataSourceStatusCurrent :: RPCExternalDataSourceStatus -> Bool
externalDataSourceStatusCurrent status =
  redssFresh status && maybe False (const True) (redssObservedAt status)

-- | Stamp a provider-reported status with host observation metadata.
observeExternalDataSourceStatus :: UTCTime -> RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus
observeExternalDataSourceStatus observedAt status = status
  { redssObservedAt = Just observedAt
  , redssFresh = True
  }

-- | Preserve the last status payload as diagnostic history while marking it
-- stale for brokerability and diagnostics.
staleExternalDataSourceStatus :: RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus
staleExternalDataSourceStatus status = status { redssFresh = False }

-- | Backend-neutral grant offered for a provider-owned external data source.
--
-- A grant names the access modes and source capabilities the provider is
-- prepared to broker.  'redsgReference' is opaque provider metadata: topo may
-- preserve and report it, but must not interpret it as a storage backend it
-- owns.  Grant resources and capabilities do not define backend schemas,
-- migration tables, or consistency rules.  'redsgConfigRefs' names opaque
-- configuration references that identify the grant across user/provider,
-- environment, or deployment configuration without exposing a backend layout.
data RPCExternalDataSourceGrant = RPCExternalDataSourceGrant
  { redsgName         :: !Text
  , redsgAccess       :: ![RPCExternalDataSourceAccess]
  , redsgCapabilities :: ![RPCExternalDataSourceCapability]
  , redsgResources    :: ![Text]
  , redsgStatus       :: !RPCExternalDataSourceStatus
  , redsgReference    :: !(Maybe Value)
  , redsgConfigRefs   :: ![RPCExternalDataSourceConfigRef]
  } deriving (Eq, Show, Generic)

instance FromJSON RPCExternalDataSourceGrant where
  parseJSON = withObject "RPCExternalDataSourceGrant" $ \o ->
    RPCExternalDataSourceGrant
      <$> o .: "name"
      <*> o .: "access"
      <*> o .: "capabilities"
      <*> (o .:? "resources" >>= pure . maybe [] id)
      <*> o .: "status"
      <*> o .:? "reference"
      <*> optionalNonNullListField o ["configRefs", "config_refs"]

instance ToJSON RPCExternalDataSourceGrant where
  toJSON grant = object $
    [ "name" .= redsgName grant
    , "access" .= redsgAccess grant
    , "capabilities" .= redsgCapabilities grant
    , "status" .= redsgStatus grant
    ] <>
    [ "resources" .= redsgResources grant | not (null (redsgResources grant)) ] <>
    [ "reference" .= ref | Just ref <- [redsgReference grant] ] <>
    [ "configRefs" .= redsgConfigRefs grant | not (null (redsgConfigRefs grant)) ]

-- | Provider declaration for a named external data source.
--
-- The provider plugin, adapter, or external system owns the backing migrations,
-- schemas, connection details, and consistency semantics.  Topo stores and
-- validates only backend-neutral names, capabilities, grants, resource labels,
-- statuses, opaque config references, and opaque metadata; it may surface
-- status/errors but must not prescribe backend-specific migration tables or
-- schema rules.
data RPCExternalDataSourceDecl = RPCExternalDataSourceDecl
  { redsdName         :: !Text
  , redsdLabel        :: !Text
  , redsdDescription  :: !Text
  , redsdKind         :: !Text
  , redsdCapabilities :: ![RPCExternalDataSourceCapability]
  , redsdResources    :: ![Text]
  , redsdStatus       :: !RPCExternalDataSourceStatus
  , redsdConnection   :: !(Maybe Value)
  , redsdConfigRefs   :: ![RPCExternalDataSourceConfigRef]
  , redsdGrants       :: ![RPCExternalDataSourceGrant]
  , redsdUiHints      :: !RPCUIHints
  } deriving (Eq, Show, Generic)

instance FromJSON RPCExternalDataSourceDecl where
  parseJSON = withObject "RPCExternalDataSourceDecl" $ \o -> do
    name <- o .: "name"
    label <- o .:? "label"
    desc <- o .:? "description"
    RPCExternalDataSourceDecl
      <$> pure name
      <*> pure (maybe name id label)
      <*> pure (maybe "" id desc)
      <*> o .: "kind"
      <*> o .: "capabilities"
      <*> (o .:? "resources" >>= pure . maybe [] id)
      <*> o .: "status"
      <*> o .:? "connection"
      <*> optionalNonNullListField o ["configRefs", "config_refs"]
      <*> (o .:? "grants" >>= pure . maybe [] id)
      <*> optionalPolicyField o ["ui"] defaultRPCUIHints

instance ToJSON RPCExternalDataSourceDecl where
  toJSON source = object $
    [ "name" .= redsdName source
    , "label" .= redsdLabel source
    , "kind" .= redsdKind source
    , "capabilities" .= redsdCapabilities source
    , "status" .= redsdStatus source
    ] <>
    [ "description" .= redsdDescription source | redsdDescription source /= "" ] <>
    [ "resources" .= redsdResources source | not (null (redsdResources source)) ] <>
    [ "connection" .= conn | Just conn <- [redsdConnection source] ] <>
    [ "configRefs" .= redsdConfigRefs source | not (null (redsdConfigRefs source)) ] <>
    [ "grants" .= redsdGrants source | not (null (redsdGrants source)) ] <>
    [ "ui" .= redsdUiHints source | redsdUiHints source /= defaultRPCUIHints ]

-- | Consumer reference to a provider-owned external data source.
--
-- References bind a plugin to provider-owned sources, grants, and external
-- config references without exposing migration, schema, connection, or
-- consistency internals to topo core.
data RPCExternalDataSourceRef = RPCExternalDataSourceRef
  { redsrName       :: !Text
  , redsrProvider   :: !(Maybe Text)
  , redsrSource     :: !Text
  , redsrRequired   :: !Bool
  , redsrAccess     :: ![RPCExternalDataSourceAccess]
  , redsrResources  :: ![Text]
  , redsrGrant      :: !(Maybe Text)
  , redsrStatus     :: !RPCExternalDataSourceStatus
  , redsrReference  :: !(Maybe Value)
  , redsrConfigRefs :: ![RPCExternalDataSourceConfigRef]
  , redsrUiHints    :: !RPCUIHints
  } deriving (Eq, Show, Generic)

instance FromJSON RPCExternalDataSourceRef where
  parseJSON = withObject "RPCExternalDataSourceRef" $ \o ->
    RPCExternalDataSourceRef
      <$> o .: "name"
      <*> o .:? "provider"
      <*> o .: "source"
      <*> o .: "required"
      <*> o .: "access"
      <*> (o .:? "resources" >>= pure . maybe [] id)
      <*> o .:? "grant"
      <*> o .: "status"
      <*> o .:? "reference"
      <*> optionalNonNullListField o ["configRefs", "config_refs"]
      <*> optionalPolicyField o ["ui"] defaultRPCUIHints

instance ToJSON RPCExternalDataSourceRef where
  toJSON ref = object $
    [ "name" .= redsrName ref
    , "source" .= redsrSource ref
    , "required" .= redsrRequired ref
    , "access" .= redsrAccess ref
    , "status" .= redsrStatus ref
    ] <>
    [ "provider" .= p | Just p <- [redsrProvider ref] ] <>
    [ "resources" .= redsrResources ref | not (null (redsrResources ref)) ] <>
    [ "grant" .= grant | Just grant <- [redsrGrant ref] ] <>
    [ "reference" .= refMeta | Just refMeta <- [redsrReference ref] ] <>
    [ "configRefs" .= redsrConfigRefs ref | not (null (redsrConfigRefs ref)) ] <>
    [ "ui" .= redsrUiHints ref | redsrUiHints ref /= defaultRPCUIHints ]

------------------------------------------------------------------------
-- Full manifest
------------------------------------------------------------------------

-- | A plugin's complete manifest, parsed from @manifest.json@.
data RPCManifest = RPCManifest
  { rmManifestVersion :: !Int
    -- ^ Manifest contract version. Version 3 is the current contract.
  , rmName          :: !Text
    -- ^ Unique plugin identifier.
  , rmVersion       :: !Text
    -- ^ Plugin version string (informational).
  , rmRuntime       :: !RPCManifestRuntime
    -- ^ Runtime and protocol compatibility bounds.
  , rmDescription   :: !Text
    -- ^ Human-readable description.
  , rmUiHints       :: !RPCUIHints
    -- ^ Optional UI presentation hints.
  , rmGenerator     :: !(Maybe RPCGeneratorDecl)
    -- ^ Generator pipeline declaration (if the plugin seeds data).
  , rmSimulation    :: !(Maybe RPCSimulationDecl)
    -- ^ Simulation DAG declaration (if the plugin ticks an overlay).
  , rmOverlay       :: !(Maybe RPCOverlayDecl)
    -- ^ Overlay schema reference (if the plugin owns an overlay).
  , rmCapabilities  :: ![RPCCapability]
    -- ^ Requested capabilities.
  , rmParameters    :: ![RPCParamSpec]
    -- ^ User-facing configuration parameters.
  , rmDataResources :: ![DataResourceSchema]
    -- ^ Plugin-declared data resource schemas.
  , rmDataDirectory :: !(Maybe Text)
    -- ^ Safe relative plugin-data archive directory under the world save. The
    --   host bundles from its launch-created data root, not from this text as a
    --   source path.
  , rmExternalDataSources :: ![RPCExternalDataSourceDecl]
    -- ^ Provider-owned external data sources advertised by this plugin.
  , rmExternalDataSourceRefs :: ![RPCExternalDataSourceRef]
    -- ^ External data sources consumed by this plugin.
  , rmStartPolicy   :: !RPCStartPolicy
    -- ^ Host-side process supervision policy.
  } deriving (Eq, Show, Generic)

instance FromJSON RPCManifest where
  parseJSON = withObject "RPCManifest" $ \o -> do
    manifestVersion <- o .: "manifestVersion"
    name   <- o .:  "name"
    ver    <- o .:  "version"
    runtime <- o .: "runtime"
    desc   <- o .:? "description"
    ui     <- optionalPolicyField o ["ui"] defaultRPCUIHints
    gen    <- o .:? "generator"
    sim    <- o .:? "simulation"
    ov     <- o .:? "overlay"
    caps   <- o .:? "capabilities"
    config <- o .:? "config"
    params <- case config of
      Just co -> withObject "config" (\c -> c .:? "parameters") co
      Nothing -> pure Nothing
    dataRes  <- o .:? "dataResources"
    dataDir  <- o .:? "dataDirectory"
    externalSources <- o .:? "externalDataSources"
    externalRefs <- optionalPolicyField o ["externalDataSourceRefs", "externalDataSourceReferences"] ([] :: [RPCExternalDataSourceRef])
    startPolicy <- optionalPolicyField o ["startPolicy", "start_policy"] defaultRPCStartPolicy
    pure RPCManifest
      { rmManifestVersion = manifestVersion
      , rmName          = name
      , rmVersion       = ver
      , rmRuntime       = runtime
      , rmDescription   = maybe "" id desc
      , rmUiHints       = ui
      , rmGenerator     = gen
      , rmSimulation    = sim
      , rmOverlay       = ov
      , rmCapabilities  = maybe [] id caps
      , rmParameters    = maybe [] id params
      , rmDataResources = maybe [] id dataRes
      , rmDataDirectory = dataDir
      , rmExternalDataSources = maybe [] id externalSources
      , rmExternalDataSourceRefs = externalRefs
      , rmStartPolicy   = startPolicy
      }

instance ToJSON RPCManifest where
  toJSON rm = object $
    [ "manifestVersion" .= rmManifestVersion rm
    , "name"    .= rmName rm
    , "version" .= rmVersion rm
    , "runtime" .= rmRuntime rm
    ] <>
    [ "description" .= rmDescription rm | rmDescription rm /= "" ] <>
    [ "ui" .= rmUiHints rm | rmUiHints rm /= defaultRPCUIHints ] <>
    [ "generator"   .= g  | Just g  <- [rmGenerator rm] ] <>
    [ "simulation"  .= s  | Just s  <- [rmSimulation rm] ] <>
    [ "overlay"     .= ov | Just ov <- [rmOverlay rm] ] <>
    [ "capabilities" .= rmCapabilities rm | not (null (rmCapabilities rm)) ] <>
    [ "config" .= object ["parameters" .= rmParameters rm]
    | not (null (rmParameters rm))
    ] <>
    [ "dataResources" .= rmDataResources rm
    | not (null (rmDataResources rm))
    ] <>
    [ "dataDirectory" .= dd
    | Just dd <- [rmDataDirectory rm]
    ] <>
    [ "externalDataSources" .= rmExternalDataSources rm
    | not (null (rmExternalDataSources rm))
    ] <>
    [ "externalDataSourceRefs" .= rmExternalDataSourceRefs rm
    | not (null (rmExternalDataSourceRefs rm))
    ] <>
    [ "startPolicy" .= sp
    | let sp = rmStartPolicy rm
    , sp /= defaultRPCStartPolicy
    ]

------------------------------------------------------------------------
-- Manifest v3 JSON Schema and golden examples
------------------------------------------------------------------------

-- | JSON Schema for the manifest v3 contract.
manifestV3Schema :: Value
manifestV3Schema = object
  [ "$schema" .= ("https://json-schema.org/draft/2020-12/schema" :: Text)
  , "$id" .= ("https://topo.dev/schemas/plugin-manifest-v3.schema.json" :: Text)
  , "title" .= ("Topo plugin manifest v3" :: Text)
  , "type" .= ("object" :: Text)
  , "required" .= (["manifestVersion", "name", "version", "runtime"] :: [Text])
  , "properties" .= object
      [ "manifestVersion" .= object ["const" .= manifestV3]
      , "name" .= stringSchema
      , "version" .= stringSchema
      , "runtime" .= schemaRef "runtime"
      , "description" .= stringSchema
      , "ui" .= schemaRef "uiHints"
      , "generator" .= schemaRef "generator"
      , "simulation" .= schemaRef "simulation"
      , "overlay" .= schemaRef "overlay"
      , "capabilities" .= arrayOf (enumSchema capabilityNames)
      , "config" .= schemaRef "config"
      , "dataResources" .= arrayOf (schemaRef "dataResource")
      , "dataDirectory" .= stringSchema
      , "externalDataSources" .= arrayOf (schemaRef "externalDataSource")
      , "externalDataSourceRefs" .= arrayOf (schemaRef "externalDataSourceRef")
      , "startPolicy" .= schemaRef "startPolicy"
      ]
  , "$defs" .= object
      [ "runtime" .= object
          [ "type" .= ("object" :: Text)
          , "required" .= (["protocol"] :: [Text])
          , "properties" .= object
              [ "protocol" .= object
                  [ "type" .= ("object" :: Text)
                  , "required" .= (["min", "max"] :: [Text])
                  , "properties" .= object
                      [ "min" .= integerSchema
                      , "max" .= integerSchema
                      ]
                  ]
              , "topo" .= object
                  [ "type" .= ("object" :: Text)
                  , "properties" .= object
                      [ "min" .= stringSchema
                      , "max" .= stringSchema
                      ]
                  ]
              ]
          ]
      , "uiHints" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "displayName" .= stringSchema
              , "category" .= stringSchema
              , "tags" .= arrayOf stringSchema
              , "icon" .= stringSchema
              , "docsUrl" .= stringSchema
              , "order" .= integerSchema
              ]
          ]
      , "generator" .= object
          [ "type" .= ("object" :: Text)
          , "required" .= (["insertAfter"] :: [Text])
          , "properties" .= object
              [ "insertAfter" .= stringSchema
              , "requires" .= arrayOf stringSchema
              ]
          ]
      , "simulation" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "dependencies" .= arrayOf stringSchema
              , "interval_ticks" .= integerMinimumSchema 1
              , "phase_ticks" .= integerMinimumSchema 0
              , "catch_up" .= enumSchema ["run_once_if_due", "skip_missed"]
              ]
          ]
      , "overlay" .= object
          [ "type" .= ("object" :: Text)
          , "required" .= (["schemaFile"] :: [Text])
          , "properties" .= object
              [ "schemaFile" .= stringSchema
              ]
          ]
      , "config" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "parameters" .= arrayOf (schemaRef "parameter")
              ]
          ]
      , "parameter" .= object
          [ "type" .= ("object" :: Text)
          , "required" .= (["name", "label", "type", "default"] :: [Text])
          , "properties" .= object
              [ "name" .= stringSchema
              , "label" .= stringSchema
              , "type" .= enumSchema ["float", "int", "bool"]
              , "default" .= object []
              , "range" .= object
                  [ "type" .= ("array" :: Text)
                  , "minItems" .= (2 :: Int)
                  , "maxItems" .= (2 :: Int)
                  , "items" .= object []
                  ]
              , "tooltip" .= stringSchema
              ]
          ]
      , "dataResource" .= object
          [ "type" .= ("object" :: Text)
          , "required" .= (["name", "label", "fields", "operations", "keyField"] :: [Text])
          , "properties" .= object
              [ "schemaVersion" .= integerSchema
              , "resourceVersion" .= integerSchema
              , "name" .= stringSchema
              , "label" .= stringSchema
              , "hexBound" .= booleanSchema
              , "fields" .= arrayOf (schemaRef "dataField")
              , "operations" .= schemaRef "dataOperations"
              , "keyField" .= stringSchema
              , "overlay" .= stringSchema
              , "pagination" .= schemaRef "dataPagination"
              ]
          ]
      , "dataField" .= object
          [ "type" .= ("object" :: Text)
          , "required" .= (["name", "type", "label"] :: [Text])
          , "properties" .= object
              [ "name" .= stringSchema
              , "type" .= schemaRef "dataFieldType"
              , "label" .= stringSchema
              , "editable" .= booleanSchema
              , "default" .= object []
              ]
          ]
      , "dataConstructor" .= object
          [ "type" .= ("object" :: Text)
          , "required" .= (["constructor"] :: [Text])
          , "properties" .= object
              [ "constructor" .= stringSchema
              , "fields" .= arrayOf (schemaRef "dataFieldType")
              ]
          ]
      , "dataOperations" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "list" .= booleanSchema
              , "get" .= booleanSchema
              , "create" .= booleanSchema
              , "update" .= booleanSchema
              , "delete" .= booleanSchema
              , "queryByHex" .= booleanSchema
              , "queryByField" .= booleanSchema
              , "sort" .= booleanSchema
              , "filter" .= booleanSchema
              , "page" .= booleanSchema
              ]
          ]
      , "dataPagination" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "defaultPageSize" .= integerSchema
              , "maxPageSize" .= integerSchema
              , "defaultPageOffset" .= integerSchema
              ]
          ]
      , "startPolicy" .= object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "auto_start" .= booleanSchema
              , "restart_mode" .= enumSchema ["never", "on_failure", "always"]
              , "max_restarts" .= integerSchema
              , "restart_window_ms" .= integerSchema
              , "startup_timeout_ms" .= integerSchema
              , "request_timeout_ms" .= integerSchema
              , "shutdown_timeout_ms" .= integerSchema
              , "backoff_ms" .= integerSchema
              ]
          ]
      , "externalDataSource" .= object
          [ "type" .= ("object" :: Text)
          , "description" .= ("Provider-owned external source declaration. Migrations, schemas, connection details, and consistency rules are owned by the provider plugin, adapter, or external system; topo may surface status/errors but must not prescribe backend-specific migration tables or schema rules." :: Text)
          , "required" .= (["name", "kind", "capabilities", "status"] :: [Text])
          , "properties" .= object
              [ "name" .= stringSchema
              , "label" .= stringSchema
              , "description" .= stringSchema
              , "kind" .= stringSchema
              , "capabilities" .= arrayOf (enumSchema externalCapabilityNames)
              , "resources" .= arrayOf stringSchema
              , "status" .= schemaRef "externalStatus"
              , "connection" .= schemaRef "opaqueMetadata"
              , "configRefs" .= arrayOf (schemaRef "externalConfigRef")
              , "grants" .= arrayOf (schemaRef "externalGrant")
              , "ui" .= schemaRef "uiHints"
              ]
          ]
      , "externalDataSourceRef" .= object
          [ "type" .= ("object" :: Text)
          , "description" .= ("Consumer binding to a provider-owned external source. It names desired access and resources without making topo own backend schemas, migrations, connection details, or consistency rules." :: Text)
          , "required" .= (["name", "source", "required", "access", "status"] :: [Text])
          , "properties" .= object
              [ "name" .= stringSchema
              , "provider" .= stringSchema
              , "source" .= stringSchema
              , "required" .= booleanSchema
              , "access" .= arrayOf (enumSchema externalAccessNames)
              , "resources" .= arrayOf stringSchema
              , "grant" .= stringSchema
              , "status" .= schemaRef "externalStatus"
              , "reference" .= schemaRef "opaqueMetadata"
              , "configRefs" .= arrayOf (schemaRef "externalConfigRef")
              , "ui" .= schemaRef "uiHints"
              ]
          ]
      , "externalGrant" .= object
          [ "type" .= ("object" :: Text)
          , "description" .= ("Provider-defined backend-neutral grant. Capabilities and resources scope brokered access but do not prescribe backend migration tables, schemas, or consistency rules." :: Text)
          , "required" .= (["name", "access", "capabilities", "status"] :: [Text])
          , "properties" .= object
              [ "name" .= stringSchema
              , "access" .= arrayOf (enumSchema externalAccessNames)
              , "capabilities" .= arrayOf (enumSchema externalCapabilityNames)
              , "resources" .= arrayOf stringSchema
              , "status" .= schemaRef "externalStatus"
              , "reference" .= schemaRef "opaqueMetadata"
              , "configRefs" .= arrayOf (schemaRef "externalConfigRef")
              ]
          ]
      , "externalConfigRef" .= object
          [ "type" .= ("object" :: Text)
          , "description" .= ("Opaque backend-neutral configuration reference for an external source, grant, or consumer binding. The key is supplied by user config, a provider plugin, the environment, or deployment settings; topo preserves it with compatibility metadata but must not interpret it as a backend-specific file layout, schema, migration table, or connection rule." :: Text)
          , "required" .= (["name", "origin", "key"] :: [Text])
          , "properties" .= object
              [ "name" .= stringSchema
              , "origin" .= enumSchema externalConfigOriginNames
              , "key" .= stringSchema
              , "required" .= booleanSchema
              , "compatibility" .= stringSchema
              , "metadata" .= schemaRef "opaqueMetadata"
              ]
          ]
      , "externalStatus" .= object
          [ "type" .= ("object" :: Text)
          , "description" .= ("Provider-declared, backend-neutral status that topo may surface in diagnostics while leaving backend repair, migration, schema, connection, and consistency details to the provider or external system. Optional metadata can identify the provider, availability, health, access mode, capability scope, version/compatibility marker, host observation freshness, and opaque diagnostics without requiring backend-specific settings." :: Text)
          , "required" .= (["state"] :: [Text])
          , "properties" .= object
              [ "state" .= enumSchema externalStatusNames
              , "message" .= stringSchema
              , "providerId" .= stringSchema
              , "availability" .= enumSchema externalAvailabilityNames
              , "health" .= enumSchema externalHealthNames
              , "accessMode" .= enumSchema externalAccessModeNames
              , "capabilityScope" .= arrayOf (enumSchema externalCapabilityNames)
              , "version" .= stringSchema
              , "compatibility" .= stringSchema
              , "diagnostics" .= schemaRef "opaqueMetadata"
              , "observedAt" .= dateTimeSchema
              , "fresh" .= booleanSchema
              ]
          ]
      , "opaqueMetadata" .= object
          [ "type" .= ("object" :: Text)
          , "description" .= ("Opaque provider-owned metadata for handles, bindings, or connection details; topo stores, brokers, and reports it without interpreting a backend, schema, migration table, or consistency rule." :: Text)
          , "additionalProperties" .= True
          ]
      , "dataFieldType" .= dataFieldTypeSchema
      ]
  ]

manifestV3ProviderExample :: Value
manifestV3ProviderExample = object
  [ "manifestVersion" .= manifestV3
  , "name" .= ("civilization" :: Text)
  , "version" .= ("1.0.0" :: Text)
  , "runtime" .= object
      [ "protocol" .= object ["min" .= currentProtocolVersion, "max" .= currentProtocolVersion]
      , "topo" .= object ["min" .= ("1.0" :: Text)]
      ]
  , "description" .= ("Civilization simulation overlay and settlement catalogue" :: Text)
  , "ui" .= object
      [ "displayName" .= ("Civilization" :: Text)
      , "category" .= ("Simulation" :: Text)
      , "tags" .= (["settlements", "culture"] :: [Text])
      ]
  , "generator" .= object
      [ "insertAfter" .= ("biomes" :: Text)
      , "requires" .= (["biomes", "rivers"] :: [Text])
      ]
  , "simulation" .= object
      [ "dependencies" .= (["weather"] :: [Text])
      , "interval_ticks" .= (1 :: Int)
      , "phase_ticks" .= (0 :: Int)
      , "catch_up" .= ("run_once_if_due" :: Text)
      ]
  , "overlay" .= object
      [ "schemaFile" .= ("civilization.toposchema" :: Text)
      ]
  , "capabilities" .= (["readTerrain", "readOverlay", "writeOverlay", "dataRead", "log"] :: [Text])
  , "config" .= object
      [ "parameters" .=
          [ object
              [ "name" .= ("growth_rate" :: Text)
              , "label" .= ("Growth Rate" :: Text)
              , "type" .= ("float" :: Text)
              , "default" .= (0.02 :: Double)
              , "range" .= ([0.0, 0.5] :: [Double])
              , "tooltip" .= ("Population growth fraction per tick" :: Text)
              ]
          ]
      ]
  , "dataResources" .=
      [ object
          [ "schemaVersion" .= (1 :: Int)
          , "resourceVersion" .= (1 :: Int)
          , "name" .= ("settlements" :: Text)
          , "label" .= ("Settlements" :: Text)
          , "hexBound" .= True
          , "fields" .=
              [ object
                  [ "name" .= ("id" :: Text)
                  , "type" .= ("text" :: Text)
                  , "label" .= ("Settlement ID" :: Text)
                  ]
              , object
                  [ "name" .= ("population" :: Text)
                  , "type" .= ("int" :: Text)
                  , "label" .= ("Population" :: Text)
                  ]
              ]
          , "operations" .= object
              [ "list" .= True
              , "get" .= True
              , "create" .= False
              , "update" .= False
              , "delete" .= False
              , "queryByHex" .= True
              , "queryByField" .= False
              , "sort" .= False
              , "filter" .= False
              , "page" .= False
              ]
          , "keyField" .= ("id" :: Text)
          , "pagination" .= object
              [ "defaultPageSize" .= (20 :: Int)
              , "maxPageSize" .= (500 :: Int)
              , "defaultPageOffset" .= (0 :: Int)
              ]
          ]
      ]
  , "externalDataSources" .=
      [ object
          [ "name" .= ("settlement-ledger" :: Text)
          , "label" .= ("Settlement Ledger" :: Text)
          , "description" .= ("Provider-owned settlement records shared with dependent plugins" :: Text)
          , "kind" .= ("catalog" :: Text)
          , "capabilities" .= (["query", "health"] :: [Text])
          , "resources" .= (["settlements"] :: [Text])
          , "status" .= object
              [ "state" .= ("ready" :: Text)
              , "message" .= ("Records are available through the provider plugin" :: Text)
              , "providerId" .= ("civilization" :: Text)
              , "availability" .= ("available" :: Text)
              , "health" .= ("healthy" :: Text)
              , "accessMode" .= ("read_only" :: Text)
              , "capabilityScope" .= (["query", "health"] :: [Text])
              , "version" .= ("settlement-ledger.v1" :: Text)
              , "compatibility" .= ("manifest-v3" :: Text)
              , "diagnostics" .= object
                  [ "reportedBy" .= ("civilization" :: Text)
                  ]
              ]
          , "connection" .= object
              [ "handle" .= ("provider-owned:settlement-ledger" :: Text)
              ]
          , "configRefs" .=
              [ object
                  [ "name" .= ("settlement-ledger-binding" :: Text)
                  , "origin" .= ("provider" :: Text)
                  , "key" .= ("civilization.settlement-ledger" :: Text)
                  , "required" .= True
                  , "compatibility" .= ("manifest-v3" :: Text)
                  , "metadata" .= object
                      [ "handle" .= ("provider-owned:settlement-ledger" :: Text)
                      ]
                  ]
              ]
          , "grants" .=
              [ object
                  [ "name" .= ("settlement-read" :: Text)
                  , "access" .= (["read"] :: [Text])
                  , "capabilities" .= (["query", "health"] :: [Text])
                  , "resources" .= (["settlements"] :: [Text])
                  , "status" .= object
                      [ "state" .= ("ready" :: Text)
                      , "message" .= ("Read grant can be brokered to dependent plugins" :: Text)
                      , "providerId" .= ("civilization" :: Text)
                      , "availability" .= ("available" :: Text)
                      , "health" .= ("healthy" :: Text)
                      , "accessMode" .= ("read_only" :: Text)
                      , "capabilityScope" .= (["query", "health"] :: [Text])
                      , "version" .= ("settlement-read.v1" :: Text)
                      , "compatibility" .= ("manifest-v3" :: Text)
                      , "diagnostics" .= object
                          [ "grant" .= ("settlement-read" :: Text)
                          ]
                      ]
                  , "reference" .= object
                      [ "handle" .= ("grant:settlement-read" :: Text)
                      ]
                  , "configRefs" .=
                      [ object
                          [ "name" .= ("settlement-read-binding" :: Text)
                          , "origin" .= ("provider" :: Text)
                          , "key" .= ("civilization.settlement-read" :: Text)
                          , "required" .= True
                          , "compatibility" .= ("manifest-v3" :: Text)
                          , "metadata" .= object
                              [ "grant" .= ("settlement-read" :: Text)
                              ]
                          ]
                      ]
                  ]
              ]
          , "ui" .= object
              [ "displayName" .= ("Settlement Ledger" :: Text)
              , "category" .= ("External data" :: Text)
              ]
          ]
      ]
  ]

manifestV3ConsumerExample :: Value
manifestV3ConsumerExample = object
  [ "manifestVersion" .= manifestV3
  , "name" .= ("trade-routes" :: Text)
  , "version" .= ("0.3.0" :: Text)
  , "runtime" .= object
      [ "protocol" .= object ["min" .= currentProtocolVersion, "max" .= currentProtocolVersion]
      ]
  , "description" .= ("Trade route simulation that consumes settlement data" :: Text)
  , "ui" .= object
      [ "displayName" .= ("Trade Routes" :: Text)
      , "category" .= ("Simulation" :: Text)
      , "tags" .= (["trade", "settlements"] :: [Text])
      ]
  , "generator" .= object
      [ "insertAfter" .= ("civilization" :: Text)
      , "requires" .= (["civilization"] :: [Text])
      ]
  , "capabilities" .= (["readTerrain", "dataRead", "log"] :: [Text])
  , "externalDataSourceRefs" .=
      [ object
          [ "name" .= ("settlements" :: Text)
          , "provider" .= ("civilization" :: Text)
          , "source" .= ("settlement-ledger" :: Text)
          , "required" .= True
          , "access" .= (["read"] :: [Text])
          , "resources" .= (["settlements"] :: [Text])
          , "grant" .= ("settlement-read" :: Text)
          , "status" .= object
              [ "state" .= ("unknown" :: Text)
              , "message" .= ("Resolved during plugin dependency startup" :: Text)
              , "providerId" .= ("civilization" :: Text)
              , "availability" .= ("unknown" :: Text)
              , "health" .= ("unknown" :: Text)
              , "accessMode" .= ("read_only" :: Text)
              , "capabilityScope" .= (["query"] :: [Text])
              , "version" .= ("settlement-ledger.v1" :: Text)
              , "compatibility" .= ("manifest-v3" :: Text)
              , "diagnostics" .= object
                  [ "resolution" .= ("dependency-startup" :: Text)
                  ]
              ]
          , "reference" .= object
              [ "binding" .= ("trade-routes:settlements" :: Text)
              ]
          , "configRefs" .=
              [ object
                  [ "name" .= ("settlements-binding" :: Text)
                  , "origin" .= ("deployment" :: Text)
                  , "key" .= ("trade-routes.settlements" :: Text)
                  , "required" .= True
                  , "compatibility" .= ("manifest-v3" :: Text)
                  , "metadata" .= object
                      [ "binding" .= ("trade-routes:settlements" :: Text)
                      ]
                  ]
              ]
          ]
      ]
  ]

stringSchema :: Value
stringSchema = object ["type" .= ("string" :: Text)]

dateTimeSchema :: Value
dateTimeSchema = object
  [ "type" .= ("string" :: Text)
  , "format" .= ("date-time" :: Text)
  ]

integerSchema :: Value
integerSchema = object ["type" .= ("integer" :: Text)]

integerMinimumSchema :: Int -> Value
integerMinimumSchema minimumValue = object
  [ "type" .= ("integer" :: Text)
  , "minimum" .= minimumValue
  ]

booleanSchema :: Value
booleanSchema = object ["type" .= ("boolean" :: Text)]

arrayOf :: Value -> Value
arrayOf itemSchema = object
  [ "type" .= ("array" :: Text)
  , "items" .= itemSchema
  ]

schemaRef :: Text -> Value
schemaRef name = object ["$ref" .= ("#/$defs/" <> name)]

enumSchema :: [Text] -> Value
enumSchema names = object
  [ "type" .= ("string" :: Text)
  , "enum" .= names
  ]

dataFieldTypeSchema :: Value
dataFieldTypeSchema = object
  [ "oneOf" .=
      [ enumSchema ["text", "int", "float", "double", "bool", "fixed2", "fixed3", "fixed4"]
      , object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "enum" .= arrayOf stringSchema
              , "record" .= arrayOf (schemaRef "dataField")
              , "adt" .= arrayOf (schemaRef "dataConstructor")
              ]
          ]
      ]
  ]

capabilityNames :: [Text]
capabilityNames =
  [ "log"
  , "noise"
  , "readTerrain"
  , "writeTerrain"
  , "readOverlay"
  , "writeOverlay"
  , "readWorld"
  , "writeWorld"
  , "dataRead"
  , "dataWrite"
  ]

externalCapabilityNames :: [Text]
externalCapabilityNames = ["query", "mutate", "subscribe", "migrate", "health"]

externalAccessNames :: [Text]
externalAccessNames = ["read", "write", "admin"]

externalConfigOriginNames :: [Text]
externalConfigOriginNames = ["user", "provider", "environment", "deployment"]

externalStatusNames :: [Text]
externalStatusNames = ["unknown", "unconfigured", "ready", "degraded", "unavailable"]

externalAvailabilityNames :: [Text]
externalAvailabilityNames = ["unknown", "available", "degraded", "unavailable", "unconfigured"]

externalHealthNames :: [Text]
externalHealthNames = ["unknown", "healthy", "degraded", "unhealthy"]

externalAccessModeNames :: [Text]
externalAccessModeNames = ["read_only", "read_write", "admin", "disabled", "provider_managed"]

------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------

-- | Parse a manifest from a raw JSON 'BS.ByteString'.
parseManifest :: BS.ByteString -> Either Text RPCManifest
parseManifest bs =
  case Aeson.eitherDecodeStrict' bs of
    Left err   -> Left (Text.pack err)
    Right mani -> Right mani

-- | Parse a manifest from a file on disk.
parseManifestFile :: FilePath -> IO (Either Text RPCManifest)
parseManifestFile path = do
  bs <- BS.readFile path
  pure (parseManifest bs)

------------------------------------------------------------------------
-- Validation
------------------------------------------------------------------------

-- | Errors found during manifest validation.
data ManifestError
  = ManifestUnsupportedVersion !Int
    -- ^ Manifest version is not supported by this host contract.
  | ManifestProtocolRangeInvalid !Int !Int
    -- ^ Runtime protocol minimum is greater than the maximum.
  | ManifestProtocolUnsupported !Int !Int !Int
    -- ^ Current host protocol is outside the manifest's supported range.
  | ManifestEmptyName
    -- ^ Plugin name is empty.
  | ManifestEmptyVersion
    -- ^ Plugin version is empty.
  | ManifestSimWithoutOverlay
    -- ^ Simulation declared without an overlay to own.
  | ManifestWriteTerrainWithoutSim
    -- ^ @writeTerrain@ capability without a simulation declaration.
  | ManifestNoParticipation
    -- ^ Plugin declares neither generator, simulation, nor data resources.
  | ManifestDataReadWithoutCapability
    -- ^ Data resources declared without @dataRead@ capability.
  | ManifestDataWriteWithoutCapability
    -- ^ Data resources with write operations declared without
    --   @dataWrite@ capability.
  | ManifestInvalidField !Text !Text
    -- ^ A manifest field is present but structurally invalid.
  | ManifestDuplicateFieldValue !Text !Text
    -- ^ A list-like manifest field contains a duplicate value.
  | ManifestInvalidDataResource !Text !Text
    -- ^ A data resource schema failed validation.
  deriving (Eq, Ord, Show, Read)

-- | Render a manifest validation error as an actionable host/UI diagnostic.
manifestErrorMessage :: ManifestError -> Text
manifestErrorMessage (ManifestUnsupportedVersion version) =
  "manifestVersion must be 3; found " <> showText version <> ". Update the plugin manifest to v3 or use a compatible Topo host."
manifestErrorMessage (ManifestProtocolRangeInvalid pmin pmax) =
  "runtime.protocol.min must be less than or equal to runtime.protocol.max; found min=" <> showText pmin <> ", max=" <> showText pmax <> "."
manifestErrorMessage (ManifestProtocolUnsupported current pmin pmax) =
  "runtime.protocol range " <> showText pmin <> ".." <> showText pmax
    <> " does not include this host protocol " <> showText current <> ". Rebuild the plugin SDK or adjust the manifest runtime bounds."
manifestErrorMessage ManifestEmptyName =
  "name must be a non-empty plugin identifier."
manifestErrorMessage ManifestEmptyVersion =
  "version must be a non-empty plugin version string."
manifestErrorMessage ManifestSimWithoutOverlay =
  "simulation requires an overlay.schemaFile declaration so the host can load the plugin-owned overlay schema."
manifestErrorMessage ManifestWriteTerrainWithoutSim =
  "writeTerrain/writeWorld capabilities require a simulation declaration; remove the write capability or add simulation and overlay sections."
manifestErrorMessage ManifestNoParticipation =
  "manifest must declare generator, simulation, dataResources, externalDataSources, or externalDataSourceRefs so the host knows how the plugin participates."
manifestErrorMessage ManifestDataReadWithoutCapability =
  "dataResources require the dataRead capability so the host can expose resource browsing safely."
manifestErrorMessage ManifestDataWriteWithoutCapability =
  "dataResources with create/update/delete operations require the dataWrite capability."
manifestErrorMessage (ManifestInvalidField field detail) =
  field <> " is invalid: " <> detail
manifestErrorMessage (ManifestDuplicateFieldValue field value) =
  field <> " contains duplicate value " <> quote value <> "; remove the duplicate or rename one declaration."
manifestErrorMessage (ManifestInvalidDataResource resource detail) =
  "dataResources." <> resource <> " is invalid: " <> detail

-- | Render multiple manifest errors for display in a single lifecycle message.
renderManifestErrors :: [ManifestError] -> Text
renderManifestErrors = Text.intercalate "; " . map manifestErrorMessage

-- | Validate structural invariants of a parsed manifest.
--
-- Returns a list of errors (empty means valid).
validateManifest :: RPCManifest -> [ManifestError]
validateManifest rm = concat
  [ [ ManifestUnsupportedVersion (rmManifestVersion rm)
    | rmManifestVersion rm /= manifestV3
    ]
  , validateRuntime (rmRuntime rm)
  , [ ManifestEmptyName | Text.null (rmName rm) ]
  , validatePluginName (rmName rm)
  , [ ManifestEmptyVersion | Text.null (rmVersion rm) ]
  , validateUIHints "ui" (rmUiHints rm)
  , validateGenerator (rmGenerator rm)
  , validateSimulation (rmSimulation rm)
  , validateOverlay (rmOverlay rm)
  , [ ManifestSimWithoutOverlay
    | Just _ <- [rmSimulation rm]
    , Nothing <- [rmOverlay rm]
    ]
  , [ ManifestWriteTerrainWithoutSim
    | any (`elem` rmCapabilities rm) [CapWriteTerrain, CapWriteWorld]
    , Nothing <- [rmSimulation rm]
    ]
  , [ ManifestNoParticipation
    | Nothing <- [rmGenerator rm]
    , Nothing <- [rmSimulation rm]
    , null (rmDataResources rm)
    , null (rmExternalDataSources rm)
    , null (rmExternalDataSourceRefs rm)
    ]
  , duplicateErrors "capabilities" (map capabilityText (rmCapabilities rm))
  , validateParameters (rmParameters rm)
  , [ ManifestDataReadWithoutCapability
    | not (null (rmDataResources rm))
    , CapDataRead `notElem` rmCapabilities rm
    ]
  , [ ManifestDataWriteWithoutCapability
    | any hasWriteOps (rmDataResources rm)
    , CapDataWrite `notElem` rmCapabilities rm
    ]
  , validateDataResources (rmDataResources rm)
  , validateDataDirectory (rmDataDirectory rm)
  , validateExternalDataSources (rmExternalDataSources rm)
  , validateExternalDataSourceRefs (rmExternalDataSourceRefs rm)
  ]
  where
    hasWriteOps = dataResourceHasWriteOps

-- | Validate data-resource schemas advertised by a runtime handshake.
--
-- Handshake resources may narrow the manifest declaration, but must never
-- widen the host-routed data surface.  This keeps the manifest as the static
-- authorization boundary while still allowing a runtime to omit unsupported
-- resources or operations.
validateHandshakeDataResources :: RPCManifest -> [DataResourceSchema] -> [ManifestError]
validateHandshakeDataResources manifest resources
  | null resources = []
  | otherwise = concat
      [ validateHandshakeResourceStructures resources
      , [ ManifestInvalidField "handshake.resources" "handshake data resources require the manifest dataRead capability."
        | CapDataRead `notElem` rmCapabilities manifest
        ]
      , [ ManifestInvalidField (handshakeResourcePath resource "operations") "write-capable handshake data resources require the manifest dataWrite capability."
        | resource <- resources
        , dataResourceHasWriteOps resource
        , CapDataWrite `notElem` rmCapabilities manifest
        ]
      , concatMap validateAgainstManifest resources
      ]
  where
    manifestResources = Map.fromList
      [ (drsName resource, resource)
      | resource <- rmDataResources manifest
      ]

    validateAgainstManifest resource
      | Text.null (drsName resource) = []
      | otherwise = case Map.lookup (drsName resource) manifestResources of
          Nothing ->
            [ ManifestInvalidField (handshakeResourcePath resource "name")
                "resource must be declared by manifest dataResources before runtime negotiation."
            ]
          Just declared -> validateNonWideningResource declared resource

validateHandshakeResourceStructures :: [DataResourceSchema] -> [ManifestError]
validateHandshakeResourceStructures resources =
  duplicateErrors "handshake.resources.name" (map drsName resources) <>
  [ ManifestInvalidField (handshakeResourcePath resource "schema") (dataResourceErrorMessage err)
  | resource <- resources
  , err <- validateDataResource resource
  ]

validateNonWideningResource :: DataResourceSchema -> DataResourceSchema -> [ManifestError]
validateNonWideningResource declared offered = concat
  [ [ ManifestInvalidField (basePath <> ".schemaVersion")
        "schemaVersion must match the manifest declaration."
    | drsSchemaVersion offered /= drsSchemaVersion declared
    ]
  , [ ManifestInvalidField (basePath <> ".resourceVersion")
        "resourceVersion must match the manifest declaration."
    | drsResourceVersion offered /= drsResourceVersion declared
    ]
  , [ ManifestInvalidField (basePath <> ".keyField")
        ("keyField cannot change from " <> quote (drsKeyField declared) <> " to " <> quote (drsKeyField offered) <> ".")
    | drsKeyField offered /= drsKeyField declared
    ]
  , [ ManifestInvalidField (basePath <> ".overlay")
        ("overlay cannot change from " <> maybeText (drsOverlay declared) <> " to " <> maybeText (drsOverlay offered) <> ".")
    | drsOverlay offered /= drsOverlay declared
    ]
  , [ ManifestInvalidField (basePath <> ".hexBound")
        "hexBound cannot change from the manifest declaration."
    | drsHexBound offered /= drsHexBound declared
    ]
  , [ ManifestInvalidField (basePath <> ".fields")
        ("cannot add fields not declared in the manifest: " <> Text.intercalate ", " (map quote newFieldNames) <> ".")
    | not (null newFieldNames)
    ]
  , [ ManifestInvalidField (basePath <> ".fields." <> dfName offeredField <> ".type")
        "field type must match the manifest declaration."
    | offeredField <- drsFields offered
    , Just declaredField <- [Map.lookup (dfName offeredField) declaredFieldMap]
    , dfType offeredField /= dfType declaredField
    ]
  , [ ManifestInvalidField (basePath <> ".fields." <> dfName offeredField <> ".editable")
        "editable=true cannot be negotiated unless the manifest declares the field editable."
    | offeredField <- drsFields offered
    , Just declaredField <- [Map.lookup (dfName offeredField) declaredFieldMap]
    , dfEditable offeredField
    , not (dfEditable declaredField)
    ]
  , [ ManifestInvalidField (basePath <> ".operations")
        ("cannot enable operations not declared in the manifest: " <> Text.intercalate ", " (map quote widenedOperations) <> ".")
    | not (null widenedOperations)
    ]
  , [ ManifestInvalidField (basePath <> ".pagination.maxPageSize")
        ("maxPageSize cannot increase above the manifest limit " <> showText (dpMaxPageSize (drsPagination declared)) <> ".")
    | dpMaxPageSize (drsPagination offered) > dpMaxPageSize (drsPagination declared)
    ]
  ]
  where
    basePath = "handshake.resources." <> nameOrPlaceholder (drsName offered)
    declaredFieldMap = Map.fromList
      [ (dfName field, field)
      | field <- drsFields declared
      ]
    newFieldNames =
      [ dfName field
      | field <- drsFields offered
      , Map.notMember (dfName field) declaredFieldMap
      ]
    widenedOperations = dataOperationWidenings (drsOperations declared) (drsOperations offered)

handshakeResourcePath :: DataResourceSchema -> Text -> Text
handshakeResourcePath resource field =
  "handshake.resources." <> nameOrPlaceholder (drsName resource) <> "." <> field

dataOperationWidenings :: DataOperations -> DataOperations -> [Text]
dataOperationWidenings declared offered =
  [ name
  | (name, getter) <- dataOperationFlags
  , getter offered
  , not (getter declared)
  ]

dataOperationFlags :: [(Text, DataOperations -> Bool)]
dataOperationFlags =
  [ ("list", doList)
  , ("get", doGet)
  , ("create", doCreate)
  , ("update", doUpdate)
  , ("delete", doDelete)
  , ("queryByHex", doQueryByHex)
  , ("queryByField", doQueryByField)
  , ("sort", doSort)
  , ("filter", doFilter)
  , ("page", doPage)
  ]

dataResourceHasWriteOps :: DataResourceSchema -> Bool
dataResourceHasWriteOps drs =
  let ops = drsOperations drs
  in doCreate ops || doUpdate ops || doDelete ops

maybeText :: Maybe Text -> Text
maybeText Nothing = "<none>"
maybeText (Just value) = quote value

validatePluginName :: Text -> [ManifestError]
validatePluginName name =
  [ ManifestInvalidField "name" "name must be a plugin identifier, not a path; do not include '/', '\\', ':', or the special names '.' and '..'."
  | not (Text.null name)
  , not (safePluginIdentifier name)
  ]

validateRuntime :: RPCManifestRuntime -> [ManifestError]
validateRuntime runtime = concat
  [ [ ManifestProtocolRangeInvalid pmin pmax | pmin > pmax ]
  , [ ManifestProtocolUnsupported currentProtocolVersion pmin pmax
    | currentProtocolVersion < pmin || currentProtocolVersion > pmax
    ]
  , [ ManifestInvalidField "runtime.topo.min" "topo minimum version must be non-empty when present."
    | Just value <- [rmrTopoMin runtime]
    , Text.null value
    ]
  , [ ManifestInvalidField "runtime.topo.max" "topo maximum version must be non-empty when present."
    | Just value <- [rmrTopoMax runtime]
    , Text.null value
    ]
  ]
  where
    pmin = rmrProtocolMin runtime
    pmax = rmrProtocolMax runtime

validateGenerator :: Maybe RPCGeneratorDecl -> [ManifestError]
validateGenerator Nothing = []
validateGenerator (Just gen) = concat
  [ [ ManifestInvalidField "generator.insertAfter" "insertAfter must be a non-empty built-in stage, plugin, or overlay name."
    | Text.null (rgdInsertAfter gen)
    ]
  , [ ManifestInvalidField "generator.requires" "requires entries must be non-empty."
    | any Text.null (rgdRequires gen)
    ]
  , duplicateErrors "generator.requires" (rgdRequires gen)
  ]

validateSimulation :: Maybe RPCSimulationDecl -> [ManifestError]
validateSimulation Nothing = []
validateSimulation (Just sim) = concat
  [ [ ManifestInvalidField "simulation.dependencies" "dependency entries must be non-empty."
    | any Text.null (rsdDependencies sim)
    ]
  , duplicateErrors "simulation.dependencies" (rsdDependencies sim)
  , validateSimulationSchedule (rsdSchedule sim)
  ]

validateSimulationSchedule :: SimulationScheduleDecl -> [ManifestError]
validateSimulationSchedule schedule =
  [ ManifestInvalidField "simulation.schedule" err
  | Just err <- [scheduleDeclError schedule]
  ]

validateOverlay :: Maybe RPCOverlayDecl -> [ManifestError]
validateOverlay Nothing = []
validateOverlay (Just overlayDecl) = concat
  [ [ ManifestInvalidField "overlay.schemaFile" "schemaFile must be a non-empty path relative to the plugin directory."
    | Text.null schemaFile
    ]
  , [ ManifestInvalidField "overlay.schemaFile" "schemaFile must stay inside the plugin directory; do not use absolute paths, drive prefixes, '.', or '..' segments."
    | not (Text.null schemaFile)
    , not (safeRelativePath schemaFile)
    ]
  ]
  where
    schemaFile = rodSchemaFile overlayDecl

validateUIHints :: Text -> RPCUIHints -> [ManifestError]
validateUIHints base ui = concat
  [ validateOptionalNonEmpty (base <> ".displayName") (ruiDisplayName ui)
  , validateOptionalNonEmpty (base <> ".category") (ruiCategory ui)
  , validateOptionalNonEmpty (base <> ".icon") (ruiIcon ui)
  , validateOptionalNonEmpty (base <> ".docsUrl") (ruiDocsUrl ui)
  , [ ManifestInvalidField (base <> ".tags") "tags must not contain empty strings."
    | any Text.null (ruiTags ui)
    ]
  , duplicateErrors (base <> ".tags") (ruiTags ui)
  , [ ManifestInvalidField (base <> ".order") "order must be zero or greater."
    | Just order <- [ruiOrder ui]
    , order < 0
    ]
  ]

validateOptionalNonEmpty :: Text -> Maybe Text -> [ManifestError]
validateOptionalNonEmpty field value =
  [ ManifestInvalidField field "field must be non-empty when present."
  | Just text <- [value]
  , Text.null text
  ]

validateParameters :: [RPCParamSpec] -> [ManifestError]
validateParameters params = concat
  [ [ ManifestInvalidField (paramPath param "name") "parameter name must be non-empty."
    | param <- params
    , Text.null (rpsName param)
    ]
  , [ ManifestInvalidField (paramPath param "label") "parameter label must be non-empty."
    | param <- params
    , Text.null (rpsLabel param)
    ]
  , concatMap validateParameterSpec params
  , duplicateErrors "config.parameters.name" (map rpsName params)
  ]
  where
    paramPath param field = "config.parameters." <> nameOrPlaceholder (rpsName param) <> "." <> field

validateParameterSpec :: RPCParamSpec -> [ManifestError]
validateParameterSpec param = concat
  [ validateParameterDefault param
  , validateParameterRange param
  , validateParameterDefaultWithinRange param
  ]

validateParameterDefault :: RPCParamSpec -> [ManifestError]
validateParameterDefault param = case rpsType param of
  ParamBool ->
    [ invalidDefault param "default must be a boolean value."
    | not (isBoolValue (rpsDefault param))
    ]
  ParamFloat ->
    [ invalidDefault param "default must be a numeric value."
    | not (isNumberValue (rpsDefault param))
    ]
  ParamInt ->
    [ invalidDefault param "default must be an integral numeric value."
    | not (isIntegralNumberValue (rpsDefault param))
    ]

validateParameterRange :: RPCParamSpec -> [ManifestError]
validateParameterRange param = case (rpsType param, rpsRange param) of
  (ParamBool, Just _) ->
    [ ManifestInvalidField (paramFieldPath param "range") "bool parameters must not declare numeric ranges." ]
  (ParamBool, Nothing) -> []
  (ParamFloat, Just (lo, hi)) -> validateFloatParameterRange param lo hi
  (ParamFloat, Nothing) -> []
  (ParamInt, Just (lo, hi)) -> validateIntParameterRange param lo hi
  (ParamInt, Nothing) -> []

validateFloatParameterRange :: RPCParamSpec -> Value -> Value -> [ManifestError]
validateFloatParameterRange param lo hi = case (numericValue lo, numericValue hi) of
  (Just loNum, Just hiNum)
    | loNum < hiNum -> []
    | otherwise -> [invalidRange param "range bounds must be strictly increasing."]
  _ -> [invalidRange param "range bounds must be numeric values."]

validateIntParameterRange :: RPCParamSpec -> Value -> Value -> [ManifestError]
validateIntParameterRange param lo hi = case (lo, hi) of
  (Number _, Number _) -> case (integerValue lo, integerValue hi) of
    (Just loInt, Just hiInt)
      | loInt < hiInt -> []
      | otherwise -> [invalidRange param "range bounds must be strictly increasing."]
    _ -> [invalidRange param "integer range bounds must be integral numeric values."]
  _ -> [invalidRange param "range bounds must be numeric values."]

validateParameterDefaultWithinRange :: RPCParamSpec -> [ManifestError]
validateParameterDefaultWithinRange param = case (rpsType param, rpsRange param) of
  (ParamFloat, Just (lo, hi)) -> case (numericValue (rpsDefault param), numericValue lo, numericValue hi) of
    (Just def, Just loNum, Just hiNum)
      | loNum < hiNum && (def < loNum || def > hiNum) -> [invalidDefault param "default must be within the inclusive range."]
      | otherwise -> []
    _ -> []
  (ParamInt, Just (lo, hi)) -> case (integerValue (rpsDefault param), integerValue lo, integerValue hi) of
    (Just def, Just loInt, Just hiInt)
      | loInt < hiInt && (def < loInt || def > hiInt) -> [invalidDefault param "default must be within the inclusive range."]
      | otherwise -> []
    _ -> []
  _ -> []

invalidDefault :: RPCParamSpec -> Text -> ManifestError
invalidDefault param = ManifestInvalidField (paramFieldPath param "default")

invalidRange :: RPCParamSpec -> Text -> ManifestError
invalidRange param = ManifestInvalidField (paramFieldPath param "range")

paramFieldPath :: RPCParamSpec -> Text -> Text
paramFieldPath param field = "config.parameters." <> nameOrPlaceholder (rpsName param) <> "." <> field

isBoolValue :: Value -> Bool
isBoolValue (Bool _) = True
isBoolValue _ = False

isNumberValue :: Value -> Bool
isNumberValue (Number _) = True
isNumberValue _ = False

isIntegralNumberValue :: Value -> Bool
isIntegralNumberValue = maybe False (const True) . integerValue

validateDataResources :: [DataResourceSchema] -> [ManifestError]
validateDataResources resources =
  duplicateErrors "dataResources.name" (map drsName resources) <>
  [ ManifestInvalidDataResource (nameOrPlaceholder (drsName resource)) (dataResourceErrorMessage err)
  | resource <- resources
  , err <- validateDataResource resource
  ]

validateDataDirectory :: Maybe Text -> [ManifestError]
validateDataDirectory dataDirectory = concat
  [ [ ManifestInvalidField "dataDirectory" "dataDirectory must be non-empty when present."
    | Just dir <- [dataDirectory]
    , Text.null dir
    ]
  , [ ManifestInvalidField "dataDirectory" "dataDirectory must stay inside the plugin/world data boundary; do not use absolute paths, drive prefixes, '.', or '..' segments."
    | Just dir <- [dataDirectory]
    , not (Text.null dir)
    , not (safeRelativePath dir)
    ]
  ]

validateExternalDataSources :: [RPCExternalDataSourceDecl] -> [ManifestError]
validateExternalDataSources sources =
  duplicateErrors "externalDataSources.name" (map redsdName sources) <>
  concatMap validateSource sources
  where
    validateSource source = concat
      [ [ ManifestInvalidField (sourcePath source "name") "external data-source name must be non-empty."
        | Text.null (redsdName source)
        ]
      , [ ManifestInvalidField (sourcePath source "label") "external data-source label must be non-empty."
        | Text.null (redsdLabel source)
        ]
      , [ ManifestInvalidField (sourcePath source "kind") "kind must be a backend-neutral, non-empty source category."
        | Text.null (redsdKind source)
        ]
      , [ ManifestInvalidField (sourcePath source "capabilities") "capabilities must include at least one of query, mutate, subscribe, migrate, or health."
        | null (redsdCapabilities source)
        ]
      , duplicateErrors (sourcePath source "capabilities") (map externalCapabilityText (redsdCapabilities source))
      , [ ManifestInvalidField (sourcePath source "resources") "resource names must be non-empty."
        | any Text.null (redsdResources source)
        ]
      , duplicateErrors (sourcePath source "resources") (redsdResources source)
      , validateStatus (sourcePath source "status") (redsdStatus source)
      , [ ManifestInvalidField (sourcePath source "status.capabilityScope") "status capabilityScope entries must be declared by the external data source."
        | capability <- redssCapabilityScope (redsdStatus source)
        , capability `notElem` redsdCapabilities source
        ]
      , validateOpaqueMetadata (sourcePath source "connection") (redsdConnection source)
      , validateConfigRefs (sourcePath source "configRefs") (redsdConfigRefs source)
      , duplicateErrors (sourcePath source "grants.name") (map redsgName (redsdGrants source))
      , concatMap (validateGrant source) (redsdGrants source)
      , validateUIHints (sourcePath source "ui") (redsdUiHints source)
      ]
    validateGrant source grant = concat
      [ [ ManifestInvalidField (grantPath source grant "name") "grant name must be non-empty."
        | Text.null (redsgName grant)
        ]
      , [ ManifestInvalidField (grantPath source grant "access") "grant access must include at least one of read, write, or admin."
        | null (redsgAccess grant)
        ]
      , duplicateErrors (grantPath source grant "access") (map externalAccessText (redsgAccess grant))
      , [ ManifestInvalidField (grantPath source grant "capabilities") "grant capabilities must include at least one of query, mutate, subscribe, migrate, or health."
        | null (redsgCapabilities grant)
        ]
      , duplicateErrors (grantPath source grant "capabilities") (map externalCapabilityText (redsgCapabilities grant))
      , [ ManifestInvalidField (grantPath source grant "capabilities") "grant capabilities must be declared by the external data source."
        | capability <- redsgCapabilities grant
        , capability `notElem` redsdCapabilities source
        ]
      , [ ManifestInvalidField (grantPath source grant "capabilities")
            ("grant access " <> quote (externalAccessText access)
              <> " requires capability " <> capabilityListText requiredCapabilities <> ".")
        | access <- redsgAccess grant
        , let requiredCapabilities = externalAccessRequiredCapabilities access
        , not (all (`elem` redsgCapabilities grant) requiredCapabilities)
        ]
      , [ ManifestInvalidField (grantPath source grant "resources") "grant resource names must be non-empty."
        | any Text.null (redsgResources grant)
        ]
      , duplicateErrors (grantPath source grant "resources") (redsgResources grant)
      , [ ManifestInvalidField (grantPath source grant "resources") "grant resources require matching source resources."
        | null (redsdResources source)
        , not (null (redsgResources grant))
        ]
      , [ ManifestInvalidField (grantPath source grant "resources") "grant resources must be declared by the external data source."
        | not (null (redsdResources source))
        , resource <- redsgResources grant
        , resource `notElem` redsdResources source
        ]
      , validateStatus (grantPath source grant "status") (redsgStatus grant)
      , [ ManifestInvalidField (grantPath source grant "status.capabilityScope") "status capabilityScope entries must be declared by the external data-source grant."
        | capability <- redssCapabilityScope (redsgStatus grant)
        , capability `notElem` redsgCapabilities grant
        ]
      , validateOpaqueMetadata (grantPath source grant "reference") (redsgReference grant)
      , validateConfigRefs (grantPath source grant "configRefs") (redsgConfigRefs grant)
      ]
    sourcePath source field = "externalDataSources." <> nameOrPlaceholder (redsdName source) <> "." <> field
    grantPath source grant field = sourcePath source ("grants." <> nameOrPlaceholder (redsgName grant) <> "." <> field)

validateExternalDataSourceRefs :: [RPCExternalDataSourceRef] -> [ManifestError]
validateExternalDataSourceRefs refs =
  duplicateErrors "externalDataSourceRefs.name" (map redsrName refs) <>
  concatMap validateRef refs
  where
    validateRef ref = concat
      [ [ ManifestInvalidField (refPath ref "name") "external data-source reference name must be non-empty."
        | Text.null (redsrName ref)
        ]
      , validateOptionalNonEmpty (refPath ref "provider") (redsrProvider ref)
      , [ ManifestInvalidField (refPath ref "source") "source must name the provider-owned external data source."
        | Text.null (redsrSource ref)
        ]
      , [ ManifestInvalidField (refPath ref "access") "access must include at least one of read, write, or admin."
        | null (redsrAccess ref)
        ]
      , duplicateErrors (refPath ref "access") (map externalAccessText (redsrAccess ref))
      , [ ManifestInvalidField (refPath ref "resources") "resource names must be non-empty."
        | any Text.null (redsrResources ref)
        ]
      , duplicateErrors (refPath ref "resources") (redsrResources ref)
      , validateOptionalNonEmpty (refPath ref "grant") (redsrGrant ref)
      , validateStatus (refPath ref "status") (redsrStatus ref)
      , validateOpaqueMetadata (refPath ref "reference") (redsrReference ref)
      , validateConfigRefs (refPath ref "configRefs") (redsrConfigRefs ref)
      , validateUIHints (refPath ref "ui") (redsrUiHints ref)
      ]
    refPath ref field = "externalDataSourceRefs." <> nameOrPlaceholder (redsrName ref) <> "." <> field

validateConfigRefs :: Text -> [RPCExternalDataSourceConfigRef] -> [ManifestError]
validateConfigRefs base refs =
  duplicateErrors (base <> ".name") (map redscrName refs) <>
  concatMap validateConfigRef refs
  where
    validateConfigRef configRef = concat
      [ [ ManifestInvalidField (configRefPath configRef "name") "config reference name must be non-empty."
        | Text.null (redscrName configRef)
        ]
      , [ ManifestInvalidField (configRefPath configRef "key") "config reference key must be a non-empty opaque identifier."
        | Text.null (redscrKey configRef)
        ]
      , validateOptionalNonEmpty (configRefPath configRef "compatibility") (redscrCompatibility configRef)
      , validateOpaqueMetadata (configRefPath configRef "metadata") (redscrMetadata configRef)
      ]
    configRefPath configRef field = base <> "." <> nameOrPlaceholder (redscrName configRef) <> "." <> field

validateOpaqueMetadata :: Text -> Maybe Value -> [ManifestError]
validateOpaqueMetadata base metadata =
  [ ManifestInvalidField base "opaque metadata must be a JSON object when present."
  | Just value <- [metadata]
  , not (isObject value)
  ]
  where
    isObject (Object _) = True
    isObject _ = False

validateStatus :: Text -> RPCExternalDataSourceStatus -> [ManifestError]
validateStatus base status = concat
  [ [ ManifestInvalidField (base <> ".message") "message must be non-empty when present."
    | Just message <- [redssMessage status]
    , Text.null message
    ]
  , validateOptionalNonEmpty (base <> ".providerId") (redssProviderId status)
  , duplicateErrors (base <> ".capabilityScope") (map externalCapabilityText (redssCapabilityScope status))
  , validateOptionalNonEmpty (base <> ".version") (redssVersion status)
  , validateOptionalNonEmpty (base <> ".compatibility") (redssCompatibility status)
  , validateOpaqueMetadata (base <> ".diagnostics") (redssDiagnostics status)
  ]

dataResourceErrorMessage :: DataResourceError -> Text
dataResourceErrorMessage DREEmptyName = "resource name must be non-empty."
dataResourceErrorMessage DREEmptyLabel = "resource label must be non-empty."
dataResourceErrorMessage DRENoFields = "resource must declare at least one field."
dataResourceErrorMessage (DREKeyFieldMissing keyField) = "keyField " <> quote keyField <> " must name one of the resource fields."
dataResourceErrorMessage DREQueryByHexNotHexBound = "queryByHex requires hexBound=true."
dataResourceErrorMessage (DREDuplicateField fieldName) = "duplicate field name " <> quote fieldName <> "."
dataResourceErrorMessage DREEmptyFieldName = "field names must be non-empty."
dataResourceErrorMessage DREOverlayNotHexBound = "overlay-backed resources must set hexBound=true."
dataResourceErrorMessage (DREEmptyEnum fieldName) = "enum field " <> quote fieldName <> " must declare at least one choice."
dataResourceErrorMessage (DRENullaryAdt fieldName) = "ADT field " <> quote fieldName <> " must declare at least one constructor."
dataResourceErrorMessage (DREEmptyConstructorName fieldName) = "ADT field " <> quote fieldName <> " has an empty constructor name."
dataResourceErrorMessage (DREDuplicateConstructor fieldName constructorName) =
  "ADT field " <> quote fieldName <> " has duplicate constructor " <> quote constructorName <> "."

duplicateErrors :: Text -> [Text] -> [ManifestError]
duplicateErrors field values =
  [ ManifestDuplicateFieldValue field value
  | value <- duplicates values
  , not (Text.null value)
  ]

duplicates :: Eq a => [a] -> [a]
duplicates [] = []
duplicates (x:xs)
  | x `elem` xs = x : duplicates (filter (/= x) xs)
  | otherwise = duplicates xs

nameOrPlaceholder :: Text -> Text
nameOrPlaceholder value
  | Text.null value = "<empty>"
  | otherwise = value

quote :: Text -> Text
quote value = "'" <> value <> "'"

capabilityListText :: [RPCExternalDataSourceCapability] -> Text
capabilityListText = Text.intercalate ", " . map (quote . externalCapabilityText)

safePluginIdentifier :: Text -> Bool
safePluginIdentifier value =
  value /= "."
    && value /= ".."
    && not (Text.any (`elem` pathDelimiterChars) value)

safeRelativePath :: Text -> Bool
safeRelativePath value =
  not (Text.null value)
    && not (Text.isPrefixOf "/" value)
    && not (Text.isPrefixOf "\\" value)
    && not (Text.any (== ':') value)
    && all safePathSegment (pathSegments value)

pathSegments :: Text -> [Text]
pathSegments = Text.splitOn "/" . Text.replace "\\" "/"

safePathSegment :: Text -> Bool
safePathSegment segment =
  not (Text.null segment)
    && segment /= "."
    && segment /= ".."

pathDelimiterChars :: [Char]
pathDelimiterChars = ['/', '\\', ':']

showText :: Show a => a -> Text
showText = Text.pack . show

capabilityText :: Capability -> Text
capabilityText CapLog = "log"
capabilityText CapNoise = "noise"
capabilityText CapReadTerrain = "readTerrain"
capabilityText CapWriteTerrain = "writeTerrain"
capabilityText CapReadOverlay = "readOverlay"
capabilityText CapWriteOverlay = "writeOverlay"
capabilityText CapReadWorld = "readWorld"
capabilityText CapWriteWorld = "writeWorld"
capabilityText CapDataRead = "dataRead"
capabilityText CapDataWrite = "dataWrite"

externalCapabilityText :: RPCExternalDataSourceCapability -> Text
externalCapabilityText ExternalSourceQuery = "query"
externalCapabilityText ExternalSourceMutate = "mutate"
externalCapabilityText ExternalSourceSubscribe = "subscribe"
externalCapabilityText ExternalSourceMigrate = "migrate"
externalCapabilityText ExternalSourceHealth = "health"

externalAccessText :: RPCExternalDataSourceAccess -> Text
externalAccessText ExternalAccessRead = "read"
externalAccessText ExternalAccessWrite = "write"
externalAccessText ExternalAccessAdmin = "admin"

------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------

-- | Does this plugin declare @writeTerrain@ capability?
-- If so, its sim node should be a 'SimNodeWriter'.
manifestWritesTerrain :: RPCManifest -> Bool
manifestWritesTerrain manifest =
  elem CapWriteTerrain (rmCapabilities manifest)
    || elem CapWriteWorld (rmCapabilities manifest)

-- | Does this plugin participate in the generator pipeline?
manifestHasGenerator :: RPCManifest -> Bool
manifestHasGenerator rm = case rmGenerator rm of
  Just _  -> True
  Nothing -> False

-- | Does this plugin participate in the simulation DAG?
manifestHasSimulation :: RPCManifest -> Bool
manifestHasSimulation rm = case rmSimulation rm of
  Just _  -> True
  Nothing -> False

-- | Does this plugin own an overlay?
manifestHasOverlay :: RPCManifest -> Bool
manifestHasOverlay rm = case rmOverlay rm of
  Just _  -> True
  Nothing -> False
