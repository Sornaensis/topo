{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Small OpenAPI document generator for the HTTP route table.
module Seer.HTTP.OpenAPI
  ( HttpRouteSpec(..)
  , JsonSchema(..)
  , QueryParamSpec(..)
  , RouteBody(..)
  , openApiDocument
  , routePathText
  , schemaRef
  , withRequestSchema
  , withResponseSchema
  , withSchemas
  ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Named JSON schema component referenced by a route request or response.
data JsonSchema = JsonSchema
  { jsonSchemaName :: !Text
  , jsonSchemaValue :: !Value
  } deriving (Eq, Show)

-- | Whether a route accepts a JSON request body.
data RouteBody
  = NoRequestBody
  | OptionalJsonRequestBody
  | RequiredJsonRequestBody
  deriving (Eq, Show)

data QueryParamSpec = QueryParamSpec
  { qpsName :: !Text
  , qpsRequired :: !Bool
  , qpsDescription :: !Text
  , qpsSchema :: !Value
  } deriving (Eq, Show)

-- | Runtime route metadata. The HTTP server and public OpenAPI response use
-- route-spec tables from the same shape so handler coverage and contract paths
-- cannot drift.
data HttpRouteSpec = HttpRouteSpec
  { hrsMethod :: !Text
  , hrsPath :: ![Text]
  , hrsOperationId :: !Text
  , hrsSummary :: !Text
  , hrsTag :: !Text
  , hrsServiceMethod :: !(Maybe Text)
  , hrsRequestBody :: !RouteBody
  , hrsQueryParams :: ![QueryParamSpec]
  , hrsRequestSchema :: !(Maybe JsonSchema)
  , hrsResponseSchema :: !(Maybe JsonSchema)
  } deriving (Eq, Show)

routePathText :: HttpRouteSpec -> Text
routePathText spec = "/" <> Text.intercalate "/" (hrsPath spec)

withRequestSchema :: JsonSchema -> HttpRouteSpec -> HttpRouteSpec
withRequestSchema schema spec = spec { hrsRequestSchema = Just schema }

withResponseSchema :: JsonSchema -> HttpRouteSpec -> HttpRouteSpec
withResponseSchema schema spec = spec { hrsResponseSchema = Just schema }

withSchemas :: JsonSchema -> JsonSchema -> HttpRouteSpec -> HttpRouteSpec
withSchemas requestSchema responseSchema =
  withRequestSchema requestSchema . withResponseSchema responseSchema

openApiDocument :: [HttpRouteSpec] -> Value
openApiDocument specs = object
  [ "openapi" .= ("3.0.3" :: Text)
  , "info" .= object
      [ "title" .= ("Topo Seer HTTP API" :: Text)
      , "version" .= ("1.0.0" :: Text)
      , "description" .= openApiDescription
      , "x-topo-api-version" .= ("1" :: Text)
      , "x-topo-versioning-policy" .= versioningPolicy
      ]
  , "servers" .=
      [ object
          [ "url" .= ("http://127.0.0.1:7373" :: Text)
          , "description" .= ("Default loopback topo-seer HTTP host." :: Text)
          ]
      ]
  , "tags" .= tagObjects specs
  , "paths" .= pathsObject specs
  , "components" .= componentsObject specs
  , "externalDocs" .= object
      [ "description" .= ("Published Topo HTTP/OpenAPI automation guide." :: Text)
      , "url" .= ("./README.md" :: Text)
      ]
  ]

pathsObject :: [HttpRouteSpec] -> Value
pathsObject specs =
  let paths = foldr addRoute Map.empty specs
  in object
      [ Key.fromText path .= object
          [ Key.fromText method .= operation
          | (method, operation) <- Map.toList methods
          ]
      | (path, methods) <- Map.toList paths
      ]
  where
    addRoute spec =
      Map.insertWith Map.union (routePathText spec)
        (Map.singleton (Text.toLower (hrsMethod spec)) (operationObject spec))

operationObject :: HttpRouteSpec -> Value
operationObject spec = object $ baseFields <> securityFields <> queryFields <> bodyFields
  where
    baseFields =
      [ "operationId" .= hrsOperationId spec
      , "summary" .= hrsSummary spec
      , "tags" .= [hrsTag spec]
      , "responses" .= object (responseFields spec)
      ]
    responseFields route =
      [ "200" .= okResponseObject route
      , "400" .= errorResponseObject "Invalid request or unsupported query" "invalid_request"
      , "401" .= errorResponseObject "Unauthorized" "unauthorized"
      , "403" .= errorResponseObject "Forbidden" "permission_denied"
      , "404" .= errorResponseObject "Not found" "not_found"
      , "405" .= errorResponseObject "Operation not supported" "operation_not_supported"
      , "409" .= errorResponseObject "Conflict or rejected state change" "conflict"
      ] <> unsupportedMediaTypeResponse route <>
      [ "422" .= errorResponseObject "Schema validation failed" "schema_validation_failed"
      , "500" .= errorResponseObject "Internal server error" "internal_error"
      , "503" .= errorResponseObject "Unavailable" "unavailable"
      , "504" .= errorResponseObject "Data resource timeout" "timeout"
      ]
    unsupportedMediaTypeResponse route = case hrsRequestBody route of
      NoRequestBody -> []
      _ -> ["415" .= errorResponseObject "Unsupported media type" "unsupported_media_type"]
    securityFields
      | hrsOperationId spec == "meta.health" = []
      | otherwise = ["security" .= [object ["bearerAuth" .= ([] :: [Value])]]]
    queryFields
      | null (hrsQueryParams spec) = []
      | otherwise = ["parameters" .= map queryParameterObject (hrsQueryParams spec)]
    bodyFields = case hrsRequestBody spec of
      NoRequestBody -> []
      OptionalJsonRequestBody -> ["requestBody" .= jsonRequestBody False (hrsRequestSchema spec)]
      RequiredJsonRequestBody -> ["requestBody" .= jsonRequestBody True (hrsRequestSchema spec)]

queryParameterObject :: QueryParamSpec -> Value
queryParameterObject param = object
  [ "name" .= qpsName param
  , "in" .= ("query" :: Text)
  , "required" .= qpsRequired param
  , "description" .= qpsDescription param
  , "schema" .= qpsSchema param
  ]

jsonRequestBody :: Bool -> Maybe JsonSchema -> Value
jsonRequestBody required schema = object
  [ "required" .= required
  , "content" .= jsonContent (maybe genericObjectSchema schemaRef schema)
  ]

okResponseObject :: HttpRouteSpec -> Value
okResponseObject spec
  | hrsOperationId spec == "events.list" = responseObjectWithContent "OK" (eventStreamContent <$> hrsResponseSchema spec)
  | otherwise = responseObject "OK" (hrsResponseSchema spec)

responseObject :: Text -> Maybe JsonSchema -> Value
responseObject description schema =
  responseObjectWithContent description (jsonContent . schemaRef <$> schema)

errorResponseObject :: Text -> Text -> Value
errorResponseObject description code =
  responseObjectWithContent description . Just $
    jsonContentWithExample (schemaRef errorEnvelopeSchema) (Just (errorEnvelopeExampleFor code description))

responseObjectWithContent :: Text -> Maybe Value -> Value
responseObjectWithContent description content = object $
  [ "description" .= description
  , "headers" .= requestIdResponseHeaders
  ]
  <> maybe [] (\c -> ["content" .= c]) content

requestIdResponseHeaders :: Value
requestIdResponseHeaders = object
  [ "X-Request-Id" .= object
      [ "description" .= ("Echoes the optional X-Request-Id request header when supplied." :: Text)
      , "schema" .= object ["type" .= ("string" :: Text)]
      ]
  ]

jsonContent :: Value -> Value
jsonContent schema = jsonContentWithExample schema (schemaExample schema)

jsonContentWithExample :: Value -> Maybe Value -> Value
jsonContentWithExample schema example = object
  [ "application/json" .= object
      ([ "schema" .= schema
       ] <> maybe [] (\value -> ["example" .= value]) example)
  ]

eventStreamContent :: JsonSchema -> Value
eventStreamContent schema = object
  [ "application/json" .= object
      [ "schema" .= schemaRef schema
      ]
  , "text/event-stream" .= object
      [ "schema" .= object
          [ "type" .= ("string" :: Text)
          , "description" .= ("Server-Sent Events stream; each data line contains a ServiceEventEnvelope JSON object." :: Text)
          ]
      ]
  ]

schemaRef :: JsonSchema -> Value
schemaRef schema = object
  [ "$ref" .= ("#/components/schemas/" <> jsonSchemaName schema)
  ]

genericObjectSchema :: Value
genericObjectSchema = object ["type" .= ("object" :: Text)]

componentsObject :: [HttpRouteSpec] -> Value
componentsObject specs = object
  [ "securitySchemes" .= object
      [ "bearerAuth" .= object
          [ "type" .= ("http" :: Text)
          , "scheme" .= ("bearer" :: Text)
          ]
      ]
  , "schemas" .= schemasObject (errorEnvelopeSchema : routeSchemas specs)
  ]

routeSchemas :: [HttpRouteSpec] -> [JsonSchema]
routeSchemas specs = concatMap schemasForRoute specs
  where
    schemasForRoute spec =
      mapMaybe ($ spec) [hrsRequestSchema, hrsResponseSchema]

schemasObject :: [JsonSchema] -> Value
schemasObject schemas = object
  [ Key.fromText name .= schemaValue
  | (name, schemaValue) <- Map.toList schemaMap
  ]
  where
    schemaMap = Map.fromList
      [ (jsonSchemaName schema, jsonSchemaValue schema)
      | schema <- schemas
      ]

openApiDescription :: Text
openApiDescription = Text.unlines
  [ "Resource-oriented HTTP automation contract for topo-seer 1.0."
  , "The document is generated from the same route metadata used by dispatch."
  , "Protected routes use optional Bearer authentication when a token is configured; /health remains unauthenticated."
  , "Errors use the shared ErrorEnvelope schema and echo X-Request-Id when the request supplies it."
  ]

versioningPolicy :: Text
versioningPolicy = Text.unlines
  [ "The HTTP API is versioned independently from the topo-seer package."
  , "The /version route exposes api_version=1 for this contract."
  , "Within major version 1, additive routes, fields, enum values, and schemas may be introduced without breaking existing clients."
  , "Breaking route, request, response, authentication, or error-envelope changes require a new major API version or an explicitly documented migration window."
  ]

tagObjects :: [HttpRouteSpec] -> [Value]
tagObjects specs =
  [ object
      [ "name" .= tag
      , "description" .= tagDescription tag
      ]
  | tag <- Map.keys (Map.fromList [(hrsTag spec, ()) | spec <- specs])
  ]

tagDescription :: Text -> Text
tagDescription "meta" = "Health, version, and OpenAPI discovery."
tagDescription "events" = "Buffered and streaming service events."
tagDescription "state" = "Application and view-mode state reads."
tagDescription "ui" = "UI state, panel, widget, input, and selection operations."
tagDescription "config" = "Runtime configuration, slider, and enum operations."
tagDescription "presets" = "Preset save, load, and list operations."
tagDescription "world" = "World generation, metadata, persistence, and naming operations."
tagDescription "terrain" = "Terrain, hex, chunk, export, search, and sample operations."
tagDescription "overlays" = "Overlay schema, provenance, field, import, export, and selection operations."
tagDescription "editor" = "Terrain editor state and brush operations."
tagDescription "pipeline" = "Generation pipeline stage and DAG controls."
tagDescription "plugins" = "Plugin status, dependency, parameter, and lifecycle operations."
tagDescription "data" = "Plugin-owned data-resource discovery and CRUD operations."
tagDescription "simulation" = "Simulation state, DAG, auto-tick, and tick operations."
tagDescription "logs" = "Runtime log queries."
tagDescription "screenshots" = "Screenshot capture operations."
tagDescription "camera" = "Viewport camera read and mutation operations."
tagDescription tag = "Topo HTTP API operations tagged as " <> tag <> "."

schemaExample :: Value -> Maybe Value
schemaExample schema = do
  name <- schemaRefName schema
  lookup name schemaExamples

schemaRefName :: Value -> Maybe Text
schemaRefName (Object schema) = do
  String ref <- KM.lookup "$ref" schema
  Text.stripPrefix "#/components/schemas/" ref
schemaRefName _ = Nothing

schemaExamples :: [(Text, Value)]
schemaExamples =
  [ ("HealthResponse", object ["status" .= ("ok" :: Text)])
  , ("VersionResponse", object
      [ "name" .= ("topo-seer" :: Text)
      , "version" .= ("1.0.0.0" :: Text)
      , "api_version" .= ("1" :: Text)
      ])
  , ("UiSeedSetRequest", object ["seed" .= (123 :: Int)])
  , ("UiSeedSetResponse", object ["seed" .= (123 :: Int)])
  , ("DataRecordCreateRequest", object
      [ "plugin" .= ("civ" :: Text)
      , "resource" .= ("settlements" :: Text)
      , "fields" .= object
          [ "name" .= ("Riverford" :: Text)
          , "population" .= (1200 :: Int)
          ]
      ])
  , ("DataRecordCreateResponse", object
      [ "plugin" .= ("civ" :: Text)
      , "resource" .= ("settlements" :: Text)
      , "created" .= True
      , "record" .= object
          [ "id" .= ("riverford" :: Text)
          , "name" .= ("Riverford" :: Text)
          ]
      ])
  , ("ErrorEnvelope", errorEnvelopeExample)
  ]

errorEnvelopeExample :: Value
errorEnvelopeExample = errorEnvelopeExampleWithDetails "validation_failed" "validation failed"
  [ object
      [ "path" .= (["name"] :: [Text])
      , "code" .= ("missing_field" :: Text)
      , "message" .= ("missing required field 'name'" :: Text)
      ]
  ]

errorEnvelopeExampleFor :: Text -> Text -> Value
errorEnvelopeExampleFor code message = errorEnvelopeExampleWithDetails code message []

errorEnvelopeExampleWithDetails :: Text -> Text -> [Value] -> Value
errorEnvelopeExampleWithDetails code message details = object
  [ "error" .= object
      [ "code" .= code
      , "message" .= message
      , "request_id" .= ("req-123" :: Text)
      , "details" .= details
      ]
  ]

errorEnvelopeSchema :: JsonSchema
errorEnvelopeSchema = JsonSchema "ErrorEnvelope" $ object
  [ "type" .= ("object" :: Text)
  , "required" .= (["error"] :: [Text])
  , "properties" .= object
      [ "error" .= object
          [ "type" .= ("object" :: Text)
          , "required" .= (["code", "message", "details"] :: [Text])
          , "properties" .= object
              [ "code" .= object
                  [ "type" .= ("string" :: Text)
                  , "enum" .= ([ "invalid_request", "validation_failed", "unauthorized"
                                , "not_found", "rejected", "internal_error", "unavailable"
                                , "resource_not_found", "operation_not_supported", "record_not_found"
                                , "duplicate_key", "schema_validation_failed", "permission_denied"
                                , "conflict", "unsupported_media_type", "plugin_unavailable"
                                , "external_data_source_unavailable", "query_unsupported", "timeout"
                                , "data_resource_error"
                                ] :: [Text])
                  ]
              , "message" .= object ["type" .= ("string" :: Text)]
              , "request_id" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Request correlation id copied from X-Request-Id when present." :: Text)
                  ]
              , "details" .= object
                  [ "type" .= ("array" :: Text)
                  , "items" .= object
                      [ "type" .= ("object" :: Text)
                      , "properties" .= object
                          [ "path" .= object
                              [ "type" .= ("array" :: Text)
                              , "items" .= object ["type" .= ("string" :: Text)]
                              ]
                          , "code" .= object ["type" .= ("string" :: Text)]
                          , "message" .= object ["type" .= ("string" :: Text)]
                          ]
                      ]
                  ]
              ]
          ]
      ]
  , "example" .= errorEnvelopeExample
  ]
