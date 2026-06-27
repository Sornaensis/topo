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

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as Key
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
      , "version" .= ("0.1.0" :: Text)
      ]
  , "paths" .= pathsObject specs
  , "components" .= componentsObject specs
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
      , "responses" .= object
          [ "200" .= okResponseObject spec
          , "400" .= responseObject "Invalid request" (Just errorEnvelopeSchema)
          , "401" .= responseObject "Unauthorized" (Just errorEnvelopeSchema)
          , "404" .= responseObject "Not found" (Just errorEnvelopeSchema)
          , "409" .= responseObject "Rejected" (Just errorEnvelopeSchema)
          , "500" .= responseObject "Internal server error" (Just errorEnvelopeSchema)
          , "503" .= responseObject "Unavailable" (Just errorEnvelopeSchema)
          ]
      ]
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
jsonContent schema = object
  [ "application/json" .= object
      [ "schema" .= schema
      ]
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
                  , "enum" .= ([ "invalid_json", "invalid_request", "validation_failed", "unauthorized"
                                , "not_found", "rejected", "internal_error", "unavailable"
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
  , "example" .= object
      [ "error" .= object
          [ "code" .= ("validation_failed" :: Text)
          , "message" .= ("validation failed" :: Text)
          , "request_id" .= ("req-123" :: Text)
          , "details" .=
              [ object
                  [ "path" .= (["name"] :: [Text])
                  , "code" .= ("missing_field" :: Text)
                  , "message" .= ("missing required field 'name'" :: Text)
                  ]
              ]
          ]
      ]
  ]
