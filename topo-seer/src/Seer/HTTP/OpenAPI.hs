{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Small OpenAPI document generator for the HTTP route table.
module Seer.HTTP.OpenAPI
  ( HttpRouteSpec(..)
  , QueryParamSpec(..)
  , RouteBody(..)
  , openApiDocument
  , routePathText
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

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
  } deriving (Eq, Show)

-- | Runtime route metadata. The HTTP server and OpenAPI response are generated
-- from this table so handler coverage and public contract paths cannot drift.
data HttpRouteSpec = HttpRouteSpec
  { hrsMethod :: !Text
  , hrsPath :: ![Text]
  , hrsOperationId :: !Text
  , hrsSummary :: !Text
  , hrsTag :: !Text
  , hrsServiceMethod :: !(Maybe Text)
  , hrsRequestBody :: !RouteBody
  , hrsQueryParams :: ![QueryParamSpec]
  } deriving (Eq, Show)

routePathText :: HttpRouteSpec -> Text
routePathText spec = "/" <> Text.intercalate "/" (hrsPath spec)

openApiDocument :: [HttpRouteSpec] -> Value
openApiDocument specs = object
  [ "openapi" .= ("3.0.3" :: Text)
  , "info" .= object
      [ "title" .= ("Topo Seer HTTP API" :: Text)
      , "version" .= ("0.1.0" :: Text)
      ]
  , "paths" .= pathsObject specs
  , "components" .= object
      [ "securitySchemes" .= object
          [ "bearerAuth" .= object
              [ "type" .= ("http" :: Text)
              , "scheme" .= ("bearer" :: Text)
              ]
          ]
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
      , "responses" .= object
          [ "200" .= object ["description" .= ("OK" :: Text)]
          , "400" .= object ["description" .= ("Invalid request" :: Text)]
          , "401" .= object ["description" .= ("Unauthorized" :: Text)]
          , "404" .= object ["description" .= ("Not found" :: Text)]
          , "500" .= object ["description" .= ("Internal server error" :: Text)]
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
      OptionalJsonRequestBody -> ["requestBody" .= jsonRequestBody False]
      RequiredJsonRequestBody -> ["requestBody" .= jsonRequestBody True]

queryParameterObject :: QueryParamSpec -> Value
queryParameterObject param = object
  [ "name" .= qpsName param
  , "in" .= ("query" :: Text)
  , "required" .= qpsRequired param
  , "description" .= qpsDescription param
  , "schema" .= object ["type" .= ("string" :: Text)]
  ]

jsonRequestBody :: Bool -> Value
jsonRequestBody required = object
  [ "required" .= required
  , "content" .= object
      [ "application/json" .= object
          [ "schema" .= object ["type" .= ("object" :: Text)]
          ]
      ]
  ]
