{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- | WAI/Warp HTTP server over AppService.
module Seer.HTTP.Server
  ( HttpServerConfig(..)
  , defaultHttpServerConfig
  , parseHttpBind
  , runHttpServer
  , forkHttpServer
  , httpApplication
  , handleHttpRequest
  , HttpRequest(..)
  , HttpResponse(..)
  , httpRouteSpecs
  , publicHttpRouteSpecs
  , friendlyHttpRouteSpecs
  , commandHttpRouteSpecs
  , headlessHttpAppService
  ) where

import Codec.Picture (Image(..), PixelRGBA8(..), encodePng)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (TChan, atomically, readTChan)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.Aeson (Value(..), eitherDecode, object, (.=))
import Data.Char (toLower)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Builder (Builder, byteString, lazyByteString, stringUtf8)
import Data.Foldable (asum, toList)
import Data.Maybe (fromMaybe)
import Data.Scientific (scientific)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector.Storable as VS
import Text.Read (readMaybe)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Lazy as LBS

import Seer.Command.AppServiceAdapter (commandAppService)
import Seer.HTTP.Auth
import Seer.HTTP.API
import Seer.HTTP.OpenAPI
import Seer.Service.AppService
import Seer.Service.Context (ServiceContext(..))
import Seer.Service.Events
import Seer.Service.Types

-- | HTTP runtime configuration. The default is intentionally loopback-only.
data HttpServerConfig = HttpServerConfig
  { hscBindHost :: !String
  , hscBindPort :: !Int
  , hscBearerToken :: !(Maybe Text)
  } deriving (Eq, Show)

defaultHttpServerConfig :: HttpServerConfig
defaultHttpServerConfig = HttpServerConfig
  { hscBindHost = "127.0.0.1"
  , hscBindPort = 7373
  , hscBearerToken = Nothing
  }

-- | Parse @HOST:PORT@ bindings used by @topo-seer --http@.
parseHttpBind :: String -> Maybe (String, Int)
parseHttpBind input =
  case breakOnLast ':' input of
    Nothing -> Nothing
    Just (host, portText)
      | null host -> Nothing
      | [(port, "")] <- reads portText
      , port > 0
      , port <= (65535 :: Int) -> Just (host, port)
      | otherwise -> Nothing

breakOnLast :: Char -> String -> Maybe (String, String)
breakOnLast needle input = go input Nothing
  where
    go [] found = found
    go (c:cs) found
      | c == needle = go cs (Just (take (length input - length cs - 1) input, cs))
      | otherwise = go cs found

runHttpServer :: HttpServerConfig -> AppService -> ServiceContext -> IO ()
runHttpServer cfg app ctx = do
  case validateHttpAuthConfig (HttpAuthConfig (hscBindHost cfg) (hscBearerToken cfg)) of
    Left err -> fail (Text.unpack err)
    Right () -> pure ()
  Warp.runSettings settings (httpApplication cfg app ctx)
  where
    settings = Warp.setHost (fromStringHost (hscBindHost cfg))
      $ Warp.setPort (hscBindPort cfg) Warp.defaultSettings

forkHttpServer :: HttpServerConfig -> AppService -> ServiceContext -> IO ThreadId
forkHttpServer cfg app ctx = do
  case validateHttpAuthConfig (HttpAuthConfig (hscBindHost cfg) (hscBearerToken cfg)) of
    Left err -> fail (Text.unpack err)
    Right () -> pure ()
  forkIO (runHttpServer cfg app ctx)

fromStringHost :: String -> Warp.HostPreference
fromStringHost = fromString

-- | WAI application used by both the real server and future integration tests.
httpApplication :: HttpServerConfig -> AppService -> ServiceContext -> Wai.Application
httpApplication cfg app ctx request respond = do
  let method = Text.decodeUtf8 (Wai.requestMethod request)
      path = Wai.pathInfo request
      headers = decodeHeaders (Wai.requestHeaders request)
      withReqId = withRequestId (requestIdFromHeaders headers)
      unauthorized = jsonResponse 401 (errorEnvelope "unauthorized" "missing or invalid bearer token" [])
  case lookupRoute method path of
    Nothing -> respond (waiResponse (withReqId (jsonResponse 404 (errorEnvelope "not_found" "route not found" []))))
    Just spec
      | not (isPublicRoute spec)
      , not (isAuthorized (hscBearerToken cfg) headers) -> respond (waiResponse (withReqId unauthorized))
      | otherwise -> do
          body <- Wai.strictRequestBody request
          case decodeHttpRequestBody spec body of
            Left rsp -> respond (waiResponse (withReqId rsp))
            Right mBody -> case validateHttpRequestBody spec mBody of
              Left rsp -> respond (waiResponse (withReqId rsp))
              Right () -> do
                let httpReq = HttpRequest
                      { hreqMethod = method
                      , hreqPath = path
                      , hreqQuery = map decodeQueryParam (Wai.queryString request)
                      , hreqHeaders = headers
                      , hreqBody = mBody
                      }
                case requestParams spec httpReq of
                  Left rsp -> respond (waiResponse (withReqId rsp))
                  Right _
                    | hrsOperationId spec == "events.list"
                    , eventStreamRequested request -> do
                        response <- eventStreamWaiResponse ctx (requestIdFromHeaders headers) (Wai.queryString request)
                        respond response
                    | otherwise -> do
                        rsp <- handleHttpRequest cfg app ctx httpReq
                        respond (waiResponse rsp)

-- | Pure request shape for route tests without opening a socket.
data HttpRequest = HttpRequest
  { hreqMethod :: !Text
  , hreqPath :: ![Text]
  , hreqQuery :: ![(Text, Maybe Text)]
  , hreqHeaders :: ![(Text, Text)]
  , hreqBody :: !(Maybe Value)
  } deriving (Eq, Show)

data HttpResponse = HttpResponse
  { hresStatusCode :: !Int
  , hresHeaders :: ![(Text, Text)]
  , hresBody :: !Value
  } deriving (Eq, Show)

handleHttpRequest :: HttpServerConfig -> AppService -> ServiceContext -> HttpRequest -> IO HttpResponse
handleHttpRequest cfg app ctx req =
  fmap (withRequestId (requestIdFromHeaders (hreqHeaders req))) $
  case lookupRoute (hreqMethod req) (hreqPath req) of
    Nothing -> pure (jsonResponse 404 (errorEnvelope "not_found" "route not found" []))
    Just spec
      | not (isPublicRoute spec)
      , not (isAuthorized (hscBearerToken cfg) (hreqHeaders req)) ->
          pure (jsonResponse 401 (errorEnvelope "unauthorized" "missing or invalid bearer token" []))
      | otherwise -> case validateHttpRequestBody spec (hreqBody req) of
          Left rsp -> pure rsp
          Right () -> case requestParams spec req of
            Left rsp -> pure rsp
            Right params -> handleRoute spec params
  where
    handleRoute spec params = case hrsOperationId spec of
      "meta.health" -> pure (jsonResponse 200 (object ["status" .= ("ok" :: Text)]))
      "meta.version" -> pure (jsonResponse 200 (object
        [ "name" .= ("topo-seer" :: Text)
        , "version" .= ("1.0.0.0" :: Text)
        , "api_version" .= ("1" :: Text)
        ]))
      "meta.openapi" -> pure (jsonResponse 200 (openApiDocument publicHttpRouteSpecs))
      "events.list" -> do
        events <- maybe (pure []) readBufferedServiceEvents (svcEventBus ctx)
        pure (jsonResponse 200 (object
          [ "events" .= map serviceEventEnvelopeJson events
          , "mode" .= ("polling" :: Text)
          ]))
      _ -> case hrsServiceMethod spec of
        Nothing -> pure (jsonResponse 500 (errorEnvelope "internal_error" "route has no handler" []))
        Just _method -> invokeService app ctx spec req params

isPublicRoute :: HttpRouteSpec -> Bool
isPublicRoute spec = hrsOperationId spec == "meta.health"

validateHttpRequestBody :: HttpRouteSpec -> Maybe Value -> Either HttpResponse ()
validateHttpRequestBody spec body =
  case hrsRequestBody spec of
    NoRequestBody -> case body of
      Nothing -> Right ()
      Just _ -> Left requestBodyNotAllowedError
    OptionalJsonRequestBody -> validateOptionalJsonBody body
    RequiredJsonRequestBody -> case body of
      Nothing -> Left requestBodyRequiredError
      Just value -> validateJsonObject value

validateOptionalJsonBody :: Maybe Value -> Either HttpResponse ()
validateOptionalJsonBody Nothing = Right ()
validateOptionalJsonBody (Just value) = validateJsonObject value

validateJsonObject :: Value -> Either HttpResponse ()
validateJsonObject (Object _) = Right ()
validateJsonObject _ = Left requestBodyMustBeObjectError

requestBodyNotAllowedError :: HttpResponse
requestBodyNotAllowedError = requestBodyPolicyError
  "unexpected_body"
  "request body is not allowed for this route"

requestBodyRequiredError :: HttpResponse
requestBodyRequiredError = requestBodyPolicyError
  "missing_body"
  "JSON request body is required for this route"

requestBodyMustBeObjectError :: HttpResponse
requestBodyMustBeObjectError = requestBodyPolicyError
  "invalid_body"
  "JSON request body must be an object"

requestBodyPolicyError :: Text -> Text -> HttpResponse
requestBodyPolicyError detailCode message =
  jsonResponse 400 (errorEnvelope "validation_failed" "validation failed" [errorDetail detailCode message])

requestParams :: HttpRouteSpec -> HttpRequest -> Either HttpResponse Value
requestParams spec req = do
  queryParams <- queryObject spec (hreqQuery req)
  case hreqBody req of
    Just value -> Right value
    Nothing
      | hrsMethod spec == "GET" -> Right queryParams
      | otherwise -> Right Null

queryObject :: HttpRouteSpec -> [(Text, Maybe Text)] -> Either HttpResponse Value
queryObject spec query = do
  validateRequiredQueryParams spec query
  fields <- traverse (queryField spec) query
  pure (object fields)

validateRequiredQueryParams :: HttpRouteSpec -> [(Text, Maybe Text)] -> Either HttpResponse ()
validateRequiredQueryParams spec query =
  case [param | param <- hrsQueryParams spec, missingRequired param] of
    param:_ -> Left (missingQueryParamError param)
    [] -> Right ()
  where
    missingRequired param =
      qpsRequired param && not (queryHasValue (qpsName param))
    queryHasValue name = any (\(key, value) -> key == name && value /= Nothing) query

queryField :: HttpRouteSpec -> (Text, Maybe Text) -> Either HttpResponse Pair
queryField spec (key, value) = do
  coerced <- case value of
    Nothing -> maybe (Right Null) (Left . missingQueryValueError) (queryParamSpec spec key)
    Just raw -> maybe (Right (String raw)) (`queryValueFor` raw) (queryParamSpec spec key)
  pure (Key.fromText key .= coerced)

queryParamSpec :: HttpRouteSpec -> Text -> Maybe QueryParamSpec
queryParamSpec spec key = case [param | param <- hrsQueryParams spec, qpsName param == key] of
  param:_ -> Just param
  [] -> Nothing

queryValueFor :: QueryParamSpec -> Text -> Either HttpResponse Value
queryValueFor param value =
  case querySchemaType (qpsSchema param) of
    Just "integer" -> case parseQueryInteger value of
      Just integer -> Right (Number (scientific integer 0))
      Nothing -> Left (invalidQueryParamError (qpsName param) "must be a base-10 integer")
    Just "boolean" -> case parseQueryBoolean value of
      Just boolean -> Right (Bool boolean)
      Nothing -> Left (invalidQueryParamError (qpsName param) "must be true or false")
    _ -> Right (String value)

querySchemaType :: Value -> Maybe Text
querySchemaType (Object schema) = case KM.lookup "type" schema of
  Just (String schemaType) -> Just schemaType
  _ -> Nothing
querySchemaType _ = Nothing

parseQueryInteger :: Text -> Maybe Integer
parseQueryInteger value
  | Text.null digits = Nothing
  | Text.all asciiDigit digits = applySign <$> readMaybe (Text.unpack digits)
  | otherwise = Nothing
  where
    (applySign, digits) = case Text.uncons value of
      Just ('-', rest) -> (negate, rest)
      Just ('+', rest) -> (id, rest)
      _ -> (id, value)
    asciiDigit c = c >= '0' && c <= '9'

parseQueryBoolean :: Text -> Maybe Bool
parseQueryBoolean "true" = Just True
parseQueryBoolean "false" = Just False
parseQueryBoolean _ = Nothing

missingQueryParamError :: QueryParamSpec -> HttpResponse
missingQueryParamError param = queryParamPolicyError
  "missing_query_param"
  ("missing required query parameter '" <> qpsName param <> "'")

missingQueryValueError :: QueryParamSpec -> HttpResponse
missingQueryValueError param = invalidQueryParamError (qpsName param) "requires a value"

invalidQueryParamError :: Text -> Text -> HttpResponse
invalidQueryParamError name expectation = queryParamPolicyError
  "invalid_query_param"
  ("query parameter '" <> name <> "' " <> expectation)

queryParamPolicyError :: Text -> Text -> HttpResponse
queryParamPolicyError detailCode message =
  jsonResponse 400 (errorEnvelope "validation_failed" "validation failed" [errorDetail detailCode message])

invokeService :: AppService -> ServiceContext -> HttpRouteSpec -> HttpRequest -> Value -> IO HttpResponse
invokeService app ctx spec req params = do
  result <- try (runServiceOperation app ctx method params) :: IO (Either SomeException ServiceResult)
  case result of
    Left _ -> do
      let body = errorEnvelope "internal_error" "service handler exception" []
      publishHttpEvent ctx spec req ServiceEventError "error" body
      pure (jsonResponse 500 body)
    Right (Right response) -> do
      publishHttpEvent ctx spec req ServiceEventInfo "ok" (serviceResponseBody response)
      pure (jsonResponse 200 (serviceResponseBody response))
    Right (Left err) -> do
      let errorResponse = serviceErrorResponse err
      publishHttpEvent ctx spec req ServiceEventWarn "error" (hresBody errorResponse)
      pure errorResponse
  where
    method = fromMaybe "" (hrsServiceMethod spec)

publishHttpEvent :: ServiceContext -> HttpRouteSpec -> HttpRequest -> ServiceEventSeverity -> Text -> Value -> IO ()
publishHttpEvent ctx spec req severity status body =
  case svcEventBus ctx of
    Nothing -> pure ()
    Just bus -> do
      _ <- publishServiceEvent bus ServiceEventEnvelope
        { serviceEventTopic = eventTopic spec status
        , serviceEventSource = ServiceEventFromHttp
        , serviceEventSeverity = severity
        , serviceEventSequence = Nothing
        , serviceEventCorrelationId = requestIdFromHeaders (hreqHeaders req)
        , serviceEventPayload = object
            [ "status" .= status
            , "operation_id" .= hrsOperationId spec
            , "http_method" .= hrsMethod spec
            , "path" .= routePathText spec
            , "service_method" .= hrsServiceMethod spec
            , "result" .= eventBodySummary body
            ]
        }
      pure ()

eventTopic :: HttpRouteSpec -> Text -> Text
eventTopic spec status
  | status == "error" = base <> ".failed"
  | otherwise = base
  where
    op = hrsOperationId spec
    base
      | op == "world.generate" = "world.generation.requested"
      | op == "world.generationStatus" = "world.generation.status"
      | op == "plugins.list" || op == "plugins.status" || op == "plugins.state" || op == "plugins.dependencies" = "plugins.status"
      | op == "plugins.setEnabled" || op == "plugins.params.set" = "plugins.changed"
      | op == "logs.get" = "logs.read"
      | op == "data.records.create" || op == "data.records.update" || op == "data.records.delete" = "data.resources.changed"
      | "data." `Text.isPrefixOf` op = "data.resources.status"
      | op == "simulation.tick" = "simulation.tick"
      | op == "simulation.autoTick.set" = "simulation.auto_tick.changed"
      | "simulation." `Text.isPrefixOf` op = "simulation.status"
      | "ui." `Text.isPrefixOf` op || "camera." `Text.isPrefixOf` op || "overlays." `Text.isPrefixOf` op = "ui.state.changed"
      | otherwise = "http." <> op

eventBodySummary :: Value -> Value
eventBodySummary (Object obj) =
  case KM.lookup "error" obj of
    Just (Object err) -> object
      [ "type" .= ("error" :: Text)
      , "code" .= lookupStringField err "code"
      , "message" .= lookupStringField err "message"
      ]
    _ -> object $ baseFields <> keptFields
  where
    baseFields =
      [ "type" .= ("object" :: Text)
      , "keys" .= map Key.toText (KM.keys obj)
      ]
    keptFields =
      [ Key.fromText key .= value
      | key <- eventSummaryFieldNames
      , Just value <- [KM.lookup (Key.fromText key) obj]
      ] <>
      [ "external_data_sources" .= summary
      | Just summary <- [pluginExternalDataSourceEventSummary obj]
      ]
eventBodySummary (Array values) = object
  [ "type" .= ("array" :: Text)
  , "count" .= length (toList values)
  ]
eventBodySummary (String _) = object ["type" .= ("string" :: Text)]
eventBodySummary (Number _) = object ["type" .= ("number" :: Text)]
eventBodySummary (Bool _) = object ["type" .= ("boolean" :: Text)]
eventBodySummary Null = object ["type" .= ("null" :: Text)]

eventSummaryFieldNames :: [Text]
eventSummaryFieldNames =
  [ "status"
  , "generating"
  , "chunk_count"
  , "seed"
  , "auto_tick"
  , "tick_count"
  , "plugin_count"
  , "record_count"
  , "total_count"
  , "count"
  , "total"
  ]

pluginExternalDataSourceEventSummary :: KM.KeyMap Value -> Maybe Value
pluginExternalDataSourceEventSummary obj = case KM.lookup "plugins" obj of
  Just (Array plugins) ->
    let pluginSummaries = map pluginExternalSummary (toList plugins)
        totalCount = sum [count | (_, count, _) <- pluginSummaries]
        totalFailures = sum [failures | (_, _, failures) <- pluginSummaries]
        visiblePlugins = [summary | (summary, count, failures) <- pluginSummaries, count > 0 || failures > 0]
    in Just $ object
      [ "count" .= totalCount
      , "failures" .= totalFailures
      , "plugins" .= visiblePlugins
      ]
  _ -> case KM.lookup "external_data_sources" obj of
    Just (Array diagnostics) ->
      let diagnosticValues = toList diagnostics
      in Just $ object
        [ "count" .= length diagnosticValues
        , "failures" .= externalDiagnosticsFailureCount diagnosticValues
        , "diagnostics" .= map externalDiagnosticSummary diagnosticValues
        ]
    _ -> Nothing

pluginExternalSummary :: Value -> (Value, Int, Int)
pluginExternalSummary (Object plugin) =
  let pluginName = case lookupStringField plugin "name" of
        Just name -> Just name
        Nothing -> lookupStringField plugin "plugin"
      diagnostics = case KM.lookup "external_data_sources" plugin of
        Just (Array values) -> toList values
        _ -> []
      failureCount = externalDiagnosticsFailureCount diagnostics
  in ( object
        [ "plugin" .= pluginName
        , "count" .= length diagnostics
        , "failures" .= failureCount
        , "diagnostics" .= map externalDiagnosticSummary diagnostics
        ]
     , length diagnostics
     , failureCount
     )
pluginExternalSummary _ = (object ["count" .= (0 :: Int), "failures" .= (0 :: Int), "diagnostics" .= ([] :: [Value])], 0, 0)

externalDiagnosticSummary :: Value -> Value
externalDiagnosticSummary (Object diagnostic) = object
  [ "role" .= lookupStringField diagnostic "role"
  , "provider" .= lookupStringField diagnostic "provider"
  , "consumer" .= lookupStringField diagnostic "consumer"
  , "source" .= lookupStringField diagnostic "source"
  , "grant" .= lookupStringField diagnostic "grant"
  , "status" .= lookupStringField diagnostic "status"
  , "availability" .= lookupStringField diagnostic "availability"
  , "failure_reason" .= lookupStringField diagnostic "failure_reason"
  , "resource_availability" .= KM.lookup "resource_availability" diagnostic
  , "grants" .= externalGrantSummaries diagnostic
  ]
externalDiagnosticSummary _ = object []

externalGrantSummaries :: KM.KeyMap Value -> [Value]
externalGrantSummaries diagnostic = case KM.lookup "grants" diagnostic of
  Just (Array grants) -> map externalGrantSummary (toList grants)
  _ -> []

externalGrantSummary :: Value -> Value
externalGrantSummary (Object grant) = object
  [ "name" .= lookupStringField grant "name"
  , "status" .= lookupStringField grant "status"
  , "availability" .= lookupStringField grant "availability"
  , "failure_reason" .= lookupStringField grant "failure_reason"
  , "resource_availability" .= KM.lookup "resource_availability" grant
  ]
externalGrantSummary _ = object []

externalDiagnosticsFailureCount :: [Value] -> Int
externalDiagnosticsFailureCount = length . filter externalDiagnosticHasFailure

externalDiagnosticHasFailure :: Value -> Bool
externalDiagnosticHasFailure (Object diagnostic) =
  fieldPresent diagnostic "failure_reason"
    || any externalGrantHasFailure (externalGrantValues diagnostic)
externalDiagnosticHasFailure _ = False

externalGrantHasFailure :: Value -> Bool
externalGrantHasFailure (Object grant) = fieldPresent grant "failure_reason"
externalGrantHasFailure _ = False

externalGrantValues :: KM.KeyMap Value -> [Value]
externalGrantValues diagnostic = case KM.lookup "grants" diagnostic of
  Just (Array grants) -> toList grants
  _ -> []

fieldPresent :: KM.KeyMap Value -> Text -> Bool
fieldPresent obj field = case KM.lookup (Key.fromText field) obj of
  Just Null -> False
  Just (String value) -> not (Text.null value)
  Just _ -> True
  Nothing -> False

lookupStringField :: KM.KeyMap Value -> Text -> Maybe Text
lookupStringField obj field = case KM.lookup (Key.fromText field) obj of
  Just (String value) -> Just value
  _ -> Nothing

serviceErrorResponse :: ServiceError -> HttpResponse
serviceErrorResponse err = jsonResponse (serviceErrorHTTPStatus err) (errorEnvelope
  (serviceErrorCode err)
  (serviceErrorMessage err)
  [ object
      [ "path" .= serviceErrorDetailPath detail
      , "code" .= serviceErrorDetailCode detail
      , "message" .= serviceErrorDetailMessage detail
      ]
  | detail <- serviceErrorDetails err
  ])

errorEnvelope :: Text -> Text -> [Value] -> Value
errorEnvelope code message details = object
  [ "error" .= object
      [ "code" .= code
      , "message" .= message
      , "details" .= details
      ]
  ]

requestIdHeader :: Text
requestIdHeader = "x-request-id"

requestIdFromHeaders :: [(Text, Text)] -> Maybe Text
requestIdFromHeaders headers = lookup requestIdHeader
  [ (Text.toLower name, value)
  | (name, value) <- headers
  ]

withRequestId :: Maybe Text -> HttpResponse -> HttpResponse
withRequestId Nothing response = response
withRequestId (Just requestId) response = response
  { hresHeaders = setHeader requestIdHeader requestId (hresHeaders response)
  , hresBody = addRequestIdToError requestId (hresBody response)
  }

setHeader :: Text -> Text -> [(Text, Text)] -> [(Text, Text)]
setHeader name value headers =
  (name, value) : filter ((/= Text.toLower name) . Text.toLower . fst) headers

addRequestIdToError :: Text -> Value -> Value
addRequestIdToError requestId (Object body) = case KM.lookup (Key.fromText "error") body of
  Just (Object err) -> Object $ KM.insert (Key.fromText "error")
    (Object (KM.insert (Key.fromText "request_id") (String requestId) err))
    body
  _ -> Object body
addRequestIdToError _ body = body

jsonResponse :: Int -> Value -> HttpResponse
jsonResponse status body = HttpResponse
  { hresStatusCode = status
  , hresHeaders = [("content-type", "application/json")]
  , hresBody = body
  }

waiResponse :: HttpResponse -> Wai.Response
waiResponse HttpResponse{..} = Wai.responseLBS
  (HTTP.mkStatus hresStatusCode "")
  (waiResponseHeaders hresHeaders)
  (Aeson.encode hresBody)

waiResponseHeaders :: [(Text, Text)] -> HTTP.ResponseHeaders
waiResponseHeaders headers =
  (HTTP.hContentType, "application/json") :
    [ ("X-Request-Id", Text.encodeUtf8 requestId)
    | (name, requestId) <- headers
    , Text.toLower name == requestIdHeader
    ]

decodeHttpRequestBody :: HttpRouteSpec -> LBS.ByteString -> Either HttpResponse (Maybe Value)
decodeHttpRequestBody spec body
  | hrsRequestBody spec == NoRequestBody
  , not (LBS.null body) = Left requestBodyNotAllowedError
  | otherwise = case decodeRequestBody body of
      Left err -> Left (invalidJsonError err)
      Right value -> Right value

decodeRequestBody :: LBS.ByteString -> Either Text (Maybe Value)
decodeRequestBody body
  | LBS.null body = Right Nothing
  | otherwise = case eitherDecode body of
      Left err -> Left (Text.pack err)
      Right value -> Right (Just value)

invalidJsonError :: Text -> HttpResponse
invalidJsonError message =
  requestBodyPolicyError "invalid_body" ("request body is malformed JSON: " <> message)

errorDetail :: Text -> Text -> Value
errorDetail detailCode message = object
  [ "code" .= detailCode
  , "message" .= message
  ]

decodeQueryParam :: (BS.ByteString, Maybe BS.ByteString) -> (Text, Maybe Text)
decodeQueryParam (key, value) = (Text.decodeUtf8 key, fmap Text.decodeUtf8 value)

decodeHeaders :: [HTTP.Header] -> [(Text, Text)]
decodeHeaders headers =
  maybe [] (\value -> [("authorization", Text.decodeUtf8 value)]) (lookup HTTP.hAuthorization headers)
    <> maybe [] (\value -> [(requestIdHeader, Text.decodeUtf8 value)]) (lookup "X-Request-Id" headers)

eventStreamRequested :: Wai.Request -> Bool
eventStreamRequested request =
  acceptsEventStream (lookup HTTP.hAccept (Wai.requestHeaders request))
    || queryFlag "stream" (Wai.queryString request)

acceptsEventStream :: Maybe BS.ByteString -> Bool
acceptsEventStream = maybe False (BSC.isInfixOf "text/event-stream" . BSC.map toLower)

queryFlag :: Text -> HTTP.Query -> Bool
queryFlag name query = case lookup (Text.encodeUtf8 name) query of
  Just Nothing -> True
  Just (Just value) -> Text.toLower (Text.decodeUtf8 value) `elem` ["1", "true", "yes", "sse"]
  Nothing -> False

eventStreamWaiResponse :: ServiceContext -> Maybe Text -> HTTP.Query -> IO Wai.Response
eventStreamWaiResponse ctx requestId query =
  case svcEventBus ctx of
    Nothing -> pure $ Wai.responseStream (HTTP.mkStatus 200 "OK") (eventStreamHeaders requestId) $ \write flush -> do
      write (byteString ": event bus unavailable\n\n")
      flush
    Just bus -> do
      (snapshot, live) <- serviceEventSnapshotAndSubscribe bus
      pure $ Wai.responseStream (HTTP.mkStatus 200 "OK") (eventStreamHeaders requestId) $ \write flush ->
        streamEvents snapshot live (eventStreamLimit query) write flush

eventStreamHeaders :: Maybe Text -> HTTP.ResponseHeaders
eventStreamHeaders requestId =
  [ (HTTP.hContentType, "text/event-stream; charset=utf-8")
  , ("Cache-Control", "no-cache")
  , ("X-Accel-Buffering", "no")
  ] <> maybe [] (\rid -> [("X-Request-Id", Text.encodeUtf8 rid)]) requestId

eventStreamLimit :: HTTP.Query -> Maybe Int
eventStreamLimit query = do
  raw <- lookup (Text.encodeUtf8 "limit") query >>= id
  n <- readMaybe (Text.unpack (Text.decodeUtf8 raw))
  Just (max 0 n)

streamEvents
  :: [ServiceEventEnvelope]
  -> TChan ServiceEventEnvelope
  -> Maybe Int
  -> (Builder -> IO ())
  -> IO ()
  -> IO ()
streamEvents snapshot live limitEvents write flush =
  case limitEvents of
    Just limitCount ->
      mapM_ send (take limitCount snapshot)
    Nothing -> do
      mapM_ send snapshot
      forever (atomically (readTChan live) >>= send)
  where
    send event = do
      write (sseEventBuilder event)
      flush

sseEventBuilder :: ServiceEventEnvelope -> Builder
sseEventBuilder event =
  maybe mempty (\seqNo -> stringUtf8 ("id: " <> show seqNo <> "\n")) (serviceEventSequence event)
    <> byteString "event: "
    <> byteString (Text.encodeUtf8 (serviceEventTopic event))
    <> byteString "\n"
    <> byteString "data: "
    <> lazyByteString (Aeson.encode (serviceEventEnvelopeJson event))
    <> byteString "\n\n"

lookupRoute :: Text -> [Text] -> Maybe HttpRouteSpec
lookupRoute method path = asum
  [ if hrsMethod spec == method && hrsPath spec == path then Just spec else Nothing
  | spec <- httpRouteSpecs
  ]

httpRouteSpecs :: [HttpRouteSpec]
httpRouteSpecs = friendlyHttpRouteSpecs <> commandHttpRouteSpecs

-- | Routes published in the public OpenAPI contract. Internal command-shaped
-- compatibility routes remain dispatchable but are intentionally omitted.
publicHttpRouteSpecs :: [HttpRouteSpec]
publicHttpRouteSpecs = friendlyHttpRouteSpecs

friendlyHttpRouteSpecs :: [HttpRouteSpec]
friendlyHttpRouteSpecs = map annotateHttpRouteSpec
  [ special "GET" ["health"] "meta.health" "meta" "Check server health."
  , special "GET" ["version"] "meta.version" "meta" "Read topo-seer and API version metadata."
  , special "GET" ["openapi.json"] "meta.openapi" "meta" "Read the OpenAPI contract."
  , specialWithQuery "GET" ["events"] "events.list" "events" "Read buffered events or stream them as Server-Sent Events."
      [ optionalQueryWithSchema "stream" "When true, return text/event-stream instead of JSON." queryBooleanSchema
      , optionalQueryWithSchema "limit" "Maximum SSE events to stream before closing." queryIntegerSchema
      ]

  , service "GET" ["state"] "state.get" "state" "get_state" "Read current application state." NoRequestBody
  , service "GET" ["state", "view-modes"] "state.viewModes" "state" "get_view_modes" "List view modes." NoRequestBody
  , service "GET" ["state", "views"] "state.views" "state" "get_views" "Read layered view state and choices." NoRequestBody
  , service "GET" ["ui", "state"] "ui.state" "ui" "get_ui_state" "Read UI state." NoRequestBody

  , serviceWithQuery "GET" ["config", "sliders"] "config.sliders.list" "config" "get_sliders" "List sliders." NoRequestBody
      [ optionalQueryWithSchema "tab" "Filter by config tab." (queryEnumSchema ["terrain", "planet", "climate", "weather", "biome", "erosion"])
      ]
  , service "POST" ["config", "sliders", "get"] "config.sliders.get" "config" "get_slider" "Read one slider." RequiredJsonRequestBody
  , service "POST" ["config", "sliders"] "config.sliders.set" "config" "set_slider" "Set one slider." RequiredJsonRequestBody
  , service "PATCH" ["config", "sliders"] "config.sliders.setMany" "config" "set_sliders" "Set multiple sliders." RequiredJsonRequestBody
  , service "POST" ["config", "sliders", "reset"] "config.sliders.reset" "config" "reset_sliders" "Reset sliders." OptionalJsonRequestBody
  , service "GET" ["config", "summary"] "config.summary" "config" "get_config_summary" "Read config summary." NoRequestBody
  , serviceWithQuery "GET" ["config", "enums"] "config.enums" "config" "get_enums" "Read enum values." NoRequestBody
      [requiredQuery "type" "Enum type to read."]
  , withResponseSchema presetsListResponseSchema $
      service "GET" ["presets"] "presets.list" "presets" "list_presets" "List presets." NoRequestBody
  , withSchemas presetsSaveRequestSchema presetsSaveResponseSchema $
      service "POST" ["presets"] "presets.save" "presets" "save_preset" "Save a preset." RequiredJsonRequestBody
  , withSchemas presetsLoadRequestSchema presetsLoadResponseSchema $
      service "POST" ["presets", "load"] "presets.load" "presets" "load_preset" "Load a preset." RequiredJsonRequestBody

  , service "POST" ["world", "generate"] "world.generate" "world" "generate" "Generate a world." OptionalJsonRequestBody
  , service "GET" ["world"] "world.meta" "world" "get_world_meta" "Read world metadata." NoRequestBody
  , service "GET" ["world", "generation-status"] "world.generationStatus" "world" "get_generation_status" "Read generation status." NoRequestBody
  , service "GET" ["worlds"] "worlds.list" "world" "list_worlds" "List saved worlds." NoRequestBody
  , service "POST" ["worlds", "save"] "worlds.save" "world" "save_world" "Save a world." RequiredJsonRequestBody
  , service "POST" ["worlds", "load"] "worlds.load" "world" "load_world" "Load a world." RequiredJsonRequestBody
  , service "PATCH" ["world", "name"] "world.name.set" "world" "set_world_name" "Set world name." RequiredJsonRequestBody

  , serviceWithQuery "GET" ["terrain", "hex"] "terrain.hex" "terrain" "get_hex" "Read one hex." NoRequestBody
      [ requiredQueryWithSchema "q" "Axial q coordinate." queryIntegerSchema
      , requiredQueryWithSchema "r" "Axial r coordinate." queryIntegerSchema
      ]
  , service "GET" ["terrain", "chunks"] "terrain.chunks" "terrain" "get_chunks" "List chunks." NoRequestBody
  , serviceWithQuery "GET" ["terrain", "chunk-summary"] "terrain.chunkSummary" "terrain" "get_chunk_summary" "Read chunk summary." NoRequestBody
      [requiredQueryWithSchema "chunk" "Chunk id." queryIntegerSchema]
  , service "GET" ["terrain", "stats"] "terrain.stats" "terrain" "get_terrain_stats" "Read terrain stats." NoRequestBody
  , service "GET" ["terrain", "overlays"] "terrain.overlays" "terrain" "get_overlays" "List overlays." NoRequestBody

  , service "GET" ["overlays"] "overlays.list" "overlays" "get_overlays" "List overlays." NoRequestBody
  , serviceWithQuery "GET" ["overlays", "schema"] "overlays.schema.get" "overlays" "get_overlay_schema" "Read an overlay schema." NoRequestBody
      [requiredQuery "overlay" "Overlay name."]
  , serviceWithQuery "GET" ["overlays", "provenance"] "overlays.provenance.get" "overlays" "get_overlay_provenance" "Read overlay provenance." NoRequestBody
      [requiredQuery "overlay" "Overlay name."]
  , serviceWithQuery "GET" ["overlays", "fields"] "overlays.fields.list" "overlays" "list_overlay_fields" "List overlay fields." NoRequestBody
      [optionalQuery "overlay" "Overlay name."]
  , service "POST" ["overlays", "export"] "overlays.export" "overlays" "export_overlay_data" "Export overlay data." RequiredJsonRequestBody
  , service "POST" ["overlays", "import", "validate"] "overlays.import.validate" "overlays" "validate_overlay_import" "Validate overlay import payload." RequiredJsonRequestBody
  , service "PUT" ["overlays", "current"] "overlays.current.set" "overlays" "set_overlay" "Set current overlay." RequiredJsonRequestBody
  , service "POST" ["overlays", "cycle"] "overlays.cycle" "overlays" "cycle_overlay" "Cycle overlay." RequiredJsonRequestBody
  , service "POST" ["overlays", "fields", "cycle"] "overlays.field.cycle" "overlays" "cycle_overlay_field" "Cycle overlay field." RequiredJsonRequestBody

  , service "POST" ["terrain", "search"] "terrain.search" "terrain" "find_hexes" "Find hexes." RequiredJsonRequestBody
  , service "POST" ["terrain", "export"] "terrain.export" "terrain" "export_terrain_data" "Export terrain data." OptionalJsonRequestBody
  , service "POST" ["terrain", "mesh", "export"] "terrain.mesh.export" "terrain" "export_mesh_data" "Export a terrain mesh patch." OptionalJsonRequestBody
  , service "POST" ["terrain", "sample", "export"] "terrain.sample.export" "terrain" "export_sample_data" "Export a terrain sample." RequiredJsonRequestBody

  , service "GET" ["editor"] "editor.state" "editor" "editor_get_state" "Read editor state." NoRequestBody
  , service "POST" ["editor", "toggle"] "editor.toggle" "editor" "editor_toggle" "Toggle editor." OptionalJsonRequestBody
  , service "POST" ["editor", "tool"] "editor.tool.set" "editor" "editor_set_tool" "Set editor tool." RequiredJsonRequestBody
  , service "PATCH" ["editor", "brush"] "editor.brush.set" "editor" "editor_set_brush" "Set editor brush." RequiredJsonRequestBody
  , service "POST" ["editor", "brush-stroke"] "editor.brushStroke" "editor" "editor_brush_stroke" "Queue a brush stroke." RequiredJsonRequestBody
  , service "POST" ["editor", "brush-line"] "editor.brushLine" "editor" "editor_brush_line" "Queue a brush line." RequiredJsonRequestBody
  , service "POST" ["editor", "biome"] "editor.biome.set" "editor" "editor_set_biome" "Set editor biome." RequiredJsonRequestBody
  , service "POST" ["editor", "form"] "editor.form.set" "editor" "editor_set_form" "Set editor form." RequiredJsonRequestBody
  , service "POST" ["editor", "hardness"] "editor.hardness.set" "editor" "editor_set_hardness" "Set editor hardness." RequiredJsonRequestBody
  , service "POST" ["editor", "undo"] "editor.undo" "editor" "editor_undo" "Undo editor action." NoRequestBody
  , service "POST" ["editor", "redo"] "editor.redo" "editor" "editor_redo" "Redo editor action." NoRequestBody

  , withResponseSchema pipelineGetResponseSchema $
      service "GET" ["pipeline"] "pipeline.get" "pipeline" "get_pipeline" "Read pipeline stages." NoRequestBody
  , withSchemas pipelineSetStageEnabledRequestSchema pipelineSetStageEnabledResponseSchema $
      service "PATCH" ["pipeline", "stages"] "pipeline.stage.setEnabled" "pipeline" "set_stage_enabled" "Enable or disable a stage." RequiredJsonRequestBody

  , withResponseSchema pluginListResponseSchema $
      service "GET" ["plugins"] "plugins.list" "plugins" "list_plugins" "List plugins."
        NoRequestBody
  , withResponseSchema pluginListResponseSchema $
      service "GET" ["plugins", "status"] "plugins.status" "plugins" "list_plugins" "Read plugin status."
        NoRequestBody
  , withResponseSchema pluginListResponseSchema $
      service "GET" ["plugins", "state"] "plugins.state" "plugins" "list_plugins" "Read plugin state."
        NoRequestBody
  , withResponseSchema pluginListResponseSchema $
      service "GET" ["plugins", "dependencies"] "plugins.dependencies" "plugins" "list_plugins" "Read plugin dependency declarations."
        NoRequestBody
  , withSchemas pluginSetEnabledRequestSchema pluginSetEnabledResponseSchema $
      service "PATCH" ["plugins", "enabled"] "plugins.setEnabled" "plugins" "set_plugin_enabled" "Enable or disable a plugin."
        RequiredJsonRequestBody
  , withSchemas pluginSetParamRequestSchema pluginSetParamResponseSchema $
      service "PATCH" ["plugins", "params"] "plugins.params.set" "plugins" "set_plugin_param" "Set a plugin parameter."
        RequiredJsonRequestBody

  , withResponseSchema dataPluginsListResponseSchema $
      service "GET" ["data", "plugins"] "data.plugins.list" "data" "data_list_plugins" "List plugins with data resources." NoRequestBody
  , withResponseSchema dataResourcesListResponseSchema $
      serviceWithQuery "GET" ["data", "resources"] "data.resources.list" "data" "data_list_resources" "List plugin data resources." NoRequestBody
        [requiredQuery "plugin" "Plugin name."]
  , withResponseSchema dataRecordsListResponseSchema $
      serviceWithQuery "GET" ["data", "records"] "data.records.list" "data" "data_list_records" "List records." NoRequestBody
        [ requiredQuery "plugin" "Plugin name."
        , requiredQuery "resource" "Resource name."
        , optionalQueryWithSchema "query" "Query mode: all, by_key, by_hex, or by_field." (queryEnumSchema ["all", "by_key", "by_hex", "by_field"])
        , optionalQuery "key" "Primary key for by_key queries."
        , optionalQueryWithSchema "chunk" "Chunk index for by_hex queries." queryIntegerSchema
        , optionalQueryWithSchema "tile" "Tile index for by_hex queries." queryIntegerSchema
        , optionalQuery "field" "Field name for by_field queries."
        , optionalQuery "value" "Field value for by_field queries."
        , optionalQueryWithSchema "page_size" "Maximum records to return." queryIntegerSchema
        , optionalQueryWithSchema "page_offset" "Record offset." queryIntegerSchema
        ]
  , withSchemas dataRecordGetRequestSchema dataRecordGetResponseSchema $
      service "POST" ["data", "records", "get"] "data.records.get" "data" "data_get_record" "Read one record." RequiredJsonRequestBody
  , withSchemas dataRecordCreateRequestSchema dataRecordCreateResponseSchema $
      service "POST" ["data", "records"] "data.records.create" "data" "data_create_record" "Create a record." RequiredJsonRequestBody
  , withSchemas dataRecordUpdateRequestSchema dataRecordUpdateResponseSchema $
      service "PUT" ["data", "records"] "data.records.update" "data" "data_update_record" "Update a record." RequiredJsonRequestBody
  , withSchemas dataRecordDeleteRequestSchema dataRecordDeleteResponseSchema $
      service "DELETE" ["data", "records"] "data.records.delete" "data" "data_delete_record" "Delete a record." RequiredJsonRequestBody
  , withResponseSchema dataStateResponseSchema $
      service "GET" ["data", "state"] "data.state" "data" "data_get_state" "Read data browser state." NoRequestBody

  , withResponseSchema simulationStateResponseSchema $
      service "GET" ["simulation"] "simulation.state" "simulation" "get_sim_state" "Read simulation state." NoRequestBody
  , withResponseSchema simulationDagResponseSchema $
      service "GET" ["simulation", "dag"] "simulation.dag" "simulation" "get_sim_dag" "Read simulation DAG." NoRequestBody
  , withSchemas simulationAutoTickRequestSchema simulationAutoTickResponseSchema $
      service "POST" ["simulation", "auto-tick"] "simulation.autoTick.set" "simulation" "set_sim_auto_tick" "Set auto-tick." RequiredJsonRequestBody
  , withSchemas simulationTickRequestSchema simulationTickResponseSchema $
      service "POST" ["simulation", "tick"] "simulation.tick" "simulation" "sim_tick" "Run simulation ticks." OptionalJsonRequestBody

  , withResponseSchema logGetResponseSchema $
      serviceWithQuery "GET" ["logs"] "logs.get" "logs" "get_logs" "Read logs." NoRequestBody
        [ optionalQueryWithSchema "level" "Minimum log level (debug, info, warn, or error)."
            (queryEnumSchema ["debug", "info", "warn", "error"])
        , optionalQueryWithSchema "limit" "Maximum entries to return." queryIntegerSchema
        , optionalQueryWithSchema "offset" "Entries to skip after filtering." queryIntegerSchema
        ]
  , withSchemas screenshotTakeRequestSchema screenshotTakeResponseSchema $
      service "POST" ["screenshots"] "screenshots.take" "screenshots" "take_screenshot" "Capture a screenshot." OptionalJsonRequestBody

  , service "POST" ["ui", "seed"] "ui.seed.set" "ui" "set_seed" "Set seed." RequiredJsonRequestBody
  , service "POST" ["ui", "view-mode"] "ui.viewMode.set" "ui" "set_view_mode" "Set view mode." RequiredJsonRequestBody
  , service "POST" ["ui", "view"] "ui.view.set" "ui" "set_view" "Set layered view." RequiredJsonRequestBody
  , service "POST" ["ui", "config-tab"] "ui.configTab.set" "ui" "set_config_tab" "Set config tab." RequiredJsonRequestBody
  , service "POST" ["ui", "select-hex"] "ui.hex.select" "ui" "select_hex" "Select a hex." OptionalJsonRequestBody
  , service "POST" ["ui", "overlay"] "ui.overlay.set" "ui" "set_overlay" "Set overlay." RequiredJsonRequestBody
  , serviceWithQuery "GET" ["ui", "overlay-fields"] "ui.overlayFields.list" "ui" "list_overlay_fields" "List overlay fields." NoRequestBody
      [optionalQuery "overlay" "Overlay name."]
  , service "POST" ["ui", "overlay", "cycle"] "ui.overlay.cycle" "ui" "cycle_overlay" "Cycle overlay." RequiredJsonRequestBody
  , service "POST" ["ui", "overlay-field", "cycle"] "ui.overlayField.cycle" "ui" "cycle_overlay_field" "Cycle overlay field." RequiredJsonRequestBody

  , service "PUT" ["camera"] "camera.set" "camera" "set_camera" "Set camera." RequiredJsonRequestBody
  , service "GET" ["camera"] "camera.get" "camera" "get_camera" "Read camera." NoRequestBody
  , service "POST" ["camera", "zoom-to-chunk"] "camera.zoomToChunk" "camera" "zoom_to_chunk" "Zoom to chunk." RequiredJsonRequestBody
  , service "PUT" ["ui", "camera"] "ui.camera.set" "ui" "set_camera" "Set camera." RequiredJsonRequestBody
  , service "GET" ["ui", "camera"] "ui.camera.get" "ui" "get_camera" "Read camera." NoRequestBody
  , service "POST" ["ui", "camera", "zoom-to-chunk"] "ui.camera.zoomToChunk" "ui" "zoom_to_chunk" "Zoom to chunk." RequiredJsonRequestBody
  , service "PUT" ["ui", "left-panel"] "ui.leftPanel.set" "ui" "set_left_panel" "Set left panel visibility." RequiredJsonRequestBody
  , service "PUT" ["ui", "left-tab"] "ui.leftTab.set" "ui" "set_left_tab" "Set left tab." RequiredJsonRequestBody
  , service "POST" ["ui", "config-panel", "toggle"] "ui.configPanel.toggle" "ui" "toggle_config_panel" "Toggle config panel." OptionalJsonRequestBody
  , service "PUT" ["ui", "log", "collapsed"] "ui.logCollapsed.set" "ui" "set_log_collapsed" "Set log collapsed state." RequiredJsonRequestBody
  , service "PUT" ["ui", "log", "level"] "ui.logLevel.set" "ui" "set_log_level" "Set log level." RequiredJsonRequestBody
  , service "GET" ["ui", "panels"] "ui.panels.get" "ui" "get_ui_panels" "Read panel state." NoRequestBody
  , service "POST" ["ui", "viewport", "scroll"] "ui.viewport.scroll" "ui" "viewport_scroll" "Scroll viewport." RequiredJsonRequestBody
  , service "POST" ["ui", "viewport", "click"] "ui.viewport.click" "ui" "viewport_click" "Click viewport." RequiredJsonRequestBody
  , service "POST" ["ui", "viewport", "drag"] "ui.viewport.drag" "ui" "viewport_drag" "Drag viewport." RequiredJsonRequestBody
  , service "POST" ["ui", "viewport", "hover"] "ui.viewport.hover" "ui" "viewport_hover" "Hover viewport." RequiredJsonRequestBody
  , service "POST" ["ui", "widgets", "click"] "ui.widgets.click" "ui" "click_widget" "Click widget." RequiredJsonRequestBody
  , service "GET" ["ui", "widgets"] "ui.widgets.list" "ui" "list_widgets" "List widgets." NoRequestBody
  , serviceWithQuery "GET" ["ui", "widget-state"] "ui.widgetState.get" "ui" "get_widget_state" "Read widget state." NoRequestBody
      [requiredQuery "widget_id" "Widget identifier."]
  , service "GET" ["ui", "dialog"] "ui.dialog.get" "ui" "get_dialog_state" "Read dialog state." NoRequestBody
  , service "PUT" ["ui", "dialog", "text"] "ui.dialogText.set" "ui" "set_dialog_text" "Set dialog text." RequiredJsonRequestBody
  , service "POST" ["ui", "dialog", "confirm"] "ui.dialog.confirm" "ui" "dialog_confirm" "Confirm dialog." NoRequestBody
  , service "POST" ["ui", "dialog", "cancel"] "ui.dialog.cancel" "ui" "dialog_cancel" "Cancel dialog." NoRequestBody
  , service "POST" ["ui", "key"] "ui.key.send" "ui" "send_key" "Send key." RequiredJsonRequestBody
  ]

commandHttpRouteSpecs :: [HttpRouteSpec]
commandHttpRouteSpecs =
  [ service "POST" ["commands", serviceOperationMethod spec]
      ("command." <> serviceOperationMethod spec)
      "commands"
      (serviceOperationMethod spec)
      ("Dispatch AppService command method " <> serviceOperationMethod spec <> ".")
      OptionalJsonRequestBody
  | spec <- appServiceOperationSpecs
  ]

special :: Text -> [Text] -> Text -> Text -> Text -> HttpRouteSpec
special method path operationId tag summary = HttpRouteSpec
  { hrsMethod = method
  , hrsPath = path
  , hrsOperationId = operationId
  , hrsSummary = summary
  , hrsTag = tag
  , hrsServiceMethod = Nothing
  , hrsRequestBody = NoRequestBody
  , hrsQueryParams = []
  , hrsRequestSchema = Nothing
  , hrsResponseSchema = Nothing
  }

specialWithQuery :: Text -> [Text] -> Text -> Text -> Text -> [QueryParamSpec] -> HttpRouteSpec
specialWithQuery method path operationId tag summary queryParams =
  (special method path operationId tag summary) { hrsQueryParams = queryParams }

service :: Text -> [Text] -> Text -> Text -> Text -> Text -> RouteBody -> HttpRouteSpec
service method path operationId tag serviceMethod summary body =
  serviceWithQuery method path operationId tag serviceMethod summary body []

serviceWithQuery :: Text -> [Text] -> Text -> Text -> Text -> Text -> RouteBody -> [QueryParamSpec] -> HttpRouteSpec
serviceWithQuery method path operationId tag serviceMethod summary body queryParams = HttpRouteSpec
  { hrsMethod = method
  , hrsPath = path
  , hrsOperationId = operationId
  , hrsSummary = summary
  , hrsTag = tag
  , hrsServiceMethod = Just serviceMethod
  , hrsRequestBody = body
  , hrsQueryParams = queryParams
  , hrsRequestSchema = Nothing
  , hrsResponseSchema = Nothing
  }

requiredQuery :: Text -> Text -> QueryParamSpec
requiredQuery name description = requiredQueryWithSchema name description queryStringSchema

requiredQueryWithSchema :: Text -> Text -> Value -> QueryParamSpec
requiredQueryWithSchema name description schema = QueryParamSpec name True description schema

optionalQuery :: Text -> Text -> QueryParamSpec
optionalQuery name description = QueryParamSpec name False description queryStringSchema

optionalQueryWithSchema :: Text -> Text -> Value -> QueryParamSpec
optionalQueryWithSchema name description schema = QueryParamSpec name False description schema

queryStringSchema :: Value
queryStringSchema = object ["type" .= ("string" :: Text)]

queryIntegerSchema :: Value
queryIntegerSchema = object ["type" .= ("integer" :: Text)]

queryBooleanSchema :: Value
queryBooleanSchema = object ["type" .= ("boolean" :: Text)]

queryEnumSchema :: [Text] -> Value
queryEnumSchema values = object
  [ "type" .= ("string" :: Text)
  , "enum" .= values
  ]

-- | Command-backed AppService with a deterministic headless screenshot handler.
-- The SDL render-loop screenshot path is still used by normal GUI runs; this
-- override makes headless HTTP smoke tests return a valid PNG instead of timing
-- out waiting for a renderer that was intentionally not started.
headlessHttpAppService :: AppService
headlessHttpAppService = commandAppService
  { appScreenshots = ScreenshotService
      { screenshotTake = headlessScreenshotHandler
      }
  }

headlessScreenshotHandler :: ServiceHandler ScreenshotTakeRequest ScreenshotTakeResponse
headlessScreenshotHandler = rawServiceHandler screenshotTakeOperation $ \_ request -> do
  let pngBytes = blankPng
      body = fromMaybe Null (serviceRequestBody request)
      mSavePath = case body of
        Object obj -> case KM.lookup "path" obj of
          Just (String path) -> Just (Text.unpack path)
          _ -> Nothing
        _ -> Nothing
  mapM_ (BS.writeFile `flip` pngBytes) mSavePath
  pure $ Right $ ServiceResponse $ object
    [ "image_base64" .= Text.decodeUtf8 (Base64.encode pngBytes)
    , "format" .= ("png" :: Text)
    , "source" .= ("headless" :: Text)
    ]

blankPng :: BS.ByteString
blankPng = BL.toStrict $ encodePng image
  where
    image = Image
      { imageWidth = 1
      , imageHeight = 1
      , imageData = VS.fromList [0, 0, 0, 255]
      } :: Image PixelRGBA8
