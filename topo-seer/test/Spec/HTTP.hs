{-# LANGUAGE OverloadedStrings #-}

module Spec.HTTP (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, finally, throwIO, try)
import Control.Monad (forM_)
import Data.Aeson (Value(..), object, (.=))
import Data.Foldable (toList)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Network.HTTP.Client
  ( Manager
  , RequestBody(..)
  , defaultManagerSettings
  , httpLbs
  , method
  , newManager
  , parseRequest
  , requestBody
  , responseStatus
  )
import qualified Network.HTTP.Types.Status as HTTP
import Test.Hspec

import Seer.Headless
  ( HeadlessApp
  , defaultHeadlessConfig
  , headlessServiceContext
  , withHeadlessApp
  )
import Seer.HTTP.Auth (HttpAuthConfig(..), isLoopbackHost, validateHttpAuthConfig)
import Seer.HTTP.OpenAPI
  ( HttpRouteSpec(..)
  , QueryParamSpec(..)
  , RouteBody(..)
  , openApiDocument
  , routePathText
  )
import Seer.HTTP.Server
  ( HttpRequest(..)
  , HttpResponse(..)
  , HttpServerConfig(..)
  , commandHttpRouteSpecs
  , defaultHttpServerConfig
  , forkHttpServer
  , handleHttpRequest
  , headlessHttpAppService
  , httpRouteSpecs
  , parseHttpBind
  )
import Seer.Service.AppService (appServiceOperationMethods)
import Seer.System (runApp)
import System.Environment (withArgs)
import Paths_topo_seer (getDataFileName)

spec :: Spec
spec = describe "Seer.HTTP.Server" $ do
  it "parses loopback bind strings" $
    parseHttpBind "127.0.0.1:7373" `shouldBe` Just ("127.0.0.1", 7373)

  it "requires bearer auth for non-loopback binds" $ do
    isLoopbackHost "127.0.0.1" `shouldBe` True
    isLoopbackHost "127.example" `shouldBe` False
    validateHttpAuthConfig (HttpAuthConfig "0.0.0.0" Nothing)
      `shouldBe` Left "non-loopback HTTP bindings require a bearer token"
    validateHttpAuthConfig (HttpAuthConfig "0.0.0.0" (Just "secret"))
      `shouldBe` Right ()

  it "serves health, OpenAPI, state, and screenshot routes in headless mode" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      health <- request app (mkRequest "GET" ["health"])
      hresStatusCode health `shouldBe` 200
      lookupText "status" (hresBody health) `shouldBe` Just "ok"

      openapi <- request app (mkRequest "GET" ["openapi.json"])
      hresStatusCode openapi `shouldBe` 200
      lookupText "openapi" (hresBody openapi) `shouldBe` Just "3.0.3"

      state <- request app (mkRequest "GET" ["state"])
      hresStatusCode state `shouldBe` 200
      objectHasKey "seed" (hresBody state) `shouldBe` True

      dag <- request app (mkRequest "GET" ["simulation", "dag"])
      hresStatusCode dag `shouldBe` 200
      objectHasKey "available" (hresBody dag) `shouldBe` True

      pluginStatus <- request app (mkRequest "GET" ["plugins", "status"])
      hresStatusCode pluginStatus `shouldBe` 200
      objectHasKey "plugins" (hresBody pluginStatus) `shouldBe` True

      screenshot <- request app (mkRequest "POST" ["screenshots"])
      hresStatusCode screenshot `shouldBe` 200
      lookupText "format" (hresBody screenshot) `shouldBe` Just "png"
      lookupText "source" (hresBody screenshot) `shouldBe` Just "headless"
      lookupText "image_base64" (hresBody screenshot) `shouldSatisfy` maybe False (not . Text.null)

  it "coerces signed numeric query parameters before service validation" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      rsp <- request app (mkRequest "GET" ["terrain", "hex"])
        { hreqQuery = [("q", Just "-1"), ("r", Just "0")] }
      lookupNestedText ["error", "code"] (hresBody rsp) `shouldNotBe` Just "validation_failed"

  it "returns validation errors as HTTP 400 JSON envelopes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      rsp <- request app (mkRequest "PATCH" ["plugins", "enabled"])
        { hreqBody = Just (object []) }
      hresStatusCode rsp `shouldBe` 400
      lookupNestedText ["error", "code"] (hresBody rsp) `shouldBe` Just "validation_failed"

  it "enforces optional bearer tokens on protected routes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cfg = defaultHttpServerConfig { hscBearerToken = Just "secret" }
          ctx = headlessServiceContext app
      denied <- handleHttpRequest cfg headlessHttpAppService ctx (mkRequest "GET" ["state"])
      hresStatusCode denied `shouldBe` 401

      allowed <- handleHttpRequest cfg headlessHttpAppService ctx
        (mkRequest "GET" ["state"]) { hreqHeaders = [("authorization", "Bearer secret")] }
      hresStatusCode allowed `shouldBe` 200

  it "lists every implemented route in OpenAPI" $ do
    let doc = openApiDocument httpRouteSpecs
    forM_ httpRouteSpecs $ \route -> do
      let path = routePathText route
          routeMethod = Text.toLower (hrsMethod route)
      pathMethods doc path `shouldSatisfy` maybe False (routeMethod `elem`)

  it "keeps OpenAPI paths and route metadata in lockstep" $ do
    let doc = openApiDocument httpRouteSpecs
    openApiSignatureProblems doc `shouldBe` []
    sort (openApiSignatureLines doc) `shouldBe` sort (map routeSignature httpRouteSpecs)

  it "publishes route metadata from the route table into OpenAPI" $ do
    let doc = openApiDocument httpRouteSpecs
    forM_ httpRouteSpecs $ \route -> do
      let path = routePathText route
          routeMethod = Text.toLower (hrsMethod route)
      pathOperation doc path routeMethod `shouldSatisfy` maybe False (const True)
      operationTags doc path routeMethod `shouldBe` Just [hrsTag route]
      operationQueryParameterInfo doc path routeMethod `shouldBe` Just (routeQueryParameterInfo route)
      operationRequestBodyRequired doc path routeMethod `shouldBe` Just (routeRequestBodyRequired route)
      operationHasSecurity doc path routeMethod "bearerAuth" `shouldBe` (hrsOperationId route /= "meta.health")

  it "has a handler for every route spec" $ do
    let missingHandlers =
          [ routeSignature route
          | route <- httpRouteSpecs
          , not (routeHasHandler route)
          ]
    missingHandlers `shouldBe` []

  it "matches the committed served OpenAPI route golden" $ do
    golden <- readOpenApiRouteGolden
    let doc = openApiDocument httpRouteSpecs
    openApiSignatureProblems doc `shouldBe` []
    sort (openApiSignatureLines doc) `shouldBe` sort golden

  it "publishes query and auth metadata in OpenAPI" $ do
    let doc = openApiDocument httpRouteSpecs
    queryParameterInfo doc "/terrain/hex" "get"
      `shouldBe` Just [("q", True), ("r", True)]
    queryParameterInfo doc "/config/enums" "get"
      `shouldBe` Just [("type", True)]
    operationHasSecurity doc "/state" "get" "bearerAuth" `shouldBe` True
    operationHasSecurity doc "/health" "get" "bearerAuth" `shouldBe` False

  it "rejects unauthorized WAI requests before parsing protected bodies" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cfg = defaultHttpServerConfig { hscBindPort = 7374, hscBearerToken = Just "secret" }
      tid <- forkHttpServer cfg headlessHttpAppService (headlessServiceContext app)
      manager <- newManager defaultManagerSettings
      eventually_ (assertUnauthorizedInvalidJson manager)
        `finally` (do
          killThread tid
          threadDelay 100000)

  it "publishes a command HTTP route for every AppService operation" $
    sort (map (last . hrsPath) commandHttpRouteSpecs)
      `shouldBe` sort appServiceOperationMethods

  it "starts topo-seer headless HTTP endpoints through the CLI" $ do
    tid <- forkIO $
      withArgs ["--headless", "--http", "127.0.0.1:7373", "--test-mode"] runApp
    manager <- newManager defaultManagerSettings
    let endpoints = ["/health", "/version", "/openapi.json", "/state"]
    eventually_ (forM_ endpoints (assertEndpoint manager))
      `finally` (do
        killThread tid
        threadDelay 100000)

request :: HeadlessApp -> HttpRequest -> IO HttpResponse
request app req = handleHttpRequest defaultHttpServerConfig headlessHttpAppService (headlessServiceContext app) req

mkRequest :: Text -> [Text] -> HttpRequest
mkRequest method path = HttpRequest
  { hreqMethod = method
  , hreqPath = path
  , hreqQuery = []
  , hreqHeaders = []
  , hreqBody = Nothing
  }

lookupText :: Text -> Value -> Maybe Text
lookupText key value = case lookupValue key value of
  Just (String text) -> Just text
  _ -> Nothing

lookupNestedText :: [Text] -> Value -> Maybe Text
lookupNestedText [] _ = Nothing
lookupNestedText [key] value = lookupText key value
lookupNestedText (key:rest) value = lookupValue key value >>= lookupNestedText rest

lookupValue :: Text -> Value -> Maybe Value
lookupValue key (Object obj) = KM.lookup (Key.fromText key) obj
lookupValue _ _ = Nothing

objectHasKey :: Text -> Value -> Bool
objectHasKey key (Object obj) = KM.member (Key.fromText key) obj
objectHasKey _ _ = False

pathMethods :: Value -> Text -> Maybe [Text]
pathMethods doc path = do
  paths <- lookupValue "paths" doc
  Object pathObj <- pure paths
  Object methodObj <- KM.lookup (Key.fromText path) pathObj
  pure (map Key.toText (KM.keys methodObj))

pathOperation :: Value -> Text -> Text -> Maybe Value
pathOperation doc path routeMethod = do
  paths <- lookupValue "paths" doc
  Object pathObj <- pure paths
  Object methodObj <- KM.lookup (Key.fromText path) pathObj
  KM.lookup (Key.fromText routeMethod) methodObj

queryParameterInfo :: Value -> Text -> Text -> Maybe [(Text, Bool)]
queryParameterInfo doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  Array params <- KM.lookup "parameters" operation
  traverse queryInfo (toList params)
  where
    queryInfo (Object param) = do
      String name <- KM.lookup "name" param
      Bool required <- KM.lookup "required" param
      pure (name, required)
    queryInfo _ = Nothing

operationHasSecurity :: Value -> Text -> Text -> Text -> Bool
operationHasSecurity doc path routeMethod scheme =
  case pathOperation doc path routeMethod of
    Just (Object operation) -> case KM.lookup "security" operation of
      Just (Array entries) -> any (entryHasScheme scheme) (toList entries)
      _ -> False
    _ -> False
  where
    entryHasScheme expected (Object entry) = KM.member (Key.fromText expected) entry
    entryHasScheme _ _ = False

operationTags :: Value -> Text -> Text -> Maybe [Text]
operationTags doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  Array tags <- KM.lookup "tags" operation
  traverse tagText (toList tags)
  where
    tagText (String tag) = Just tag
    tagText _ = Nothing

operationQueryParameterInfo :: Value -> Text -> Text -> Maybe [(Text, Bool)]
operationQueryParameterInfo doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  case KM.lookup "parameters" operation of
    Nothing -> Just []
    Just (Array params) -> traverse queryInfo (toList params)
    Just _ -> Nothing
  where
    queryInfo (Object param) = do
      String name <- KM.lookup "name" param
      Bool required <- KM.lookup "required" param
      pure (name, required)
    queryInfo _ = Nothing

operationRequestBodyRequired :: Value -> Text -> Text -> Maybe (Maybe Bool)
operationRequestBodyRequired doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  case KM.lookup "requestBody" operation of
    Nothing -> Just Nothing
    Just (Object body) -> do
      Bool required <- KM.lookup "required" body
      pure (Just required)
    Just _ -> Nothing

routeQueryParameterInfo :: HttpRouteSpec -> [(Text, Bool)]
routeQueryParameterInfo route =
  [ (qpsName param, qpsRequired param)
  | param <- hrsQueryParams route
  ]

routeRequestBodyRequired :: HttpRouteSpec -> Maybe Bool
routeRequestBodyRequired route = case hrsRequestBody route of
  NoRequestBody -> Nothing
  OptionalJsonRequestBody -> Just False
  RequiredJsonRequestBody -> Just True

routeSignature :: HttpRouteSpec -> Text
routeSignature route =
  Text.unwords [hrsMethod route, routePathText route, hrsOperationId route]

openApiSignatureLines :: Value -> [Text]
openApiSignatureLines doc =
  case lookupValue "paths" doc of
    Just (Object paths) ->
      [ Text.unwords [Text.toUpper (Key.toText methodKey), Key.toText pathKey, operationId]
      | (pathKey, Object methods) <- KM.toList paths
      , (methodKey, Object operation) <- KM.toList methods
      , Just (String operationId) <- [KM.lookup "operationId" operation]
      ]
    _ -> []

openApiSignatureProblems :: Value -> [Text]
openApiSignatureProblems doc =
  case lookupValue "paths" doc of
    Just (Object paths) -> concat
      [ methodProblems pathKey methodKey operation
      | (pathKey, Object methods) <- KM.toList paths
      , (methodKey, operation) <- KM.toList methods
      ]
    Just _ -> ["OpenAPI paths is not an object"]
    Nothing -> ["OpenAPI paths is missing"]
  where
    methodProblems pathKey methodKey (Object operation) =
      case KM.lookup "operationId" operation of
        Just (String _) -> []
        Just _ -> [signaturePrefix pathKey methodKey <> " has non-string operationId"]
        Nothing -> [signaturePrefix pathKey methodKey <> " is missing operationId"]
    methodProblems pathKey methodKey _ =
      [signaturePrefix pathKey methodKey <> " operation is not an object"]

    signaturePrefix pathKey methodKey =
      Text.unwords [Text.toUpper (Key.toText methodKey), Key.toText pathKey]

routeHasHandler :: HttpRouteSpec -> Bool
routeHasHandler route = case hrsServiceMethod route of
  Just methodName -> methodName `elem` appServiceOperationMethods
  Nothing -> hrsOperationId route `elem` specialOperationIds

specialOperationIds :: [Text]
specialOperationIds =
  [ "meta.health"
  , "meta.version"
  , "meta.openapi"
  , "events.list"
  ]

readOpenApiRouteGolden :: IO [Text]
readOpenApiRouteGolden = do
  path <- getDataFileName "test/golden/openapi-routes.txt"
  filter (not . Text.null) . map Text.strip . Text.lines
    <$> TextIO.readFile path

assertEndpoint :: Manager -> String -> IO ()
assertEndpoint manager path = do
  req <- parseRequest ("http://127.0.0.1:7373" <> path)
  rsp <- httpLbs req manager
  HTTP.statusCode (responseStatus rsp) `shouldBe` 200

assertUnauthorizedInvalidJson :: Manager -> IO ()
assertUnauthorizedInvalidJson manager = do
  req0 <- parseRequest "http://127.0.0.1:7374/screenshots"
  let req = req0
        { method = "POST"
        , requestBody = RequestBodyLBS "{not-json"
        }
  rsp <- httpLbs req manager
  HTTP.statusCode (responseStatus rsp) `shouldBe` 401

eventually_ :: IO () -> IO ()
eventually_ action = go (30 :: Int)
  where
    go attempts = do
      result <- try action :: IO (Either SomeException ())
      case result of
        Right () -> pure ()
        Left err
          | attempts <= 0 -> throwIO err
          | otherwise -> do
              threadDelay 100000
              go (attempts - 1)
