{-# LANGUAGE OverloadedStrings #-}

module Seer.System.Runtime
  ( RuntimeOptions(..)
  , defaultRuntimeOptions
  , parseRuntimeOptions
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Seer.HTTP.Server
  ( HttpServerConfig(..)
  , defaultHttpServerConfig
  , parseHttpBind
  )

-- | Minimal CLI runtime options needed for the HTTP milestone.
data RuntimeOptions = RuntimeOptions
  { roHeadless :: !Bool
  , roHttp :: !(Maybe HttpServerConfig)
  , roTestMode :: !Bool
  } deriving (Eq, Show)

defaultRuntimeOptions :: RuntimeOptions
defaultRuntimeOptions = RuntimeOptions
  { roHeadless = False
  , roHttp = Nothing
  , roTestMode = False
  }

parseRuntimeOptions :: [String] -> Either String RuntimeOptions
parseRuntimeOptions = go defaultRuntimeOptions
  where
    go opts [] = Right opts
    go opts ("--headless":rest) = go opts { roHeadless = True } rest
    go opts ("--test-mode":rest) = go opts { roTestMode = True } rest
    go opts ("--http":binding:rest) = parseHttpOption opts binding rest
    go opts (arg:rest)
      | Just binding <- stripPrefixArg "--http=" arg = parseHttpOption opts binding rest
      | Just token <- stripPrefixArg "--http-token=" arg = go (setToken opts token) rest
      | arg == "--http-token" = case rest of
          token:rest' -> go (setToken opts token) rest'
          [] -> Left "--http-token requires a token"
      | otherwise = Left ("unknown topo-seer option: " <> arg)

    parseHttpOption opts binding rest =
      case parseHttpBind binding of
        Nothing -> Left ("invalid --http binding, expected HOST:PORT: " <> binding)
        Just (host, port) -> go opts
          { roHttp = Just (httpConfigWithExistingToken opts host port)
          } rest

    httpConfigWithExistingToken opts host port =
      defaultHttpServerConfig
        { hscBindHost = host
        , hscBindPort = port
        , hscBearerToken = roHttp opts >>= hscBearerToken
        }

    setToken opts token = opts
      { roHttp = Just (fromMaybe defaultHttpServerConfig (roHttp opts))
          { hscBearerToken = Just (Text.pack token)
          }
      }

stripPrefixArg :: String -> String -> Maybe String
stripPrefixArg prefix value =
  case splitAt (length prefix) value of
    (candidate, rest) | candidate == prefix -> Just rest
    _ -> Nothing
