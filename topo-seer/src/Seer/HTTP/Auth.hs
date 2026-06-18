{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Authentication and bind-address policy for topo-seer HTTP.
module Seer.HTTP.Auth
  ( HttpAuthConfig(..)
  , isLoopbackHost
  , validateHttpAuthConfig
  , isAuthorized
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

-- | Runtime auth policy. Loopback binds may run without a bearer token, but
-- non-loopback binds must provide one before the server starts.
data HttpAuthConfig = HttpAuthConfig
  { hacBindHost :: !String
  , hacBearerToken :: !(Maybe Text)
  } deriving (Eq, Show)

isLoopbackHost :: String -> Bool
isLoopbackHost host =
  host == "localhost"
    || host == "::1"
    || host == "[::1]"
    || host == "0:0:0:0:0:0:0:1"
    || isIpv4Loopback host

isIpv4Loopback :: String -> Bool
isIpv4Loopback host = case splitOnDot host of
  ["127", b, c, d] -> all validOctet [b, c, d]
  _ -> False

validOctet :: String -> Bool
validOctet octet = case readMaybe octet :: Maybe Int of
  Just value -> show value == octet && value >= 0 && value <= 255
  Nothing -> False

splitOnDot :: String -> [String]
splitOnDot = go ""
  where
    go current [] = [reverse current]
    go current ('.':rest) = reverse current : go "" rest
    go current (char:rest) = go (char:current) rest

validateHttpAuthConfig :: HttpAuthConfig -> Either Text ()
validateHttpAuthConfig cfg
  | isLoopbackHost (hacBindHost cfg) = Right ()
  | Just token <- hacBearerToken cfg
  , not (Text.null token) = Right ()
  | otherwise = Left "non-loopback HTTP bindings require a bearer token"

isAuthorized :: Maybe Text -> [(Text, Text)] -> Bool
isAuthorized Nothing _ = True
isAuthorized (Just token) headers =
  lookupHeader "authorization" headers == Just ("Bearer " <> token)

lookupHeader :: Text -> [(Text, Text)] -> Maybe Text
lookupHeader name headers = lookup (Text.toLower name)
  [ (Text.toLower key, value) | (key, value) <- headers ]
