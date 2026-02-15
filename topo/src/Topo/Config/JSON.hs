-- | Shared JSON serialization helpers for topo config types.
--
-- Provides a generic prefix-stripping 'Options' constructor that produces
-- clean, camelCase JSON keys from Haskell record field names.  Every
-- config type in the topo library uses the same convention: a short
-- lowercase prefix (e.g. @gc@, @tc@, @ec@) followed by a CamelCase
-- field name.  The JSON key is the suffix with its first letter
-- lower-cased:
--
-- > gcFrequency  →  "frequency"
-- > tcPlateSize  →  "plateSize"
--
-- All 'FromJSON' instances in the library merge incoming JSON objects
-- with the serialized default config, so every field is optional.
-- Adding a new field never breaks existing config files.
module Topo.Config.JSON
  ( -- * JSON options
    configOptions
  , multiPrefixOptions
    -- * Forward-compatible parsing
  , mergeDefaults
    -- * Re-exports for convenience
  , Options
  , genericToJSON
  , genericParseJSON
  , ToJSON(..)
  , FromJSON(..)
  , Value(..)
  , (.:?)
  , (.!=)
  , withObject
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Options
  , Value(..)
  , defaultOptions
  , fieldLabelModifier
  , genericToJSON
  , genericParseJSON
  , withObject
  , (.:?)
  , (.!=)
  )
import Data.Char (toLower)
import Data.List (isPrefixOf)
import qualified Data.Aeson.KeyMap as KM

-- | Build aeson 'Options' that strip a given prefix and lower-case
-- the first character of the remainder.
--
-- >>> fieldLabelModifier (configOptions "gc") "gcFrequency"
-- "frequency"
--
-- >>> fieldLabelModifier (configOptions "tc") "tcPlateSize"
-- "plateSize"
configOptions :: String -> Options
configOptions prefix = defaultOptions
  { fieldLabelModifier = stripAndLower prefix }
  where
    stripAndLower pfx field =
      case drop (length pfx) field of
        []     -> field  -- safety: return unchanged if prefix = whole name
        (c:cs) -> toLower c : cs

-- | Build aeson 'Options' that try multiple prefixes in order.
--
-- The first matching prefix is stripped and the remainder is
-- lower-camelCased.  Falls back to the raw field name when no prefix
-- matches.
--
-- Useful for record types with mixed-prefix fields (e.g.
-- 'MoistureConfig' which has both @moist@ and @cc@ prefixed fields).
--
-- >>> fieldLabelModifier (multiPrefixOptions ["moist","cc"]) "moistAdvect"
-- "advect"
--
-- >>> fieldLabelModifier (multiPrefixOptions ["moist","cc"]) "ccTempToC_Scale"
-- "tempToC_Scale"
multiPrefixOptions :: [String] -> Options
multiPrefixOptions prefixes = defaultOptions
  { fieldLabelModifier = stripFirst prefixes }
  where
    stripFirst [] field = field
    stripFirst (p:ps) field
      | p `isPrefixOf` field =
          case drop (length p) field of
            []     -> field
            (c:cs) -> toLower c : cs
      | otherwise = stripFirst ps field

-- | Merge an incoming JSON 'Value' with a serialized default.
--
-- When the incoming value is an 'Object', missing keys are filled in
-- from @defaultVal@.  When incoming is 'Null', the full @defaultVal@
-- is returned.  Otherwise the incoming value is returned unchanged.
--
-- Typical usage in a hand-written 'FromJSON' instance:
--
-- @
-- instance FromJSON MyConfig where
--   parseJSON v = genericParseJSON (configOptions "mc")
--                   (mergeDefaults (toJSON defaultMyConfig) v)
-- @
mergeDefaults
  :: Value  -- ^ Serialized default config (should be an Object)
  -> Value  -- ^ Incoming JSON value
  -> Value  -- ^ Merged value
mergeDefaults defaultVal incoming =
  case incoming of
    Null -> defaultVal
    Object inObj ->
      case defaultVal of
        Object defObj -> Object (KM.union inObj defObj)
        _             -> incoming
    other -> other
