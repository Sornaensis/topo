{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Topo.Metadata
  ( Metadata(..)
  , SomeMetadata(..)
  , MetadataMigration(..)
  , MetadataDecodeError(..)
  , MetadataCodec(..)
  , metadataCodec
  , JsonValue(..)
  , encodeJsonObject
  , decodeJsonObject
  , encodeJsonString
  , decodeJsonString
  , MetadataStore(..)
  , emptyMetadataStore
  , putHexMeta
  , putHexMetaWithVersion
  , putRegionMeta
  , putRegionMetaWithVersion
  , getHexMeta
  , getHexMetaVersion
  , getRegionMeta
  , getRegionMetaVersion
  , migrateMetadataStore
  ) where

import Data.Char (isDigit, isSpace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, cast)
import Data.Word (Word32)
import Topo.Types (HexCoord, RegionId)

class Typeable a => Metadata a where
  metadataKey :: proxy a -> Text
  metadataVersion :: proxy a -> Word32
  metadataEncode :: a -> Text
  metadataDecode :: Word32 -> Text -> Either MetadataDecodeError a

-- | Error reported when decoding a JSON metadata payload.
data MetadataDecodeError = MetadataDecodeError
  { mdeMessage :: !Text
  } deriving (Eq, Show)

-- | Decoder for a metadata payload keyed by its metadata key.
data MetadataCodec = MetadataCodec
  { mcKey :: !Text
  , mcDecode :: Word32 -> Text -> Either MetadataDecodeError SomeMetadata
  }

-- | Build a decoder for a metadata type based on its instance.
metadataCodec :: forall a proxy. Metadata a => proxy a -> MetadataCodec
metadataCodec _ = MetadataCodec
  { mcKey = metadataKey (Proxy :: Proxy a)
  , mcDecode = \version payload ->
      let decode :: Word32 -> Text -> Either MetadataDecodeError a
          decode = metadataDecode
      in do
        value <- decode version payload
        pure (SomeMetadata version value)
  }

data SomeMetadata = forall a. Metadata a => SomeMetadata !Word32 a

data MetadataMigration = forall a. Metadata a => MetadataMigration
  { mmKey :: !Text
  , mmFrom :: !Word32
  , mmTo :: !Word32
  , mmMigrate :: a -> a
  }

data MetadataStore = MetadataStore
  { msHex    :: !(Map HexCoord (Map Text SomeMetadata))
  , msRegion :: !(Map RegionId (Map Text SomeMetadata))
  }

-- | Minimal JSON value representation for metadata payloads.
data JsonValue
  = JsonString !Text
  | JsonNumber !Double
  deriving (Eq, Show)

-- | Encode a flat JSON object with string or numeric values.
encodeJsonObject :: [(Text, JsonValue)] -> Text
encodeJsonObject fields =
  let encoded = map encodePair fields
  in Text.concat (Text.pack "{" : intersperseText (Text.pack ",") encoded <> [Text.pack "}"])

encodePair :: (Text, JsonValue) -> Text
encodePair (key, value) =
  encodeJsonString key <> Text.pack ":" <> encodeJsonValue value

encodeJsonValue :: JsonValue -> Text
encodeJsonValue value =
  case value of
    JsonString txt -> encodeJsonString txt
    JsonNumber num -> Text.pack (show num)

-- | Decode a flat JSON object with string or numeric values.
decodeJsonObject :: Text -> Either MetadataDecodeError (Map Text JsonValue)
decodeJsonObject input =
  case parseJsonObject (Text.unpack input) of
    Left err -> Left (MetadataDecodeError err)
    Right (pairs, rest) ->
      let trailing = dropWhile isSpace rest
      in if null trailing
          then pure (Map.fromList pairs)
          else Left (MetadataDecodeError (Text.pack "json: trailing input"))

-- | Encode a JSON string with minimal escaping.
encodeJsonString :: Text -> Text
encodeJsonString txt = Text.singleton '"' <> jsonEscape txt <> Text.singleton '"'

-- | Decode a JSON string value.
decodeJsonString :: Text -> Either MetadataDecodeError Text
decodeJsonString input =
  case parseJsonString (Text.unpack input) of
    Left err -> Left (MetadataDecodeError err)
    Right (val, rest) ->
      let trailing = dropWhile isSpace rest
      in if null trailing
          then pure val
          else Left (MetadataDecodeError (Text.pack "json: trailing input"))

emptyMetadataStore :: MetadataStore
emptyMetadataStore = MetadataStore Map.empty Map.empty

putHexMeta :: forall a. Metadata a => HexCoord -> a -> MetadataStore -> MetadataStore
putHexMeta coord val = putHexMetaWithVersion (metadataVersion (Proxy :: Proxy a)) coord val

putHexMetaWithVersion :: forall a. Metadata a => Word32 -> HexCoord -> a -> MetadataStore -> MetadataStore
putHexMetaWithVersion version coord val (MetadataStore hex region) =
  let key = metadataKey (Proxy :: Proxy a)
      updated = Map.insert key (SomeMetadata version val) (Map.findWithDefault Map.empty coord hex)
  in MetadataStore (Map.insert coord updated hex) region

putRegionMeta :: forall a. Metadata a => RegionId -> a -> MetadataStore -> MetadataStore
putRegionMeta rid val = putRegionMetaWithVersion (metadataVersion (Proxy :: Proxy a)) rid val

putRegionMetaWithVersion :: forall a. Metadata a => Word32 -> RegionId -> a -> MetadataStore -> MetadataStore
putRegionMetaWithVersion version rid val (MetadataStore hex region) =
  let key = metadataKey (Proxy :: Proxy a)
      updated = Map.insert key (SomeMetadata version val) (Map.findWithDefault Map.empty rid region)
  in MetadataStore hex (Map.insert rid updated region)

getHexMeta :: forall a. Metadata a => HexCoord -> MetadataStore -> Maybe a
getHexMeta coord (MetadataStore hex _) = do
  entries <- Map.lookup coord hex
  SomeMetadata _ val <- Map.lookup (metadataKey (Proxy :: Proxy a)) entries
  cast val

getHexMetaVersion :: forall a proxy. Metadata a => proxy a -> HexCoord -> MetadataStore -> Maybe Word32
getHexMetaVersion _ coord (MetadataStore hex _) = do
  entries <- Map.lookup coord hex
  SomeMetadata version _ <- Map.lookup (metadataKey (Proxy :: Proxy a)) entries
  pure version

getRegionMeta :: forall a. Metadata a => RegionId -> MetadataStore -> Maybe a
getRegionMeta rid (MetadataStore _ region) = do
  entries <- Map.lookup rid region
  SomeMetadata _ val <- Map.lookup (metadataKey (Proxy :: Proxy a)) entries
  cast val

getRegionMetaVersion :: forall a proxy. Metadata a => proxy a -> RegionId -> MetadataStore -> Maybe Word32
getRegionMetaVersion _ rid (MetadataStore _ region) = do
  entries <- Map.lookup rid region
  SomeMetadata version _ <- Map.lookup (metadataKey (Proxy :: Proxy a)) entries
  pure version

migrateMetadataStore :: [MetadataMigration] -> MetadataStore -> MetadataStore
migrateMetadataStore migrations (MetadataStore hex region) =
  MetadataStore (Map.map (Map.mapWithKey (applyMigrations migrations)) hex)
    (Map.map (Map.mapWithKey (applyMigrations migrations)) region)

applyMigrations :: [MetadataMigration] -> Text -> SomeMetadata -> SomeMetadata
applyMigrations migrations key entry =
  case findMigration key entry migrations of
    Nothing -> entry
    Just migrated -> applyMigrations migrations key migrated

findMigration :: Text -> SomeMetadata -> [MetadataMigration] -> Maybe SomeMetadata
findMigration _ _ [] = Nothing
findMigration key entry@(SomeMetadata version val) (m:ms) =
  case m of
    MetadataMigration mKey mFrom mTo migrate
      | mKey /= key -> findMigration key entry ms
      | mFrom /= version -> findMigration key entry ms
      | otherwise ->
          case cast val of
            Nothing -> findMigration key entry ms
            Just typed -> Just (SomeMetadata mTo (migrate typed))

jsonEscape :: Text -> Text
jsonEscape = Text.concatMap escapeChar
  where
    escapeChar '\"' = Text.pack "\\\""
    escapeChar '\\' = Text.pack "\\\\"
    escapeChar '\n' = Text.pack "\\n"
    escapeChar '\r' = Text.pack "\\r"
    escapeChar '\t' = Text.pack "\\t"
    escapeChar c = Text.singleton c

parseJsonObject :: [Char] -> Either Text ([(Text, JsonValue)], [Char])
parseJsonObject input0 = do
  let input = skipSpaces input0
  rest0 <- consumeChar '{' input (Text.pack "json: expected '{'")
  let rest1 = skipSpaces rest0
  case rest1 of
    ('}':xs) -> pure ([], xs)
    _ -> parsePairs rest1
  where
    parsePairs xs0 = do
      (key, xs1) <- parseJsonString xs0
      let xs2 = skipSpaces xs1
      xs3 <- consumeChar ':' xs2 (Text.pack "json: expected ':'")
      (val, xs4) <- parseJsonValue xs3
      let xs5 = skipSpaces xs4
      case xs5 of
        (',':xs6) -> do
          (restPairs, xs7) <- parsePairs xs6
          pure ((key, val) : restPairs, xs7)
        ('}':xs6) -> pure ([(key, val)], xs6)
        _ -> Left (Text.pack "json: expected ',' or '}'")

parseJsonValue :: [Char] -> Either Text (JsonValue, [Char])
parseJsonValue input =
  let trimmed = skipSpaces input
  in case trimmed of
      ('\"':_) -> do
        (str, rest) <- parseJsonString trimmed
        pure (JsonString str, rest)
      _ -> do
        (num, rest) <- parseJsonNumber trimmed
        pure (JsonNumber num, rest)

parseJsonString :: [Char] -> Either Text (Text, [Char])
parseJsonString input0 = do
  rest0 <- consumeChar '\"' input0 (Text.pack "json: expected string")
  (chars, rest) <- go [] rest0
  pure (Text.pack (reverse chars), rest)
  where
    go acc [] = Left (Text.pack "json: unterminated string")
    go acc ('\"':xs) = pure (acc, xs)
    go acc ('\\':xs) =
      case xs of
        ('\"':ys) -> go ('\"':acc) ys
        ('\\':ys) -> go ('\\':acc) ys
        ('n':ys) -> go ('\n':acc) ys
        ('r':ys) -> go ('\r':acc) ys
        ('t':ys) -> go ('\t':acc) ys
        _ -> Left (Text.pack "json: invalid escape")
    go acc (x:xs) = go (x:acc) xs

parseJsonNumber :: [Char] -> Either Text (Double, [Char])
parseJsonNumber input0 =
  let (numChars, rest) = spanNumber (skipSpaces input0)
  in if null numChars
      then Left (Text.pack "json: expected number")
      else case reads numChars of
        [(val, "")] -> pure (val, rest)
        _ -> Left (Text.pack "json: invalid number")

spanNumber :: [Char] -> ([Char], [Char])
spanNumber [] = ([], [])
spanNumber (x:xs)
  | x == '-' = let (a, b) = spanNumber xs in (x:a, b)
  | isDigit x = let (a, b) = spanNumber xs in (x:a, b)
  | x == '.' = let (a, b) = spanNumber xs in (x:a, b)
  | otherwise = ([], x:xs)

consumeChar :: Char -> [Char] -> Text -> Either Text [Char]
consumeChar _ [] err = Left err
consumeChar expected (x:xs) err
  | x == expected = Right xs
  | otherwise = Left err

skipSpaces :: [Char] -> [Char]
skipSpaces = dropWhile isSpace

intersperseText :: Text -> [Text] -> [Text]
intersperseText _ [] = []
intersperseText _ [x] = [x]
intersperseText sep (x:xs) = x : sep : intersperseText sep xs
