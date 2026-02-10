{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Runtime configuration for the topo-seer application.
--
-- 'TopoSeerConfig' consolidates all tunable runtime parameters that were
-- previously resolved individually from environment variables.  The config
-- is loaded from @~\/.topo\/config.json@ when present, falling back to
-- 'defaultConfig' for any missing fields.
--
-- The type derives 'FromJSON'\/'ToJSON' via generic Aeson instances so it
-- can be round-tripped to disk trivially.
module Seer.Config.Runtime
  ( TopoSeerConfig(..)
  , defaultConfig
  , loadConfig
  ) where

import Control.Monad (unless)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , (.:?)
  , (.!=)
  , eitherDecodeStrict'
  , genericToJSON
  , withObject
  , defaultOptions
  , Options(..)
  )
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getHomeDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Data.ByteString as BS

-- | All tunable runtime knobs for topo-seer.
--
-- Every field has a sensible default provided by 'defaultConfig'.
-- Missing JSON keys fall back to the default automatically via the
-- 'FromJSON' instance.
data TopoSeerConfig = TopoSeerConfig
  { -- | Target frame delay in milliseconds.  Clamped to a minimum of 1.
    cfgFrameDelayMs          :: Int
    -- | Number of atlas texture uploads allowed per frame.  Clamped to >= 1.
  , cfgAtlasUploadsPerFrame  :: Int
    -- | Maximum number of atlas cache entries.  Clamped to >= 1.
  , cfgAtlasCacheEntries     :: Int
    -- | Terrain cache poll interval in milliseconds.  Clamped to >= 5.
  , cfgTerrainCachePollMs    :: Int
    -- | Atlas drain poll interval in milliseconds.  Clamped to >= 5.
  , cfgAtlasDrainPollMs      :: Int
    -- | Atlas schedule poll interval in milliseconds.  Clamped to >= 5.
  , cfgAtlasSchedulePollMs   :: Int
    -- | Chunk texture poll interval in milliseconds.  Clamped to >= 5.
  , cfgChunkTexturePollMs    :: Int
    -- | Snapshot poll interval in milliseconds.  Clamped to >= 5.
  , cfgSnapshotPollMs        :: Int
    -- | Timing-log threshold in milliseconds.  Clamped to >= 1.
  , cfgTimingLogThresholdMs  :: Int
    -- | When 'True', emit verbose render-step logs.
  , cfgRenderTraceEnabled    :: Bool
  } deriving (Eq, Show, Generic)

-- | JSON field names drop the @cfg@ prefix and lower-case the first letter,
-- e.g. @cfgFrameDelayMs@ becomes @\"frameDelayMs\"@.
jsonOptions :: Options
jsonOptions = defaultOptions
  { fieldLabelModifier = dropPrefix }
  where
    dropPrefix ('c':'f':'g':c:rest) = toLowerFirst (c : rest)
    dropPrefix other                = other
    toLowerFirst []     = []
    toLowerFirst (c:cs) = toLower c : cs
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise             = c

instance ToJSON TopoSeerConfig where
  toJSON = genericToJSON jsonOptions

instance FromJSON TopoSeerConfig where
  parseJSON = withObject "TopoSeerConfig" $ \o -> do
    let d = defaultConfig
    TopoSeerConfig
      <$> o .:? "frameDelayMs"         .!= cfgFrameDelayMs d
      <*> o .:? "atlasUploadsPerFrame"  .!= cfgAtlasUploadsPerFrame d
      <*> o .:? "atlasCacheEntries"     .!= cfgAtlasCacheEntries d
      <*> o .:? "terrainCachePollMs"    .!= cfgTerrainCachePollMs d
      <*> o .:? "atlasDrainPollMs"      .!= cfgAtlasDrainPollMs d
      <*> o .:? "atlasSchedulePollMs"   .!= cfgAtlasSchedulePollMs d
      <*> o .:? "chunkTexturePollMs"    .!= cfgChunkTexturePollMs d
      <*> o .:? "snapshotPollMs"        .!= cfgSnapshotPollMs d
      <*> o .:? "timingLogThresholdMs"  .!= cfgTimingLogThresholdMs d
      <*> o .:? "renderTraceEnabled"    .!= cfgRenderTraceEnabled d

-- | Sensible defaults matching the previous hard-coded values.
defaultConfig :: TopoSeerConfig
defaultConfig = TopoSeerConfig
  { cfgFrameDelayMs          = 16
  , cfgAtlasUploadsPerFrame  = 1
  , cfgAtlasCacheEntries     = 4
  , cfgTerrainCachePollMs    = 30
  , cfgAtlasDrainPollMs      = 30
  , cfgAtlasSchedulePollMs   = 30
  , cfgChunkTexturePollMs    = 30
  , cfgSnapshotPollMs        = 30
  , cfgTimingLogThresholdMs  = 10
  , cfgRenderTraceEnabled    = False
  }

-- | Clamp all fields to their documented minimums.
clampConfig :: TopoSeerConfig -> TopoSeerConfig
clampConfig c = c
  { cfgFrameDelayMs          = max 1 (cfgFrameDelayMs c)
  , cfgAtlasUploadsPerFrame  = max 1 (cfgAtlasUploadsPerFrame c)
  , cfgAtlasCacheEntries     = max 1 (cfgAtlasCacheEntries c)
  , cfgTerrainCachePollMs    = max 5 (cfgTerrainCachePollMs c)
  , cfgAtlasDrainPollMs      = max 5 (cfgAtlasDrainPollMs c)
  , cfgAtlasSchedulePollMs   = max 5 (cfgAtlasSchedulePollMs c)
  , cfgChunkTexturePollMs    = max 5 (cfgChunkTexturePollMs c)
  , cfgSnapshotPollMs        = max 5 (cfgSnapshotPollMs c)
  , cfgTimingLogThresholdMs  = max 1 (cfgTimingLogThresholdMs c)
  }

-- | Path to the config file: @~\/.topo\/config.json@.
configFilePath :: IO FilePath
configFilePath = do
  home <- getHomeDirectory
  pure (home </> ".topo" </> "config.json")

-- | Load 'TopoSeerConfig' from @~\/.topo\/config.json@.
--
-- * If the file exists and parses, its values are used (clamped).
-- * If the file does not exist, 'defaultConfig' is returned and the
--   directory is created so the user can drop a config file in later.
-- * If parsing fails, 'defaultConfig' is returned and the error is
--   printed to stderr.
loadConfig :: IO TopoSeerConfig
loadConfig = do
  path <- configFilePath
  exists <- doesFileExist path
  if exists
    then do
      bytes <- BS.readFile path
      case eitherDecodeStrict' bytes of
        Right cfg -> pure (clampConfig cfg)
        Left err  -> do
          putStrLn ("topo-seer: failed to parse " <> path <> ": " <> err <> "; using defaults")
          pure defaultConfig
    else do
      -- Ensure the directory tree exists so the user can create the file.
      home <- getHomeDirectory
      let dir = home </> ".topo"
      createDirectoryIfMissing True dir
      -- Drop a README explaining every config field.
      let readmePath = dir </> "README.txt"
      readmeExists <- doesFileExist readmePath
      unless readmeExists (writeFile readmePath configReadme)
      pure defaultConfig

-- | README text dropped into @~\/.topo\/@ on first run.
configReadme :: String
configReadme = unlines
  [ "topo-seer configuration"
  , "========================"
  , ""
  , "Place a file called `config.json` in this directory to override"
  , "topo-seer runtime settings.  Every field is optional; omitted"
  , "fields fall back to sensible defaults.  Example:"
  , ""
  , "  {"
  , "    \"frameDelayMs\": 16,"
  , "    \"renderTraceEnabled\": true"
  , "  }"
  , ""
  , "Fields"
  , "------"
  , ""
  , "frameDelayMs          (Int, default 16)"
  , "  Target delay between frames, in milliseconds."
  , "  Controls the upper-bound frame rate (e.g. 16 ms ~ 60 fps)."
  , "  Minimum: 1."
  , ""
  , "atlasUploadsPerFrame  (Int, default 1)"
  , "  Maximum number of atlas texture tiles uploaded to the GPU each"
  , "  frame.  Higher values speed up atlas population but can cause"
  , "  frame drops on slower GPUs.  Minimum: 1."
  , ""
  , "atlasCacheEntries     (Int, default 4)"
  , "  Number of atlas scale levels kept in the LRU cache.  Raising"
  , "  this uses more VRAM but avoids re-rendering when toggling"
  , "  between zoom levels.  Minimum: 1."
  , ""
  , "terrainCachePollMs    (Int, default 30)"
  , "  How often (ms) the render loop checks the terrain-cache broker"
  , "  for fresh chunk data.  Minimum: 5."
  , ""
  , "atlasDrainPollMs      (Int, default 30)"
  , "  How often (ms) the render loop drains completed atlas results"
  , "  from the atlas-result broker.  Minimum: 5."
  , ""
  , "atlasSchedulePollMs   (Int, default 30)"
  , "  How often (ms) the render loop checks the atlas-schedule broker"
  , "  for new work items to dispatch.  Minimum: 5."
  , ""
  , "chunkTexturePollMs    (Int, default 30)"
  , "  How often (ms) the render loop refreshes per-chunk textures."
  , "  Minimum: 5."
  , ""
  , "snapshotPollMs        (Int, default 30)"
  , "  How often (ms) the render loop re-reads the shared snapshot"
  , "  IORef.  Lower values make the UI more responsive at the cost"
  , "  of more IORef traffic.  Minimum: 5."
  , ""
  , "timingLogThresholdMs  (Int, default 10)"
  , "  Any render-loop step that takes at least this many milliseconds"
  , "  is logged for performance diagnostics.  Minimum: 1."
  , ""
  , "renderTraceEnabled    (Bool, default false)"
  , "  When true, emit a per-frame timing breakdown into the log."
  , "  Useful for profiling; noisy in production."
  ]
