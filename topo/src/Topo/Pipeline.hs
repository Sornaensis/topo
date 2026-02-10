{-# LANGUAGE OverloadedStrings #-}

module Topo.Pipeline
  ( PipelineStage(..)
  , PipelineConfig(..)
  , PipelineSnapshot(..)
  , PipelineError(..)
  , runPipeline
  ) where

import Control.Monad (foldM)
import Data.Bits (xor)
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Topo.Plugin
import Topo.World (TerrainWorld)

data PipelineStage = PipelineStage
  { stageName :: !Text
  , stageSeedTag :: !Text
  , stageRun  :: PluginM ()
  }

data PipelineConfig = PipelineConfig
  { pipelineSeed       :: !Word64
  , pipelineStages     :: ![PipelineStage]
  , pipelineSnapshots  :: !Bool
  }

data PipelineSnapshot = PipelineSnapshot
  { snapshotName  :: !Text
  , snapshotWorld :: !TerrainWorld
  }

-- | Pipeline failures surfaced from plugin stages.
data PipelineError
  = PipelinePluginError !PluginError
  deriving (Eq, Show)

runPipeline :: PipelineConfig -> TopoEnv -> TerrainWorld -> IO (Either PipelineError (TerrainWorld, [PipelineSnapshot]))
runPipeline config env world = do
  let logger = teLogger env
      pluginEnv = PluginEnv
        { peLogger = logger
        , peSeed = pipelineSeed config
        , peCaps = allowAllCapabilities
        }
  (result, world') <- runTopoM env world $
    foldM (runStage pluginEnv (pipelineSnapshots config)) (Right []) (pipelineStages config)
  pure $ case result of
    Left err -> Left (PipelinePluginError err)
    Right snapshots -> Right (world', snapshots)

runStage :: PluginEnv -> Bool -> Either PluginError [PipelineSnapshot] -> PipelineStage -> TopoM (Either PluginError [PipelineSnapshot])
runStage pluginEnv snapshotsEnabled acc stage =
  case acc of
    Left err -> pure (Left err)
    Right snapshots -> do
      let stageEnv = pluginEnv { peSeed = stageSeed (peSeed pluginEnv) (stageSeedTag stage) }
      topoLog ("stage:start " <> stageName stage)
      result <- runPluginM stageEnv (stageRun stage)
      topoLog ("stage:end " <> stageName stage)
      case result of
        Left err -> pure (Left err)
        Right _ ->
          if snapshotsEnabled
            then do
              world <- getWorld
              pure (Right (snapshots <> [PipelineSnapshot (stageName stage) world]))
            else pure (Right snapshots)

stageSeed :: Word64 -> Text -> Word64
stageSeed base tag =
  let salt = hashText tag
  in base `xor` salt

hashText :: Text -> Word64
hashText = Text.foldl' step 1469598103934665603
  where
    step h c = (h `xor` fromIntegral (ord c)) * 1099511628211
