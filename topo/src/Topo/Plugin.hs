{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Topo.Plugin
  ( Logger
  , Capability(..)
  , PluginCapabilities(..)
  , PluginError(..)
  , allowAllCapabilities
  , PluginEnv(..)
  , PluginM(..)
  , runPluginM
  , requireCapability
  , logInfo
  , noiseAt
  , getWorldP
  , putWorldP
  , modifyWorldP
  , getOverlayP
  , putOverlayP
  , liftTopo
  , TopoEnv(..)
  , TopoM(..)
  , runTopoM
  , topoLog
  , getWorld
  , putWorld
  , modifyWorld
  ) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (StateT, get, put, modify, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Topo.Noise (noise2D)
import Topo.Overlay (Overlay, lookupOverlay, insertOverlay, overlayName)
import Topo.World (TerrainWorld(..))

type Logger = Text -> IO ()

data Capability
  = CapLog
  | CapNoise
  | CapReadTerrain
  | CapWriteTerrain
  | CapReadOverlay
  | CapWriteOverlay
  | CapReadWorld
  | CapWriteWorld
  deriving (Eq, Ord, Show)

instance FromJSON Capability where
  parseJSON = withText "Capability" $ \t -> case t of
    "log" -> pure CapLog
    "noise" -> pure CapNoise
    "readTerrain" -> pure CapReadTerrain
    "writeTerrain" -> pure CapWriteTerrain
    "readOverlay" -> pure CapReadOverlay
    "writeOverlay" -> pure CapWriteOverlay
    "readWorld" -> pure CapReadWorld
    "writeWorld" -> pure CapWriteWorld
    _ -> fail ("unknown capability: " <> Text.unpack t)

instance ToJSON Capability where
  toJSON cap = case cap of
    CapLog -> "log"
    CapNoise -> "noise"
    CapReadTerrain -> "readTerrain"
    CapWriteTerrain -> "writeTerrain"
    CapReadOverlay -> "readOverlay"
    CapWriteOverlay -> "writeOverlay"
    CapReadWorld -> "readWorld"
    CapWriteWorld -> "writeWorld"

newtype PluginCapabilities = PluginCapabilities (Set Capability)
  deriving (Eq, Show)

allowAllCapabilities :: PluginCapabilities
allowAllCapabilities = PluginCapabilities
  (Set.fromList
    [ CapLog
    , CapNoise
    , CapReadTerrain
    , CapWriteTerrain
    , CapReadOverlay
    , CapWriteOverlay
    , CapReadWorld
    , CapWriteWorld
    ])

data PluginEnv = PluginEnv
  { peLogger :: !Logger
  , peSeed   :: !Word64
  , peCaps   :: !PluginCapabilities
  }

-- | Errors raised by plugin capability checks.
data PluginError
  = PluginMissingCapability !Capability
  | PluginInvariantError !Text
  deriving (Eq, Show)

data TopoEnv = TopoEnv
  { teLogger :: !Logger
  }

newtype TopoM a = TopoM
  { unTopoM :: ReaderT TopoEnv (StateT TerrainWorld IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader TopoEnv)

newtype PluginM a = PluginM
  { unPluginM :: ReaderT PluginEnv (ExceptT PluginError TopoM) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader PluginEnv, MonadError PluginError)

runTopoM :: TopoEnv -> TerrainWorld -> TopoM a -> IO (a, TerrainWorld)
runTopoM env world action = runStateT (runReaderT (unTopoM action) env) world

runPluginM :: PluginEnv -> PluginM a -> TopoM (Either PluginError a)
runPluginM env action = runExceptT (runReaderT (unPluginM action) env)

logInfo :: Text -> PluginM ()
logInfo msg = do
  requireCapability CapLog
  logger <- peLogger <$> ask
  liftIO (logger msg)

topoLog :: Text -> TopoM ()
topoLog msg = do
  logger <- teLogger <$> ask
  liftIO (logger msg)

noiseAt :: Int -> Int -> PluginM Float
noiseAt x y = do
  requireCapability CapNoise
  seed <- peSeed <$> ask
  pure (noise2D seed x y)

requireCapability :: Capability -> PluginM ()
requireCapability cap = do
  PluginCapabilities caps <- peCaps <$> ask
  when (not (Set.member cap caps)) $
    throwError (PluginMissingCapability cap)

requireAnyCapability :: [Capability] -> PluginM ()
requireAnyCapability required = do
  PluginCapabilities caps <- peCaps <$> ask
  case required of
    [] -> pure ()
    (fallback:_) ->
      when (not (any (`Set.member` caps) required)) $
        throwError (PluginMissingCapability fallback)

liftTopo :: TopoM a -> PluginM a
liftTopo = PluginM . lift . lift

getWorldP :: PluginM TerrainWorld
getWorldP = do
  requireAnyCapability [CapReadWorld, CapReadTerrain]
  liftTopo getWorld

putWorldP :: TerrainWorld -> PluginM ()
putWorldP world = do
  requireAnyCapability [CapWriteWorld, CapWriteTerrain]
  liftTopo (putWorld world)

modifyWorldP :: (TerrainWorld -> TerrainWorld) -> PluginM ()
modifyWorldP f = do
  requireAnyCapability [CapWriteWorld, CapWriteTerrain]
  liftTopo (modifyWorld f)

getOverlayP :: Text -> PluginM (Maybe Overlay)
getOverlayP overlayNameToLookup = do
  requireAnyCapability [CapReadOverlay, CapReadWorld]
  world <- liftTopo getWorld
  pure (lookupOverlay overlayNameToLookup (twOverlays world))

putOverlayP :: Overlay -> PluginM ()
putOverlayP overlay = do
  requireAnyCapability [CapWriteOverlay, CapWriteWorld]
  liftTopo (modifyWorld writeOverlay)
  where
    writeOverlay world =
      let nextStore = insertOverlay overlay (twOverlays world)
          nextManifest = overlayName overlay : twOverlayManifest world
      in world
        { twOverlays = nextStore
        , twOverlayManifest = dedupeManifest nextManifest
        }

    dedupeManifest [] = []
    dedupeManifest (name:rest) = name : filter (/= name) (dedupeManifest rest)

getWorld :: TopoM TerrainWorld
getWorld = TopoM get

-- | Store the world in the pipeline state, forcing it to WHNF.
--
-- 'Control.Monad.State.Strict.put' only forces the @(a, s)@ pair, not
-- @s@ itself.  Without the bang pattern, each 'putWorld' stores a
-- lazy thunk, and deferred work from prior stages cascades when a
-- later stage reads the terrain — making the wrong stage appear to
-- hang.  Forcing here ensures each stage pays for its own compute.
putWorld :: TerrainWorld -> TopoM ()
putWorld !w = TopoM (put w)

-- | Modify the world in-place, forcing the result to WHNF.
--
-- See 'putWorld' for rationale on the strict evaluation.
modifyWorld :: (TerrainWorld -> TerrainWorld) -> TopoM ()
modifyWorld f = TopoM $ do
  s <- get
  put $! f s
