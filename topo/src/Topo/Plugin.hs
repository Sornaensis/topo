{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Topo.Noise (noise2D)
import Topo.World (TerrainWorld)

type Logger = Text -> IO ()

data Capability
  = CapLog
  | CapNoise
  | CapReadWorld
  | CapWriteWorld
  deriving (Eq, Ord, Show)

newtype PluginCapabilities = PluginCapabilities (Set Capability)
  deriving (Eq, Show)

allowAllCapabilities :: PluginCapabilities
allowAllCapabilities = PluginCapabilities (Set.fromList [CapLog, CapNoise, CapReadWorld, CapWriteWorld])

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

liftTopo :: TopoM a -> PluginM a
liftTopo = PluginM . lift . lift

getWorldP :: PluginM TerrainWorld
getWorldP = do
  requireCapability CapReadWorld
  liftTopo getWorld

putWorldP :: TerrainWorld -> PluginM ()
putWorldP world = do
  requireCapability CapWriteWorld
  liftTopo (putWorld world)

modifyWorldP :: (TerrainWorld -> TerrainWorld) -> PluginM ()
modifyWorldP f = do
  requireCapability CapWriteWorld
  liftTopo (modifyWorld f)

getWorld :: TopoM TerrainWorld
getWorld = TopoM get

putWorld :: TerrainWorld -> TopoM ()
putWorld = TopoM . put

modifyWorld :: (TerrainWorld -> TerrainWorld) -> TopoM ()
modifyWorld = TopoM . modify
