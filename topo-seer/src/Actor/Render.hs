{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Actor.Render
  ( Render
  , RenderSnapshot(..)
  , renderActorDef
  , setRenderUi
  , setRenderLog
  , setRenderData
  , setRenderTerrain
  , getRenderSnapshot
  ) where

import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Actor.Log (LogLevel(..), LogSnapshot(..))
import Actor.UI (ConfigTab(..), LeftTab(..), UiState(..), ViewMode(..), emptyUiState)
import qualified Data.Text as Text
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)

data RenderSnapshot = RenderSnapshot
  { rsUi :: !UiState
  , rsLog :: !LogSnapshot
  , rsData :: !DataSnapshot
  , rsTerrain :: !TerrainSnapshot
  } deriving (Eq, Show)

data RenderState = RenderState
  { rsUiState :: UiState
  , rsLogState :: LogSnapshot
  , rsDataState :: DataSnapshot
  , rsTerrainState :: TerrainSnapshot
  }

emptyRenderState :: UiState -> LogSnapshot -> DataSnapshot -> TerrainSnapshot -> RenderState
emptyRenderState ui logSnap dataSnap terrainSnap = RenderState
  { rsUiState = ui
  , rsLogState = logSnap
  , rsDataState = dataSnap
  , rsTerrainState = terrainSnap
  }

snapshotRender :: RenderState -> RenderSnapshot
snapshotRender st = RenderSnapshot
  { rsUi = rsUiState st
  , rsLog = rsLogState st
  , rsData = rsDataState st
  , rsTerrain = rsTerrainState st
  }

[hyperspace|
actor Render
  state RenderState
  lifetime Singleton
  schedule pinned 4
  noDeps
  mailbox Unbounded

  cast setUi :: UiState
  cast setLog :: LogSnapshot
  cast setData :: DataSnapshot
  cast setTerrain :: TerrainSnapshot
  call snapshot :: () -> RenderSnapshot

  initial (emptyRenderState emptyUiState (LogSnapshot [] False 0 LogDebug) (DataSnapshot 0 0 Nothing) (TerrainSnapshot 0 0 mempty mempty mempty))
  onPure_ setUi = \ui st -> st { rsUiState = ui }
  onPure_ setLog = \logSnap st -> st { rsLogState = logSnap }
  onPure_ setData = \dataSnap st -> st { rsDataState = dataSnap }
  onPure_ setTerrain = \terrainSnap st -> st { rsTerrainState = terrainSnap }
  onPure snapshot = \() st -> (st, snapshotRender st)
|]

setRenderUi :: ActorHandle Render (Protocol Render) -> UiState -> IO ()
setRenderUi handle ui =
  cast @"setUi" handle #setUi ui

setRenderLog :: ActorHandle Render (Protocol Render) -> LogSnapshot -> IO ()
setRenderLog handle logSnap =
  cast @"setLog" handle #setLog logSnap

setRenderData :: ActorHandle Render (Protocol Render) -> DataSnapshot -> IO ()
setRenderData handle dataSnap =
  cast @"setData" handle #setData dataSnap

setRenderTerrain :: ActorHandle Render (Protocol Render) -> TerrainSnapshot -> IO ()
setRenderTerrain handle terrainSnap =
  cast @"setTerrain" handle #setTerrain terrainSnap

getRenderSnapshot :: ActorHandle Render (Protocol Render) -> IO RenderSnapshot
getRenderSnapshot handle =
  call @"snapshot" handle #snapshot ()
