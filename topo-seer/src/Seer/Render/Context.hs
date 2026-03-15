-- | Shared render context passed to the frame renderer.
module Seer.Render.Context
  ( RenderContext(..)
  ) where

import Actor.AtlasResultBroker (AtlasResultRef)
import Actor.AtlasScheduleBroker (AtlasScheduleRef)
import Actor.AtlasScheduler (AtlasScheduler)
import Actor.Log (Log)
import Actor.Render (RenderSnapshot)
import Actor.SnapshotReceiver (SnapshotVersion)
import Data.Word (Word32)
import Hyperspace.Actor (ActorHandle, Protocol)
import qualified SDL
import Seer.Render.Atlas (AtlasTextureCache)
import Seer.Render.Terrain (TerrainCache)
import System.IO (Handle)
import UI.Font (FontCache)
import UI.TerrainCache (ChunkTextureCache)

-- | Immutable inputs required to render a single frame.
data RenderContext = RenderContext
  { rcRenderer :: !SDL.Renderer
  , rcWindow :: !SDL.Window
  , rcSnapshotVersion :: !SnapshotVersion
  , rcSnapshot :: !RenderSnapshot
  , rcTerrainCache :: !TerrainCache
  , rcChunkTextureCache :: !ChunkTextureCache
  , rcAtlasTextureCache :: !AtlasTextureCache
  , rcLogHandle :: !(ActorHandle Log (Protocol Log))
  , rcAtlasSchedulerHandle :: !(ActorHandle AtlasScheduler (Protocol AtlasScheduler))
  , rcAtlasScheduleRef :: !AtlasScheduleRef
  , rcAtlasResultRef :: !AtlasResultRef
  , rcAtlasUploadsPerFrame :: !Int
  , rcShouldDrainAtlas :: !Bool
  , rcShouldScheduleAtlas :: !Bool
  , rcShouldUpdateChunkTextures :: !Bool
  , rcTimingLogThresholdMs :: !Word32
  , rcFontCache :: !(Maybe FontCache)
  , rcRenderTargetOk :: !Bool
  , rcTraceHandle :: !Handle
  }