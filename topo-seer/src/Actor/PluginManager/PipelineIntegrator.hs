-- | Pipeline-facing views of loaded plugins.
module Actor.PluginManager.PipelineIntegrator
  ( buildPluginStages
  , buildPluginOverlaySchemas
  , orderPlugins
  , pluginToStage
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Actor.PluginManager.Types (LoadedPlugin(..), PluginManagerState(..), PluginStatus(..))
import Topo.Overlay.Schema (OverlaySchema)
import Topo.Pipeline (PipelineStage)
import Topo.Plugin.RPC (RPCManifest(..), rpcGeneratorStage)

-- | Build pipeline stages from all loaded plugins that declare
-- a generator section, ordered by the user-defined plugin order.
buildPluginStages :: PluginManagerState -> [PipelineStage]
buildPluginStages st =
  let plugins = pmsPlugins st
      disabled = pmsDisabledPlugins st
      ordered = orderPlugins (pmsPluginOrder st) (Map.elems plugins)
      enabled = filter (\lp -> not (Set.member (lpName lp) disabled)) ordered
  in concatMap pluginToStage enabled

buildPluginOverlaySchemas :: PluginManagerState -> [OverlaySchema]
buildPluginOverlaySchemas st =
  let disabled = pmsDisabledPlugins st
      ordered = orderPlugins (pmsPluginOrder st) (Map.elems (pmsPlugins st))
      enabled = filter (\lp -> not (Set.member (lpName lp) disabled)) ordered
  in [schema | plugin <- enabled, Just schema <- [lpOverlaySchema plugin]]

-- | Reorder plugins according to the user's saved ordering.
-- Plugins not in the ordering list appear at the end.
orderPlugins :: [Text] -> [LoadedPlugin] -> [LoadedPlugin]
orderPlugins order plugins =
  let byName = Map.fromList [(lpName p, p) | p <- plugins]
      ordered = [p | name <- order, Just p <- [Map.lookup name byName]]
      remaining = [p | p <- plugins, lpName p `notElem` order]
  in ordered ++ remaining

-- | Convert a loaded plugin to a pipeline stage if it has a generator
-- declaration.
pluginToStage :: LoadedPlugin -> [PipelineStage]
pluginToStage lp =
  case (lpStatus lp, rmGenerator (lpManifest lp), lpConnection lp) of
    (PluginConnected, Just _genDecl, Just conn) -> [rpcGeneratorStage conn]
    _ -> []
