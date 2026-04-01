# Internal Development Guide

Architecture and module reference for `topo` core library contributors.

## Contents

### [Architecture Overview](architecture.md)

High-level system architecture: domain groupings, data flow through the
generation pipeline, and the relationship between the major subsystems.

---

### Core Foundation

The foundational types, coordinate systems, and utilities that everything
else builds on.

| Document | Modules |
|----------|---------|
| [Core Types](core/types.md) | `Topo.Types` |
| [World Container](core/world.md) | `Topo.World` |
| [Hex Grid](core/hex-grid.md) | `Topo.Hex` |
| [Planet Model](core/planet.md) | `Topo.Planet` |
| [Calendar & Time](core/calendar.md) | `Topo.Calendar` |
| [Metadata System](core/metadata.md) | `Topo.Metadata`, `Topo.PlateMetadata` |
| [Math & Noise](core/math-and-noise.md) | `Topo.Math`, `Topo.Noise`, `Topo.Seed` |
| [Terrain Grid](core/terrain-grid.md) | `Topo.TerrainGrid` |
| [Units & Conversion](core/units.md) | `Topo.Units`, `Topo.Units.Config` |
| [Config Utilities](core/config.md) | `Topo.Config`, `Topo.Config.JSON` |

### Pipeline Engine

The stage-based generation orchestration system.

| Document | Modules |
|----------|---------|
| [Pipeline Overview](pipeline/overview.md) | `Topo.Pipeline` |
| [Stage Reference](pipeline/stages.md) | `Topo.Pipeline.Stage`, `Topo.Pipeline.Dep` |
| [World Generation](pipeline/world-gen.md) | `Topo.WorldGen` |

### Terrain Generation (Geology)

From noise → plate tectonics → erosion → volcanism.

| Document | Modules |
|----------|---------|
| [Base Height](terrain/base-height.md) | `Topo.BaseHeight` |
| [Tectonics](terrain/tectonics.md) | `Topo.Tectonics` |
| [Erosion](terrain/erosion.md) | `Topo.Erosion` |
| [Hypsometry](terrain/hypsometry.md) | `Topo.Hypsometry` |
| [Volcanism](terrain/volcanism.md) | `Topo.Volcanism` |
| [Terrain Parameters](terrain/parameters.md) | `Topo.Parameters` |
| [Terrain Forms](terrain/terrain-forms.md) | `Topo.TerrainForm.Modifiers` |

### Hydrology & Water

Flow routing, rivers, water bodies, glaciers, ocean currents.

| Document | Modules |
|----------|---------|
| [Flow Routing](hydrology/flow-routing.md) | `Topo.Hydrology` |
| [Rivers](hydrology/rivers.md) | `Topo.River` |
| [Water Bodies](hydrology/water-bodies.md) | `Topo.WaterBody` |
| [Water Table](hydrology/water-table.md) | `Topo.WaterTable` |
| [Ocean Currents](hydrology/ocean-currents.md) | `Topo.OceanCurrent` |
| [Glaciers](hydrology/glaciers.md) | `Topo.Glacier` |

### Climate System

Temperature, wind, moisture transport, precipitation.

| Document | Modules |
|----------|---------|
| [Climate Model](climate/climate-model.md) | `Topo.Climate` |
| [Climate Config](climate/climate-config.md) | `Topo.Climate.Config` |
| [Evaporation](climate/evaporation.md) | `Topo.Climate.Evaporation` |

### Weather Simulation

Tick-based weather evolving over climate state.

| Document | Modules |
|----------|---------|
| [Weather Overview](weather/overview.md) | `Topo.Weather` |
| [Weather Config](weather/config.md) | `Topo.Weather.Config` |
| [Weather Grid](weather/grid.md) | `Topo.Weather.Grid` |
| [Weather Simulation](weather/simulation.md) | `Topo.Weather.Tick`, `Topo.Weather.Operators` |

### Biome Classification

Whittaker-style classification and sub-biome refinement.

| Document | Modules |
|----------|---------|
| [Classification](biome/classification.md) | `Topo.Biome` |
| [Biome Config](biome/config.md) | `Topo.BiomeConfig` |
| [Refinement](biome/refinement.md) | `Topo.Biome.Refine`, `Topo.Biome.Refine.*` |
| [Biome ID Reference](biome/biome-ids.md) | `Topo.Biome.Name` |

### Ecology (Soil & Vegetation)

| Document | Modules |
|----------|---------|
| [Soil](ecology/soil.md) | `Topo.Soil` |
| [Vegetation](ecology/vegetation.md) | `Topo.Vegetation` |

### Overlay System

Extensible user-defined data layers.

| Document | Modules |
|----------|---------|
| [Overlay Overview](overlay/overview.md) | `Topo.Overlay` |
| [Overlay Schema](overlay/schema.md) | `Topo.Overlay.Schema` |
| [Overlay Storage](overlay/storage.md) | `Topo.Overlay.Storage`, `Topo.Overlay.Storage.ChunkIndex` |
| [Overlay Indexing](overlay/indexing.md) | `Topo.Overlay.Index`, `Topo.Overlay.Indexed` |
| [Overlay Provenance](overlay/provenance.md) | `Topo.Overlay.Provenance` |

### Simulation Engine

DAG-based overlay simulation.

| Document | Modules |
|----------|---------|
| [Simulation DAG](simulation/dag.md) | `Topo.Simulation.DAG` |
| [Sim Nodes](simulation/sim-nodes.md) | `Topo.Simulation` |

### Persistence & I/O

Binary formats, world file I/O, query API.

| Document | Modules |
|----------|---------|
| [World Storage](persistence/world-storage.md) | `Topo.Storage` |
| [World Bundle](persistence/world-bundle.md) | `Topo.Persistence.WorldBundle` |
| [Chunk Codecs](persistence/export.md) | `Topo.Export` |
| [Sampling API](persistence/sampling.md) | `Topo.Sample` |
| [Mesh Export](persistence/mesh.md) | `Topo.Mesh` |

### Plugin Internals

Host-side plugin infrastructure (see also: [Plugin Dev Guide](../plugin-dev/index.md)).

| Document | Modules |
|----------|---------|
| [Plugin Monad](plugin-internals/plugin-monad.md) | `Topo.Plugin` |
| [RPC Client](plugin-internals/rpc.md) | `Topo.Plugin.RPC` |
| [Protocol](plugin-internals/protocol.md) | `Topo.Plugin.RPC.Protocol` |
| [Transport](plugin-internals/transport.md) | `Topo.Plugin.RPC.Transport` |
| [Manifest](plugin-internals/manifest.md) | `Topo.Plugin.RPC.Manifest` |
