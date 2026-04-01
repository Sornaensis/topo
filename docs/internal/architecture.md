# Architecture Overview

## System Design

Topo is a procedural world generation library built around three
fundamental abstractions:

1. **TerrainWorld** — a mutable container of chunk maps holding all
   per-tile terrain, climate, hydrology, biome, and vegetation data.
2. **Pipeline** — an ordered sequence of generation stages that
   progressively populate a `TerrainWorld` from an empty state to a
   fully-realized terrain.
3. **Overlays** — user-extensible, schema-driven data layers that sit
   alongside core terrain data, enabling plugins and simulation nodes
   to attach arbitrary geographic information.

## Domain Groups

The library is organized into 13 cohesive domain groups:

```
┌─────────────────────────────────────────────────────────┐
│                    Core Foundation                        │
│  Types · World · Hex · Planet · Calendar · Metadata      │
│  Math · Noise · Seed · TerrainGrid · Units · Config      │
└──────────────────────────┬──────────────────────────────┘
                           │
┌──────────────────────────▼──────────────────────────────┐
│                    Pipeline Engine                        │
│  Pipeline · Stage · Dep · WorldGen                       │
└──────────────────────────┬──────────────────────────────┘
                           │
         ┌─────────────────┼───────────────────┐
         ▼                 ▼                   ▼
┌─────────────┐  ┌─────────────────┐  ┌──────────────┐
│  Geology    │  │  Hydrology      │  │  Climate     │
│  BaseHeight │  │  FlowRouting    │  │  Temperature │
│  Tectonics  │  │  Rivers         │  │  Wind        │
│  Erosion    │  │  WaterBody      │  │  Moisture    │
│  Hypsometry │  │  WaterTable     │  │  Precip      │
│  Volcanism  │  │  OceanCurrent   │  │  Evaporation │
│  Parameters │  │  Glacier        │  │              │
└──────┬──────┘  └────────┬────────┘  └──────┬───────┘
       │                  │                  │
       └──────────────────┼──────────────────┘
                          ▼
         ┌────────────────────────────────┐
         │  Biome Classification          │
         │  Whittaker rules · Refinement  │
         │  56+ sub-biomes                │
         └───────────────┬────────────────┘
                         ▼
              ┌─────────────────────┐
              │  Ecology            │
              │  Soil · Vegetation  │
              └──────────┬──────────┘
                         ▼
    ┌────────────────────┼────────────────────┐
    ▼                    ▼                    ▼
┌──────────┐  ┌──────────────────┐  ┌──────────────┐
│ Weather  │  │  Overlay System  │  │ Simulation   │
│ Init     │  │  Schema · Store  │  │ DAG          │
│ Tick     │  │  Index · JSON    │  │ SimNode      │
│ Advect   │  │  Provenance      │  │ Wavefront    │
└──────────┘  └──────────────────┘  └──────────────┘
                         │
              ┌──────────┼──────────┐
              ▼                     ▼
    ┌──────────────────┐  ┌──────────────────┐
    │  Plugin System   │  │  Persistence     │
    │  PluginM · RPC   │  │  .topo · .topolay│
    │  Manifest        │  │  WorldBundle     │
    │  Transport       │  │  Sample · Mesh   │
    └──────────────────┘  └──────────────────┘
```

## Data Flow: Generation Pipeline

The default pipeline executes stages in dependency order. A simplified
view of the data flow:

```
Seed + WorldGenConfig
  │
  ├─► BaseHeight          Pure noise heightmap
  ├─► Tectonics           Plate boundaries, crust type, hardness
  ├─► Hypsometry          Elevation redistribution
  ├─► Erosion             Hydraulic + thermal erosion
  ├─► Volcanism           Volcanic vents + deposits
  ├─► Soil                Soil type/depth/fertility from geology
  ├─► Vegetation Bootstrap  Albedo estimate from terrain
  │
  ├─► Climate             Temperature, wind, precipitation
  ├─► Biome Classification  Whittaker rules → primary biome
  │   └─► (converge)     Climate ↔ Biome iterate until stable
  │
  ├─► Biome Refinement    Sub-biome resolution (56+ biomes)
  ├─► Vegetation Feedback  Biome-aware canopy/density
  ├─► Hydrology           Flow routing, depression filling
  ├─► Rivers              River topology extraction
  ├─► Water Body          Ocean/lake/sea classification
  ├─► Water Table         Subsurface water model
  ├─► Ocean Currents      Surface current model
  ├─► Glacier             Snow/ice accumulation + flow
  ├─► Parameters          Derived terrain params (slope, form, etc.)
  │
  ├─► Weather Init        Initialize weather overlay from climate
  ├─► [Plugin stages]     User generators inserted by dependency
  │
  └─► TerrainWorld        Fully populated world state
```

## Key Architectural Decisions

### Chunk-Based Storage

The world is subdivided into fixed-size hexagonal chunks. Each chunk
stores per-tile data for its region. Chunk maps (`IntMap ChunkType`)
are the primary data structure in `TerrainWorld`.

Cross-chunk operations (erosion, flow routing, climate advection) use
`TerrainGrid` to assemble temporary global grids from chunk maps,
perform stencil operations, then write results back.

### Normalised Internal Representation

All internal values are normalised to `[0,1]` or `[-1,1]` ranges.
Real-world units (°C, metres, mm/year) exist only at the boundary:
configuration input (`Units.Config`) and display/export output (`Units`).
This keeps generators unit-agnostic and numerically stable.

### Overlay Extensibility

The overlay system provides a schema-driven mechanism for attaching
arbitrary data layers to the hex grid. Overlays can be sparse (per-tile
struct) or dense (per-field unboxed vector). They are persisted as
`.topolay` sidecar files alongside the core `.topo` world file.

### Plugin Isolation

Plugins run as separate OS processes communicating over length-prefixed
JSON messages on named pipes (Windows) or Unix domain sockets. The
`PluginM` monad gates capabilities (terrain read, overlay write, logging)
at the type level.

### Simulation DAG

Post-generation, overlays can evolve over time via a directed acyclic
graph of `SimNode`s. The DAG executor runs concurrent reader wavefronts
followed by sequential terrain writers, preserving determinism.

## Module Count by Domain

| Domain | Modules | Approx LOC |
|--------|---------|------------|
| Core Foundation | 14 | ~4,200 |
| Pipeline Engine | 4 | ~890 |
| Terrain (Geology) | 7 | ~2,900 |
| Hydrology & Water | 6 | ~2,970 |
| Climate | 3 | ~1,870 |
| Weather | 6 | ~1,090 |
| Biome Classification | 18 | ~2,100 |
| Soil & Vegetation | 3 | ~1,870 |
| Overlay System | 9 | ~2,610 |
| Simulation | 2 | ~600 |
| Plugin System | 5 | ~1,850 |
| Persistence & I/O | 5 | ~2,990 |
| Units & Display | 2 | ~1,060 |
| **Total** | **84** | **~27,000** |
