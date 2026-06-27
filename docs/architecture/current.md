# Current Architecture Baseline

This document records the current repository shape before the 1.0 readiness
work. It is intentionally descriptive rather than aspirational; target
architecture is described in [target.md](target.md).

## Workspace shape

The Stack workspace currently uses resolver `lts-24.2` and contains these
packages:

| Package | Current role | 1.0 implication |
|---|---|---|
| `topo` | Core terrain generation, terrain/world types, overlays, persistence, simulation, plugin protocol/data-resource types, and command types. | Treat as the stable library/API core. |
| `topo-seer` | SDL2 application, render-thread-owned UI loop, actor system, AppService-backed HTTP/OpenAPI server, command dispatch compatibility, plugin manager, rendering, persistence, simulation, and tests. | Becomes the authoritative runtime host: UI, AppService, HTTP/OpenAPI, and plugin supervision. |
| `topo-plugin-sdk` | Haskell SDK for plugin authors, including SDK types, payload helpers, and runner. | Becomes canonical SDK for manifest/protocol/transport behavior. |
| `topo-plugin-example` | Minimal terrain plugin example. | Keep as small fixture/example. |
| `topo-plugin-civ-example` | Richer civilization-style plugin example. | Upgrade toward full-stack plugin fixture coverage. |

The workspace also pins `hyperspace`, `sdl2`, and `sdl2-ttf` through git
extra-dependencies.

## Current `topo` library surface

The core package already exposes a broad terrain and simulation surface:

- core world/types/hex/metadata/config/units;
- pipeline stage and dependency types;
- terrain generation: base height, tectonics, erosion, hypsometry, volcanism,
  parameters, soil, vegetation;
- hydrology: flow routing, rivers, water bodies, water table, glaciers, ocean
  currents;
- climate/weather/biomes and biome refinement modules;
- overlays, schemas, provenance, indexes, JSON, cache, export, and storage;
- persistence: storage, export, samples, mesh, and world bundles;
- simulation DAG;
- plugin capabilities, data-resource schemas, RPC protocol, manifest,
  transport, and command types.

The library is not missing domain breadth. The 1.0 work is mostly about API
discipline, test coverage, service/API exposure, plugin lifecycle clarity, and
UI feature completeness.

## Current `topo-seer` runtime shape

`topo-seer` already has the right high-level direction for a large interactive
application:

- SDL/GPU work is owned by the render thread.
- Hyperspace actors isolate UI actions, terrain generation/data, logs,
  snapshots, atlas/cache work, plugin management, and simulation.
- `Seer.Command.Dispatch` and command handler modules provide a broad remote
  control surface over app state, sliders, view/camera, generation, terrain
  editor, enum listings, terrain queries, world save/load, logs, pipeline,
  plugins, simulation, presets, screenshots, data-resource CRUD, UI panels,
  widgets, dialogs, viewport, keyboard, and input actions.

The current command dispatch surface is a strong seed for a 1.0 service layer.
It should be extracted into typed services rather than duplicated separately in
HTTP, UI, and compatibility command paths.

## Current external control path

The current public automation path is direct topo-seer HTTP/OpenAPI over the
AppService boundary:

```text
external client
  -> topo-seer HTTP/OpenAPI
  -> Seer.HTTP.Server
  -> AppService
  -> actors / UI / runtime state
```

Run the host in headless mode with
`stack exec topo-seer -- --headless --http 127.0.0.1:7373`. The live OpenAPI
contract is served at `GET /openapi.json`; basic smoke checks use
`GET /health`, `GET /state`, and resource routes such as
`GET /terrain/hex?q=0&r=0`.

The legacy command IPC remains an internal compatibility adapter for existing
runtime handlers while service extraction continues, but it is no longer exposed
through a separate MCP package in the Stack workspace. The retired MCP bridge is
only documented in migration notes and should not be used as the 1.0 automation
path.

## Current plugin foundation

The plugin system already has many concepts needed for 1.0:

- capability gates such as logging, noise, terrain/world/overlay read/write,
  and data read/write;
- length-prefixed JSON envelopes;
- handshake, world-changed, generator invocation, simulation invocation,
  shutdown, query-resource, and mutate-resource messages;
- schema-driven data resources with operation flags;
- an SDK with plugin definitions, parameters, generator/simulation callbacks,
  data-resource definitions, payload helpers, and runner;
- a plugin manager actor that discovers manifests/configs, launches plugins,
  handshakes, integrates generator/simulation definitions, and forwards data
  queries/mutations.

The production transport contract is explicit: topo-seer creates a platform
endpoint, passes `TOPO_PLUGIN_ENDPOINT` and `TOPO_PLUGIN_ENDPOINT_KIND` to the
plugin process, and the SDK connects to that endpoint. Stdio compatibility is
restricted to explicit test/development harnesses and is not part of production
plugin launch.

## Current UI shape

The UI already has substantial feature surface:

- render-thread SDL loop;
- actor-isolated background work;
- view modes for elevation, biome, climate/weather, moisture, precipitation,
  plate attributes, vegetation, terrain form, clouds, and overlays;
- config tabs for terrain, planet, climate, weather, biome, erosion, pipeline,
  and data;
- data-browser state for plugin/resource selection, records, pagination,
  edit/create modes, nested fields, cursor/focus, and delete confirmation.

The main problem is concentration of responsibilities:

- `Seer.System` combines boot, actor wiring, command-channel startup, SDL loop,
  events, snapshots, cache scheduling, screenshots, tracing, and cleanup.
- `Seer.Draw.Config` combines panel rendering, pipeline UI, plugin params,
  simulation controls, data browser, popovers, nested record/ADT editing,
  validation-adjacent behavior, and low-level drawing.
- `UI.Layout` contains many hand-coded rectangle calculators/constants for
  panels, tabs, controls, dialogs, popovers, and widgets.

These areas should be refactored into typed geometry, panel components,
reducers, and draw commands so UI behavior can be tested without SDL wherever
possible.

## Current documentation state

The repository already has useful documentation under `docs/`, including an
internal architecture overview, plugin developer guide, format specifications,
and world-bundle docs. Some high-value references still need expansion or
generation before 1.0:

- plugin manifest reference;
- plugin RPC protocol reference;
- pipeline stages reference;
- served HTTP/OpenAPI docs, README curl examples, and contract/golden checks;
- backend-neutral external data-source declarations/grants/status docs.

## Current 1.0 risks

- CI coverage and cross-platform test gates are not yet the release gate.
- HTTP/OpenAPI now appears in the `topo-seer` package dependencies, but the
  standalone `topo-api` split from ADR 0001 is still future work.
- Plugin transport needs real platform transport tests.
- Backend-neutral external data-source integration needs a clear contract so
  topo does not own or privilege any plugin ecosystem data backend.
- UI feature exposure is broad but not yet structured enough to guarantee every
  library domain has a visible surface.
- Some public protocol/reference docs are still stubs and need to become
  generated or contract-tested where possible.
