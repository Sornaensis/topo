# Target Architecture for 1.0

This document describes the intended Topo 1.0 architecture. It should be read
with [current.md](current.md), which records the baseline before the 1.0
readiness work.

## System overview

```text
                         ┌────────────────────────────┐
                         │        topo library         │
                         │ terrain, simulation,        │
                         │ overlays, persistence,      │
                         │ plugin protocol types       │
                         └──────────────┬─────────────┘
                                        │
┌──────────────────────────────┐        │        ┌──────────────────────────────┐
│        SDL/UI components      │        │        │      HTTP/OpenAPI server      │
│ panels, widgets, reducers,    │        │        │ typed routes, events,         │
│ draw commands, SDL renderer   │        │        │ generated OpenAPI             │
└───────────────┬──────────────┘        │        └──────────────┬───────────────┘
                │                       │                       │
                └──────────────┬────────┴────────┬──────────────┘
                               │                 │
                         ┌─────▼─────────────────▼─────┐
                         │      topo-seer AppService     │
                         │ typed operations, validation, │
                         │ actor messaging, persistence, │
                         │ errors, events, status        │
                         └─────┬─────────────────┬──────┘
                               │                 │
                  ┌────────────▼──────┐   ┌──────▼────────────────────┐
                  │ Hyperspace actors │   │ Plugin supervisor system   │
                  │ UI/data/log/sim/  │   │ discovery, dependency DAG, │
                  │ cache/render work │   │ transport, CRUD routing,   │
                  │                   │   │ external data-source status│
                  └───────────────────┘   └────────────┬──────────────┘
                                                       │
                                             ┌─────────▼──────────┐
                                             │ external plugins    │
                                             │ named pipe / UDS    │
                                             │ length-prefixed JSON│
                                             └────────────────────┘
```

## Core principles

1. **One behavior layer.** UI, HTTP, command compatibility, and tests should
   call the same typed service operations.
2. **Generated public contracts.** HTTP/OpenAPI, manifest, protocol, and other
   public contracts should be generated or contract-tested wherever practical.
3. **Explicit plugin isolation.** Production plugin communication uses named
   pipes on Windows and Unix domain sockets on Unix-like platforms.
4. **Backend-neutral external data sources.** Topo tracks plugin declarations,
   grants, dependencies, status, and diagnostics; it does not own or privilege a
   specific external data backend.
5. **Testable UI.** UI state, layout, input routing, reducers, and draw commands
   should be testable without SDL where possible; SDL/GPU ownership remains on
   the render thread.
6. **Cross-platform release gates.** Linux and Windows behavior must both be
   covered by CI before 1.0.

## AppService boundary

`Seer.Service.AppService` is the central boundary for 1.0 behavior. It should
contain focused services for:

- app state, UI state, panels, view modes, and camera;
- config sliders, enums, presets, and validation;
- world generation, generation status, metadata, save/load, and world listing;
- terrain hex/chunk/stat queries, search, and export;
- terrain editor state, tools, brush settings, strokes, undo, and redo;
- overlays and overlay field/schema/provenance access;
- pipeline stages, dependency closure, and diagnostics;
- plugin status, params, capabilities, dependencies, resources, external
  data-source declarations/status, and logs;
- data-resource CRUD and validation;
- simulation tick controls and DAG status;
- logs, screenshots, async status, and events.

The legacy command dispatcher becomes a compatibility adapter over AppService.
Tests should be able to exercise service operations directly without going
through UI or HTTP plumbing.

## HTTP/OpenAPI host

`topo-seer` should expose direct HTTP over AppService.

The amended framework/package decision is recorded in
[ADR 0001: HTTP Framework and API Package Structure](decisions/0001-http-framework-and-api-package.md):
the HTTP MVP uses WAI/Warp and a route metadata table inside `topo-seer`; a
future hardening pass may split stable route types, DTOs, public errors, events,
and OpenAPI generation into a sibling `topo-api` package.

Runtime defaults:

- bind to `127.0.0.1` by default;
- default port `7373`, configurable by CLI/config/env;
- CORS disabled by default;
- optional local bearer token;
- token/auth required for non-loopback binding;
- headless/test mode for CI and contract tests;
- generated `GET /openapi.json`.

Concrete client documentation should start from the direct host and served
contract:

```sh
stack exec topo-seer -- --headless --http 127.0.0.1:7373
curl http://127.0.0.1:7373/openapi.json
curl http://127.0.0.1:7373/state
curl 'http://127.0.0.1:7373/terrain/hex?q=0&r=0'
```

Initial route groups:

- meta: `/health`, `/version`, `/openapi.json`, `/events`;
- app/UI state: `/state`, `/state/view-modes`, `/ui/state`, `/ui/panels`;
- config: `/config/summary`, `/config/sliders`, `/config/enums?type={type}`;
- view/camera/overlays: `/camera`, `/overlays`, overlay field routes;
- worlds/generation: current world generation/status/meta/save/load/list;
- terrain: hex, chunk, stats, search, export;
- editor: state, tools, brush settings, strokes, undo/redo;
- pipeline/plugins: stages, plugin status, params, dependencies, resources,
  external data-source status;
- data resources: schema and CRUD routes;
- simulation: state, tick, DAG;
- logs, presets, and screenshots.

OpenAPI must be generated from the same route metadata or typed routes used by
dispatch. Drift between implementation, handlers, route schemas, examples, and
golden artifacts should fail tests as the contract hardens.

## HTTP automation path and command compatibility

The target public automation path is direct HTTP/OpenAPI. The former MCP bridge
has been removed from the workspace; remaining command IPC is internal/test
compatibility while service extraction continues.

Ongoing cleanup steps:

1. Keep service/HTTP/OpenAPI tests equivalent to the command compatibility
   surface where that surface still exists.
2. Keep docs and examples focused on HTTP/OpenAPI as the public automation path.
3. Avoid command-channel log terminology that implies MCP is the public client
   path.

Command IPC may remain temporarily as internal/test compatibility if needed,
but it is not the documented 1.0 automation surface and has no public 1.0
exception.

## Plugin supervisor architecture

The plugin manager should become a set of focused pieces under a root
supervisor:

```text
PluginRootSupervisor
├─ PluginDirectoryScanner
├─ ManifestValidator
├─ PluginDependencyResolver
├─ ResourceRegistry
├─ ExternalDataSourceRegistry
├─ PluginSupervisor[id]
│  ├─ ProcessLauncher
│  ├─ TransportSession
│  ├─ HandshakeSession
│  ├─ HealthMonitor
│  └─ PluginRouter
├─ DataResourceRouter
├─ PipelinePluginIntegrator
└─ SimulationPluginIntegrator
```

The external actor interface can stay stable during the refactor; behavior moves
behind the interface into smaller testable modules.

Each plugin should expose lifecycle state such as:

```text
Discovered -> ManifestLoaded -> Validated -> WaitingForDependencies
  -> Starting -> Connecting -> Handshaking -> Ready
  -> Degraded -> Stopping -> Stopped -> Failed
```

Transitions should record timestamps, reasons, error codes/messages, blocking
dependencies, process id, negotiated protocol version, resources, external
data-source grants/status, capabilities, and last error.

## Production plugin transport

The 1.0 production transport contract is:

1. Host creates a unique endpoint per plugin process.
2. Host launches the plugin with environment variables such as plugin id,
   protocol version, endpoint, endpoint kind, session id, auth token, world id,
   and data root.
3. SDK connects to the endpoint.
4. Host and plugin communicate through 4-byte little-endian length-prefixed JSON
   envelopes.
5. Host validates protocol version, plugin identity, auth/session, manifest
   compatibility, and capabilities.

Platform transports:

- Windows: named pipes.
- Linux/macOS: Unix domain sockets.

Stdio may remain only behind an explicit test/development flag if compatibility
is needed during migration. It is not the production contract.

## Manifest v3 and dependencies

Manifest v3 should be the SDK-generated source of truth for plugin identity,
runtime, protocol support, capabilities, parameters, generator/simulation
integration, overlays, resources, backend-neutral external data-source
relationships, and UI hints.

The versioning strategy is recorded in
[ADR 0002: Plugin Protocol and Manifest Versioning Strategy](decisions/0002-plugin-protocol-versioning.md):
manifest v3 and plugin RPC protocol v3 are the production-supported 1.0 plugin
contract; older manifests/protocols and stdio transport are migration or
test/development compatibility only.

The dependency resolver should treat dependencies as one graph:

- built-in pipeline stage dependencies;
- plugin version dependencies;
- overlay dependencies;
- data-resource dependencies;
- backend-neutral external data-source provider/consumer relationships;
- capability dependencies;
- optional or soft dependencies.

The resolver validates existence, detects cycles, produces startup order,
pipeline insertion order, simulation DAG insertion order, and user-facing
diagnostics for missing, disabled, failed, or cyclic dependencies.

## Backend-neutral external data-source contract

Plugins may need to coordinate through external data sources. Topo's contract is
to expose a backend-neutral coordination layer, not to own the backend.

Topo may track:

- requirement declarations;
- provider identity;
- access mode and capability scope;
- opaque reference/configuration metadata;
- health/status and diagnostics;
- grants and revocations;
- dependency blocking/degraded state;
- references/metadata needed for world/plugin compatibility.

Topo must not require:

- backend-specific migration tables;
- database locking policy;
- filesystem layouts;
- schema/table naming rules;
- a particular database or service implementation.

Backend-specific work belongs to provider plugins, adapters, user
configuration, or external systems. SQLite is acceptable as a fixture or adapter
example, but not as a privileged shared-state specification.

## UI architecture

Target UI organization:

```text
Seer/
  App/              -- boot, main loop, event pump, render frame, snapshots
  Service/          -- AppService and focused services
  UI/
    Model           -- UI state/view models
    Component       -- component boundaries
    Geometry        -- typed layout primitives
    Theme           -- colors/fonts/sizing tokens
    Input           -- normalized input events
    Widgets/        -- reusable controls
    Panels/         -- top bar, config, pipeline, data browser, plugins, etc.
    Render/         -- draw commands and SDL interpreter
```

Priority extraction targets:

1. DataBrowser as a standalone reducer/component calling AppService.
2. Config/pipeline/plugin/simulation panels split from `Seer.Draw.Config`.
3. Typed geometry replacing broad rectangle helper sprawl.
4. `Seer.System` split into boot, event, render, snapshot/cache, and screenshot
   modules.

## Feature-completion approach

Use an inspector-first strategy to make every major library domain visible:

- coordinates, chunks, world metadata;
- elevation, base height, hypsometry;
- tectonics and plate fields;
- erosion, terrain form, slope, roughness;
- hydrology, rivers, water bodies, water table;
- climate, weather, wind, precipitation, evaporation;
- biome, sub-biome, soil, vegetation;
- glacier, snow, ice;
- volcanism and ocean currents;
- overlays, plugin data records, provenance, units.

Then deepen the specialized view modes, legends, pipeline diagnostics, overlay
manager, export/import dialogs, simulation DAG, and plugin resource/status UI.

## Release posture

The target architecture is not complete until it is enforced by CI and release
gates:

- Linux and Windows builds.
- Coverage reports and thresholds.
- Service operation tests.
- HTTP contract and OpenAPI drift tests.
- Plugin transport/lifecycle/failure tests.
- Backend-neutral external data-source provider/consumer tests.
- UI reducer/layout/draw-command tests.
- Headless HTTP and renderer smoke tests.
- Generated or contract-tested public docs for OpenAPI, manifest, RPC,
  pipeline, and external data-source declarations.
