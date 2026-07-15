# Public Module and Feature Inventory

This inventory is the maintained 1.0 feature matrix for exposed library
modules, public HTTP routes, AppService operations, and present/missing feature
scope. The machine-readable source is [`public-surface.json`](public-surface.json),
and the drift checks are [`tools/check-public-surface.py`](../../tools/check-public-surface.py)
and the `Spec.FeatureMatrix` test.

The active hmem Topo 1.0 project tree supplies the feature-completion scope.
Package `exposed-modules`, `Seer.HTTP.Server.publicHttpRouteSpecs`, and
`Seer.Service.AppService.appServiceOperationSpecs` supply the public surfaces.
No separate markdown planning file is required as the source of truth.

## Maintenance contract

Run the checks after changing any package `exposed-modules` list, public route,
AppService operation, feature entry, or feature-group mapping:

```sh
python tools/check-public-surface.py
stack test topo-seer:topo-seer-test --test-arguments '--match Feature'
```

The checks also run in the `Feature matrix` GitHub Actions workflow on pull
requests and pushes that touch package metadata, the inventory, route/service
source, the checker, or the feature-matrix test.

The check fails when any of these conditions is true:

- a Stack package exposes library modules but is not listed in the inventory;
- an exposed module is missing from all feature groups;
- an inventoried module is no longer exposed;
- a module is mapped by more than one feature group;
- package, present/missing feature, route, operation, or feature-group entries
  are duplicated;
- package role metadata is missing;
- a listed test reference does not resolve to a test module or is not imported by
  the test suite main module;
- a package test suite main file referenced by test metadata cannot be found;
- a feature group lacks owner, status, UI surface, service surface, HTTP surface,
  and tests or an explicit waiver;
- a top-level present/missing feature lacks owner, status, and tests or an
  explicit waiver;
- a public route from `publicHttpRouteSpecs` is missing from the matrix or the
  matrix lists a stale route;
- an AppService operation from `appServiceOperationSpecs` is missing from the
  matrix or the matrix lists a stale operation.

## Current feature groups

| Feature group | Status | Owner | UI surface | Service/HTTP surface | Test/waiver state |
|---|---|---|---|---|---|
| Core model, coordinates, metadata, configuration, units, and deterministic helpers | Present | topo core maintainers | Topo-seer state/config/terrain and inspector surfaces cover core domain fields. | AppService and HTTP expose state/config/terrain metadata and terrain hex/chunk queries. | Covered by existing topo core specs. |
| Pipeline, world generation, tectonics, terrain forms, erosion, and volcanism | Partial | topo generation maintainers | Generation controls, view modes, pipeline diagnostics, and inspector sections cover these fields. | AppService and HTTP expose generation, terrain, pipeline, diagnostics, and export metadata. | Covered by generation/pipeline specs and parity tests. |
| Hydrology, rivers, water bodies, water table, glaciers, and ocean currents | Partial | topo domain maintainers | River/water rendering, inspector sections, and view-mode metadata cover these fields. | AppService and public terrain routes expose hydrology, water body, water table, glacier, and current fields. | Covered by hydrology/water specs and surface parity tests. |
| Climate, weather, biome refinement, soil, and vegetation | Partial | topo domain maintainers | Climate/weather/biome views, legends, units, timeline/status, and inspector sections cover these fields. | AppService and public terrain routes expose climate, weather, biome, soil, and vegetation fields. | Covered by climate/weather/biome/ecology specs and surface parity tests. |
| Overlays, schema/provenance/indexing, storage, world bundles, mesh, and export | Partial | topo persistence and overlay maintainers | Overlay selection/rendering, manager, provenance, persistence actions, and import/export dialogs are exposed. | AppService and HTTP expose overlay, persistence, export, schema, provenance, and terrain sample routes. | Covered by overlay/persistence/export specs and golden/API tests. |
| Simulation and simulation DAG | Present | topo simulation maintainers | Simulation controls now expose tick readiness, auto-tick/rate controls, DAG node status, tick logs, and plugin simulation-node declarations. | AppService and HTTP expose simulation state, tick, DAG node/status, plugin-node, and tick-log surfaces. | Covered by topo and topo-seer simulation plus HTTP specs. |
| Plugin capability, manifest, RPC, transport, payload, and data-resource contracts | Present | topo plugin maintainers | Plugin and data-browser UI expose lifecycle status, diagnostics/log lines, capabilities, params, dependencies, schemas/CRUD, pagination, validation, and backend-neutral external data-source grants/status. | AppService and HTTP expose plugin status/state/dependencies, params, resources, external data-source grants/status, data-resource schemas, CRUD, pagination, validation errors, and state. | Covered by plugin manager, data-resource, CRUD e2e, and HTTP specs. |
| Command DTO compatibility types | Compatibility | topo-seer service maintainers | Used indirectly by internal/test command paths. | AppService maps commands to typed operations; HTTP/OpenAPI is the public 1.0 automation surface. | Existing command specs. |
| Topo-seer actor runtime, workers, plugin manager, logging, snapshots, and UI state actor | Partial | topo-seer runtime maintainers | Runtime-backed SDL status is exposed through AppService/reducers. | AppService and HTTP expose status, events, logs, plugin, simulation, and generation/terrain status routes. | Covered by actor/runtime specs and service/API parity tests. |
| Command IPC, dispatch, and UI action compatibility adapters | Compatibility | topo-seer service maintainers | Current UI and internal/test named-pipe/Unix-socket command IPC compatibility path. | AppService backs retained IPC adapters; HTTP/OpenAPI is public automation, while `/commands/*` is permanently absent with no enable flag. | Existing command IPC tests plus HTTP route-absence and friendly-route parity tests. |
| Application boot, runtime config, snapshots, persistence, screenshots, system wiring, and timing | Partial | topo-seer runtime maintainers | App/config/save-load/screenshot and persistence/export/import diagnostics are exposed. | AppService and HTTP expose runtime/config/persistence, health/state/config/world/screenshot/log/event routes. | Covered by config/persistence specs and API tests. |
| SDL UI drawing, input routing, editor, renderer, widgets, layout, theme, terrain atlas/cache, and view rendering | Partial | topo-seer UI maintainers | Primary SDL UI uses componentized panels/reducers/layout/draw commands with M9 feature gaps closed. | AppService and HTTP expose UI state/actions/metadata rather than SDL rendering internals. | Covered by UI/editor/render specs, view-mode registry golden/API parity tests, and component/API parity tests. |
| Plugin SDK declarations, payload helpers, terrain/overlay helpers, and runner | Planned | topo plugin maintainers | Developer-facing SDK; runtime declarations surface through plugin UI. | SDK feeds manifest/protocol/resource services and canonical manifest v3 generation. | Covered by SDK payload/runner specs and manifest v3 golden tests. |

## Package coverage summary

The inventory currently covers exposed library modules from these packages:

- `topo`
- `topo-seer`
- `topo-plugin-sdk`
- `topo-plugin-example`
- `topo-plugin-civ-example`

Example plugin packages are included because their libraries expose public
fixture modules used by SDK, manifest, and protocol coverage.
