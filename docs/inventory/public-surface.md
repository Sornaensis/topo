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
| Core model, coordinates, metadata, configuration, units, and deterministic helpers | Present | topo core maintainers | Existing topo-seer state/config/terrain surfaces; M9 expands inspector coverage. | M2 AppService state/config/terrain; M3 state/config/terrain routes. | Covered by existing topo core specs. |
| Pipeline, world generation, tectonics, terrain forms, erosion, and volcanism | Partial | topo generation maintainers | Existing generation controls and view modes; M9 adds diagnostics and inspector sections. | M2 generation/pipeline services; M3 generation, pipeline, diagnostics, and terrain routes. | Covered by existing generation/pipeline specs; additional parity tests planned. |
| Hydrology, rivers, water bodies, water table, glaciers, and ocean currents | Partial | topo domain maintainers | Existing river/water rendering; M9 adds inspector and view-mode metadata coverage. | M2 terrain inspector/query services; M3 terrain feature routes. | Covered by existing hydrology/water specs; surface parity tests planned. |
| Climate, weather, biome refinement, soil, and vegetation | Partial | topo domain maintainers | Existing climate/weather/biome views; M9 adds legends, units, timeline/status, and inspector sections. | M2 domain query services; M3 terrain feature routes. | Covered by existing climate/weather/biome/ecology specs; surface parity tests planned. |
| Overlays, schema/provenance/indexing, storage, world bundles, mesh, and export | Partial | topo persistence and overlay maintainers | Existing overlay selection/rendering and persistence actions; M9 adds manager, provenance, and import/export dialogs. | M2 overlay/persistence/export services; M3 overlay, save/load, provenance, and export routes. | Covered by existing overlay/persistence/export specs; golden/API tests planned. |
| Simulation and simulation DAG | Partial | topo simulation maintainers | Existing simulation controls; M9 adds DAG/status/tick logs and plugin node visibility. | M2 simulation services; M3 simulation routes. | Existing simulation specs; DAG/status parity tests planned. |
| Plugin capability, manifest, RPC, transport, payload, and data-resource contracts | Planned | topo plugin maintainers | Plugin status/resource/dependency/external data-source UI planned through M5-M9. | M2 plugin services; M3 plugin/resource routes; M5-M7 protocol, manifest, CRUD, and backend-neutral external data-source contracts. | Existing plugin/data-resource specs; v3 transport/manifest/CRUD tests planned. |
| Command DTO compatibility types | Compatibility | topo-seer service maintainers | Used indirectly by internal/test command paths. | M2 maps commands to AppService; M3 uses topo-api DTOs; M4 removes MCP dependence. Not a public 1.0 automation surface. | Existing command specs. |
| Topo-seer actor runtime, workers, plugin manager, logging, snapshots, and UI state actor | Partial | topo-seer runtime maintainers | Runtime-backed SDL status; M8 reduces direct coupling through AppService/reducers. | M2 actor adapters; M3 status/events/logs/plugin/generation routes. | Existing actor/runtime specs; service/API parity tests planned. |
| Command IPC, dispatch, and UI action compatibility adapters | Compatibility | topo-seer service maintainers | Current UI and internal/test command IPC compatibility path. | M2 makes this an AppService adapter; M3/M4 replace public automation with HTTP/OpenAPI. Legacy command IPC has no public 1.0 exception. | Existing command and HTTP route parity tests. |
| Application boot, runtime config, snapshots, persistence, screenshots, system wiring, and timing | Partial | topo-seer runtime maintainers | Existing app/config/save-load/screenshot paths; M8 splits `Seer.System`. | M2 runtime/config/persistence services; M3 health/state/config/world/screenshot routes. | Existing config/persistence specs; API tests planned. |
| SDL UI drawing, input routing, editor, renderer, widgets, layout, theme, terrain atlas/cache, and view rendering | Partial | topo-seer UI maintainers | Primary SDL UI; M8 componentizes panels/reducers/layout/draw commands and M9 closes feature gaps. | M2/M8 route mutating actions through AppService; M3 exposes state/actions/metadata rather than SDL internals. | Existing UI/editor/render specs plus view-mode registry golden/API parity tests; broader component and API parity tests planned. |
| Plugin SDK declarations, payload helpers, terrain/overlay helpers, and runner | Planned | topo plugin maintainers | Developer-facing SDK; runtime declarations surface through plugin UI after M5-M9. | SDK feeds manifest/protocol/resource services; M6 makes manifest v3 generation canonical. | Existing SDK payload/runner specs; manifest v3 golden tests planned. |

## Package coverage summary

The inventory currently covers exposed library modules from these packages:

- `topo`
- `topo-seer`
- `topo-plugin-sdk`
- `topo-plugin-example`
- `topo-plugin-civ-example`

Example plugin packages are included because their libraries expose public
fixture modules used by SDK, manifest, and protocol coverage.
