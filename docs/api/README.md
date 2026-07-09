# Topo HTTP/OpenAPI contract

The published OpenAPI artifact is [`openapi.json`](openapi.json). It is generated from `topo-seer`'s public route metadata (`publicHttpRouteSpecs`) through `Seer.HTTP.OpenAPI.openApiDocument`; `GET /openapi.json` serves the same contract at runtime.

## Running the API

```sh
stack exec topo-seer -- --headless --http 127.0.0.1:7373
curl http://127.0.0.1:7373/openapi.json
```

Loopback binds are intended for local automation. Non-loopback binds require `--http-token TOKEN`, and protected routes require `Authorization: Bearer TOKEN`. `GET /health` remains unauthenticated for readiness checks.

## Contract contents

- **Endpoint tags:** `meta`, `events`, `state`, `ui`, `config`, `presets`, `world`, `terrain`, `overlays`, `editor`, `pipeline`, `plugins`, `data`, `simulation`, `logs`, `screenshots`, and `camera` group operations in the OpenAPI `tags` section.
- **Schemas:** JSON request and response schemas are published under `components.schemas`; operations reference them by `$ref` from request and response content.
- **Examples:** The spec includes representative JSON examples for health, version, seed mutation, data-resource creation, and error envelopes. Additional usage examples are below.
- **Errors:** Error responses use `ErrorEnvelope` and may include `invalid_json`, `invalid_request`, `validation_failed`, auth/not-found/rejected/unavailable/internal codes, and standardized data-resource codes such as `schema_validation_failed`, `permission_denied`, `operation_not_supported`, and `timeout`. Responses echo `X-Request-Id` when supplied.
- **Versioning:** `info.version` is the public HTTP API version (`1.0.0`), while `GET /version` exposes `api_version=1` plus the application package version. API major version 1 permits additive routes, fields, enum values, and schemas; breaking route, payload, auth, or error-envelope changes require a new major API version or documented migration window.

## Layered view migration

View selection is now layered: `view.base_mode` selects the terrain/base atlas,
`view.overlay_mode` optionally selects weather, sky, or plugin overlay data, and
`view.weather_basis` chooses average/typical versus current weather sources.
Prefer `GET /state/views` and `POST /ui/view` for new clients. The legacy
`view_mode`, `legacy_view_mode`, `GET /state/view-modes`, and
`POST /ui/view-mode` surfaces remain compatibility aliases for API v1 but lose
layered information such as a biome base with a cloud overlay.

See the [layered view state migration guide](../migration/layered-view-state.md)
for route mapping, legacy name mapping, source/basis semantics, persistence
behavior, and deprecation timing.

## Simulation and plugin declaration terminology

`GET /simulation/dag` distinguishes `nodes` (actor-bound DAG nodes) from plugin
simulation declaration diagnostics. Built-in weather remains an actor-bound node
with `kind: "builtin"` and `plugin: null`. The legacy `plugin_nodes` field is a
backward-compatible alias for plugin simulation declarations; prefer
`plugin_simulation_declarations` for new clients and use `bound`/`actor_bound`
versus `plan_executable`/`eligible_for_binding` to distinguish current actor
binding from plan eligibility.

Plugin list responses use `has_simulation`/`simulation` as compatibility aliases
for `has_simulation_declaration`/`simulation_declaration`. These fields describe
a plugin manifest declaration only, not the host built-in weather simulation.
`simulation.dependencies` entries are simulation node IDs and may reference host
built-ins such as `weather`.

## Examples

```sh
# Read application state.
curl http://127.0.0.1:7373/state

# Query one terrain hex.
curl 'http://127.0.0.1:7373/terrain/hex?q=0&r=0'

# Mutate UI seed.
curl -X POST http://127.0.0.1:7373/ui/seed \
  -H 'Content-Type: application/json' \
  -d '{"seed":123}'

# Create a plugin-owned data record.
curl -X POST http://127.0.0.1:7373/data/records \
  -H 'Content-Type: application/json' \
  -d '{"plugin":"civ","resource":"settlements","fields":{"name":"Riverford","population":1200}}'

# Authenticated request when a token is configured.
curl http://127.0.0.1:7373/state \
  -H 'Authorization: Bearer TOKEN'
```

The `topo-seer-test` OpenAPI drift checks compare `docs/api/openapi.json` to the generated route contract so the committed documentation fails tests when routes, schemas, examples, auth, error metadata, or versioning metadata drift.
