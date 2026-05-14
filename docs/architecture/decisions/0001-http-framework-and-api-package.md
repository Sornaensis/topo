# ADR 0001: HTTP Framework and API Package Structure

- **Status:** Accepted for 1.0 planning
- **Date:** 2026-05-14
- **Decision owners:** Topo maintainers

## Context

Topo 1.0 needs direct `topo-seer` HTTP access with generated OpenAPI. The
current public automation path goes through `topo-mcp` and topo-seer command
IPC. That bridge should be retired after HTTP reaches feature parity.

The HTTP implementation must support:

- typed route definitions;
- generated OpenAPI with drift tests;
- JSON request/response schemas shared with tests and client tooling;
- loopback-by-default runtime behavior with optional auth;
- headless CI execution;
- route coverage for the same behavior exposed through AppService;
- isolation from SDL dependencies where possible, so API contracts and client
  schemas can be compiled/tested without pulling in the graphical application.

`topo-seer` currently depends on SDL2, SDL2_ttf, Hyperspace, rendering, process,
and runtime packages. It does not currently list WAI, Warp, Servant, or OpenAPI
dependencies.

## Decision

Use **Servant** for typed routes, **WAI/Warp** for the HTTP server, and
**servant-openapi3/openapi3** for generated OpenAPI.

Introduce a small sibling package named **`topo-api`** during the HTTP milestone.
The package should own public API route types, DTOs, error envelopes, OpenAPI
generation, and API-version metadata. `topo-seer` will depend on `topo-api` and
provide the server implementation by mapping route handlers to AppService.

Target package split:

```text
topo-api/
  src/Topo/API/Routes.hs      -- Servant API type
  src/Topo/API/Types.hs       -- DTOs and JSON/OpenAPI schemas
  src/Topo/API/Errors.hs      -- public error envelope and status mapping
  src/Topo/API/Events.hs      -- event payload schemas
  src/Topo/API/OpenAPI.hs     -- generated OpenAPI document

topo-seer/
  src/Seer/HTTP/Server.hs     -- WAI/Warp server over AppService
  src/Seer/HTTP/Auth.hs       -- token/bind-address policy
  src/Seer/HTTP/Handlers.hs   -- AppService handler mapping
```

`topo-api` may depend on `topo` for stable core types where those types are
appropriate public API contracts. It must not depend on `topo-seer`, SDL2,
SDL2_ttf, Hyperspace, renderer modules, or UI internals.

## Consequences

### Testing

- Contract tests can compile against `topo-api` without initializing SDL or the
  topo-seer runtime.
- OpenAPI generation and golden drift tests can run in a light-weight package.
- `topo-seer` integration tests still verify that every typed route has a
  handler and that handlers call AppService correctly.
- API and service tests can be separated: `topo-api` tests cover schemas/routes;
  `topo-seer` tests cover runtime behavior.

### SDL dependency isolation

- API DTOs and OpenAPI generation stay outside the SDL application package.
- Client generation and docs tooling do not need to pull in SDL2/SDL2_ttf.
- Headless HTTP tests can still exercise `topo-seer`, but route/schema tests do
  not require the render-thread environment.

### Client generation and public contract stability

- The OpenAPI document is generated from the Servant route type rather than
  maintained by hand.
- API version metadata lives with the route/schema package.
- Future clients can be generated from the committed/published OpenAPI artifact.
- Breaking API changes should be visible as OpenAPI golden diffs.

### OpenAPI drift prevention

The HTTP milestone must add tests that fail when:

- generated OpenAPI differs from the committed golden artifact;
- an implemented handler is missing from the route type;
- a route in the route type has no topo-seer handler;
- enum values in OpenAPI diverge from runtime codecs;
- public error shapes diverge from the documented error envelope.

### Dependency management

The HTTP milestone should add packages such as:

- `servant`
- `servant-server`
- `servant-openapi3`
- `wai`
- `warp`
- `openapi3`
- `http-types`
- `http-media`
- optional middleware packages only when needed, such as `wai-cors` or
  `wai-extra`

If any chosen package is unavailable in the active Stack snapshot, add the
minimal required `extra-deps` in the same implementation PR that introduces
`topo-api`.

## Rejected alternatives

### Hand-written WAI/Warp router plus manual OpenAPI

This keeps dependencies small but makes route/schema drift more likely. Every
route would need separate hand-maintained OpenAPI definitions and parity tests.
That works against the 1.0 requirement for generated public contracts.

### Scotty or another lightweight routing DSL

Lightweight routing is simple for early endpoints, but it does not provide the
same single typed route contract for server implementation, tests, and OpenAPI
generation.

### Put all API types inside `topo-seer`

This is simpler initially, but it couples API contract compilation to SDL/UI
dependencies and makes client generation/docs tooling heavier. It also makes it
easier for API DTOs to drift toward UI/runtime internals.

### Put HTTP route types in `topo`

The core library should remain the terrain/simulation/protocol core. HTTP is a
topo-seer runtime contract, not a core terrain dependency. A sibling `topo-api`
package keeps the boundary explicit without polluting `topo`.

## Implementation notes

- Add `topo-api` to `stack.yaml` during the HTTP milestone.
- Keep AppService in `topo-seer`; do not move runtime actor dependencies into
  `topo-api`.
- Start with `/health`, `/version`, `/openapi.json`, and `/state` before adding
  larger route groups.
- Keep all HTTP behavior loopback-only by default and require auth for any
  non-loopback binding.
- Do not expose MCP as a 1.0 public automation path in API docs.
- Keep backend-neutral external data-source DTOs abstract: provider ids,
  capability scopes, opaque references, health/status, and diagnostics. Do not
  encode SQLite or any other backend as the API model.
