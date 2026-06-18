# ADR 0001: HTTP Framework and API Package Structure

- **Status:** Amended by HTTP MVP implementation
- **Date:** 2026-05-14
- **Decision owners:** Topo maintainers

## Context

Topo 1.0 needs direct `topo-seer` HTTP access with generated OpenAPI. The
previous public automation path went through a retired MCP bridge and topo-seer
command IPC. Direct HTTP/OpenAPI replaces that bridge for 1.0.

The HTTP implementation must support:

- typed route definitions;
- generated OpenAPI with drift tests;
- JSON request/response schemas shared with tests and client tooling;
- loopback-by-default runtime behavior with optional auth;
- headless CI execution;
- route coverage for the same behavior exposed through AppService;
- isolation from SDL dependencies where possible, so API contracts and client
  schemas can be compiled/tested without pulling in the graphical application.

`topo-seer` depends on SDL2, SDL2_ttf, Hyperspace, rendering, process,
runtime packages, and the WAI/Warp/http-types dependencies used by the HTTP MVP.

## Decision

The HTTP MVP uses **WAI/Warp** in `topo-seer` with a typed route metadata table
as the single source for request dispatch, handler coverage tests, and the
served OpenAPI document. This keeps the Writ/HTTP gate small enough to remove
the retired bridge while still routing behavior through AppService.

A future hardening pass may still introduce a small sibling package named
**`topo-api`** for Servant route types, DTOs, schemas, OpenAPI generation, and
API-version metadata once the MVP surface is stable. In that target split,
`topo-seer` will depend on `topo-api` and provide the server implementation by
mapping route handlers to AppService.

Potential package split:

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

- MVP tests compile in `topo-seer` and verify route metadata, OpenAPI path/query
  coverage, auth policy, AppService dispatch, error envelopes, and headless CLI
  HTTP smoke behavior.
- A future `topo-api` split can move schema/route contract tests out of the SDL
  application package without changing the AppService server mapping.

### SDL dependency isolation

- The MVP keeps API route metadata inside `topo-seer`, so client generation and
  docs tooling still compile the application package.
- The future `topo-api` split remains the path for isolating DTO/OpenAPI
  compilation from SDL2/SDL2_ttf.
- Headless HTTP tests exercise `topo-seer` without creating a renderer.

### Client generation and public contract stability

- The MVP OpenAPI document is generated from the same route metadata table used
  by dispatch tests.
- Future clients can be generated from the served or committed OpenAPI artifact.
- A future `topo-api`/Servant pass can strengthen schema generation and golden
  diffs once the MVP route set has settled.

### OpenAPI drift prevention

The HTTP milestone must add tests that fail when:

- an implemented route is missing from OpenAPI;
- a route in the metadata table has no topo-seer handler;
- public error shapes diverge from the documented error envelope.

Future contract hardening should add committed OpenAPI golden diffs and schema
checks for enum/runtime codec drift.

### Dependency management

The HTTP MVP adds `wai`, `warp`, `http-types`, `scientific`, and HTTP smoke-test
client dependencies. A future Servant/topo-api pass should add only the minimal
additional route/schema/OpenAPI packages it needs.

## Rejected alternatives

### Hand-written WAI/Warp router plus manual OpenAPI as final architecture

The MVP accepts a WAI/Warp route metadata table to keep the Writ gate small. It
should not become the final architecture if schema/golden drift protection needs
a stronger generated-contract package.

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
