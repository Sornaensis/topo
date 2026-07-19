# Operating the topo-seer HTTP API

This guide describes the topo-seer 1.0 HTTP host for operators and API
clients. SDL and headless modes expose the same AppService contract through the
same resource-oriented route table. External automation uses HTTP and its
OpenAPI description: there is no command socket and no `/commands/<method>` (or
generic `/commands`) surface.

The live document served by `GET /openapi.json` is the canonical discovery
source for the running process. The colocated [openapi.json](openapi.json) is a
generated publication and review mirror, encoded from the same canonical
Haskell value, `openApiDocument publicHttpRouteSpecs`; do not prefer the
committed copy over live discovery.

## Start a host

SDL mode owns the renderer and starts HTTP when either `--http` or
`--http-token` is supplied:

```console
stack run topo-seer -- --http 127.0.0.1:7373
```

Headless mode starts the actor-backed AppService without a window, SDL
renderer, or render loop, and requires an HTTP binding:

```console
stack run topo-seer -- --headless --http 127.0.0.1:7373
```

`--http=HOST:PORT` is equivalent to the two-argument form. Ports must be in the
range 1–65535. `--test-mode` makes the headless startup use deterministic
default runtime configuration instead of loading the user configuration; it is
intended for tests. Unknown flags, malformed bindings, and headless startup without either
`--http` or `--http-token` fail startup.

The HTTP defaults are `127.0.0.1:7373` and an 8 MiB (8,388,608-byte) maximum
request body. Supplying `--http-token TOKEN` or `--http-token=TOKEN` before or
after the bind configures bearer authentication. A token supplied without
`--http` also enables the default binding, though an explicit bind is clearer.

### Bind and authentication policy

Loopback hosts (`localhost`, IPv4 `127/8`, and IPv6 loopback) may start without
a token. Any non-loopback bind requires a nonempty token and fails startup
without one. When configured, the token protects every known route except
`GET /health`:

```console
stack run topo-seer -- \
  --headless --http 0.0.0.0:7373 --http-token YOUR_TOKEN

curl -H "Authorization: Bearer YOUR_TOKEN" \
  http://127.0.0.1:7373/version
```

Bearer matching is exact. `GET /health` remains unauthenticated so a supervisor
can test process readiness. The OpenAPI operations carry `bearerAuth` security
metadata except for that health operation.

## Discover and call the API

Start with these metadata resources:

```console
curl http://127.0.0.1:7373/health
curl -H "Authorization: Bearer YOUR_TOKEN" \
  http://127.0.0.1:7373/version
curl -H "Authorization: Bearer YOUR_TOKEN" \
  http://127.0.0.1:7373/openapi.json
```

`GET /version` reports the package version and `api_version: "1"`. OpenAPI
defines each current method and path, query parameter, body policy, named
request and response schema, authentication requirement, and error response.
Clients should derive route and schema knowledge from it rather than from a
manually copied operation inventory.

The final canonical resources include `/overlays*`, `/camera*`, `GET /plugins`,
`GET /state/views`, and `POST /ui/view`. Removed single-view and route aliases
are not accepted. See the [1.0 migration table](../migration/1.0.md) when
updating an older client; it records removed names without presenting them as
live API.

## Request rules

- A nonempty JSON body on a body-bearing route requires
  `Content-Type: application/json` (media-type parameters are accepted).
- Required and optional JSON bodies, when present, must be JSON objects. A
  required body cannot be omitted.
- Routes declared without a body reject a nonempty body.
- The configured body limit is checked for fixed-length and chunked requests;
  an excess returns HTTP 413 `payload_too_large`.
- Required query parameters must have a value. Declared parameters cannot be
  repeated. Integer and boolean query values are validated before dispatch.
- The OpenAPI request schema is authoritative for fields and enums. Unknown
  routes and method/path combinations return 404 rather than falling through
  to a generic command dispatcher.

For example, a JSON mutation with authentication and correlation is:

```console
curl -X POST http://127.0.0.1:7373/ui/view \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -H "X-Request-Id: client-42" \
  --data '{"base_mode":"biome","overlay_mode":"cloud","weather_basis":"current"}'
```

### Errors and request IDs

Failures use one JSON envelope:

```json
{
  "error": {
    "code": "validation_failed",
    "message": "validation failed",
    "request_id": "client-42",
    "details": [
      {"path": ["name"], "code": "missing_field", "message": "missing required field 'name'"}
    ]
  }
}
```

`details` is always an array; detail `path`, `code`, and `message` fields depend
on the failure. If the request supplies `X-Request-Id`, topo-seer echoes it in
the response header and adds it to an error envelope. The server does not
invent an ID when none is supplied.

Depending on the operation and failure, clients can receive validation and
request-policy errors, authentication/permission errors, missing-resource or
unsupported-operation errors, conflicts, internal failures, runtime
unavailability, or data-resource timeouts. Use the HTTP status and stable
`error.code`; the live OpenAPI document lists the declared statuses and shared
`ErrorEnvelope` codes.

## Events and Server-Sent Events

`GET /events` is protected when a token is configured. A normal request returns
buffered service events as JSON with `mode: "polling"`. Request a Server-Sent
Events response with either `Accept: text/event-stream` or `?stream=true`:

```console
curl -N http://127.0.0.1:7373/events?stream=true \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Accept: text/event-stream"
```

The stream first emits the buffered snapshot and then live events. Events use
the service topic as the SSE `event`, the sequence as `id` when available, and
a JSON `ServiceEventEnvelope` as `data`. `limit=N` emits at most N buffered
events and closes; without a limit the connection remains live. Responses
disable intermediary buffering and caching. If the runtime has no event bus,
the stream returns an explanatory SSE comment and closes.

## Runtime-specific availability

Most operations have the same AppService behavior in SDL and headless modes.
Runtime resources still determine whether an operation can complete:

- SDL mode can service `POST /screenshots` through the active renderer. The
  optional object body may contain a safe, nonempty, sandbox-relative `.png`
  `path`. Success returns base64 PNG data with `format: "png"`,
  `source: "renderer"`, and a nullable `saved_path`.
- Headless mode has no renderer. A valid screenshot request returns HTTP 503
  with code `unavailable` and message
  `screenshot capture requires the SDL renderer`; it does not manufacture
  image bytes or write a file.
- The command-line headless runtime does not discover user plugins, so plugin
  and plugin-data results reflect what that runtime actually loaded.

Clients intended for both modes must handle operation-level 503 responses
rather than inferring availability only from route presence.

## Current persistence and schema semantics

The OpenAPI components include the final contracts for these behaviors:

- `DELETE /worlds` requires `{"name":"..."}`. It deletes the named committed
  saved-world bundle but does not replace the current in-memory world or delete
  provider-owned external data.
- `GET /presets` distinguishes built-in and user entries with `source` and
  `read_only`; load a catalogue ID or user name through `POST /presets/load`.
  Built-ins are immutable. Saving through `POST /presets` creates a user
  preset.
- `POST /screenshots` has an optional object body and renderer-only success
  schema as described above.
- Terrain export fields use basis-qualified names such as
  `climate_temp_avg`, `weather_temp_current`, and
  `weather_temp_typical`. Read the service's `available_fields` rather than
  sending retired ambiguous aliases.

Persistence names are logical basenames, not filesystem paths. Consult the
[user workflow guide](../user/topo-seer.md) for world and preset behavior and
the [1.0 migration guide](../migration/1.0.md) for removed aliases.

## API version and publication

The HTTP API version is independent of the package version. Version 1 may gain
additive routes, fields, enum values, and schemas. Breaking route, request,
response, authentication, or error-envelope changes require a new major API
version or a documented migration window.

This committed `openapi.json` is generated as compact JSON with Aeson from
`openApiDocument publicHttpRouteSpecs`. The HTTP specification tests compare
its bytes with that expression and also lock runtime, public, friendly, and
OpenAPI route signatures together. Regenerate the mirror from the Haskell value
after route or schema changes; never hand-edit or copy it from retired
documentation.
