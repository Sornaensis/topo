# Changelog

All notable user-facing changes for Topo are summarized here. Current guides
and contract publications are organized from the
[documentation index](docs/README.md).

## 1.0.0 — 2026-06-29

### Release packages

- Set the Stack workspace packages to Cabal version `1.0.0.0`:
  `topo`, `topo-seer`, `topo-plugin-sdk`, `topo-plugin-example`, and
  `topo-plugin-civ-example`.
- Published the HTTP/OpenAPI contract as API version `1.0.0`; `GET /version`
  reports `api_version=1` and the `topo-seer` package version `1.0.0.0`.

### Public automation

- Standardized direct HTTP/OpenAPI served by `topo-seer` as the only public 1.0
  automation surface.
- Removed the transitional generic command-dispatch routes; unknown paths now
  return ordinary `404` responses and cannot be enabled by configuration.
- Added published OpenAPI drift checks so
  [`docs/operator/openapi.json`](docs/operator/openapi.json) stays in sync with
  route metadata, schemas, examples, authentication, errors, and versioning.

### Plugin contract

- Standardized manifest v3 and RPC protocol v4 for external plugins.
- Documented production plugin transports using host-created named pipes or Unix
  domain sockets with length-prefixed JSON envelopes.
- Added backend-neutral external data-source declarations, grants, revocations,
  provider status, and diagnostics without making a storage backend part of the
  Topo core contract.

### Runtime and docs

- Documented the `topo-seer` SDL/headless runtime modes and the loopback/token
  policy for the HTTP server.
- Added [1.0 migration guidance](docs/migration/1.0.md) for clients moving to
  resource-oriented HTTP/OpenAPI and layered views.
- Published audience guides for users, operators, Haskell integrators, plugin
  authors, and contributors.
