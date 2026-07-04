# Changelog

All notable user-facing changes for Topo are summarized here. Release-specific
packaging and tag instructions live in [docs/release.md](docs/release.md).

## 1.0.0 — 2026-06-29

### Release packages

- Set the Stack workspace packages to Cabal version `1.0.0.0`:
  `topo`, `topo-seer`, `topo-plugin-sdk`, `topo-plugin-example`, and
  `topo-plugin-civ-example`.
- Published the HTTP/OpenAPI contract as API version `1.0.0`; `GET /version`
  reports `api_version=1` and the `topo-seer` package version `1.0.0.0`.
- Removed `topo-mcp` from the 1.0 workspace and release artifact set.

### Public automation

- Standardized direct HTTP/OpenAPI served by `topo-seer` as the only public 1.0
  automation surface.
- Kept legacy command IPC and command-compatible HTTP routes internal/test-only;
  new automation clients should use resource-oriented HTTP routes.
- Added published OpenAPI drift checks so `docs/api/openapi.json` stays in sync
  with route metadata, schemas, examples, auth, errors, and versioning.

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
- Added migration guidance from pre-1.0 MCP/command IPC clients to HTTP/OpenAPI.
- Published release notes, plugin contract references, API docs, and reproducible
  release packaging instructions for the 1.0 artifact set.
