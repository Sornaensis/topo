# Topo Documentation

Topo is a library for large-scale procedural terrain generation with plate
tectonics, erosion, hydrology, climate simulation, biome classification,
and an extensible overlay/plugin system.

## Documentation Sections

### [Internal Development Guide](internal/index.md)

Architecture, module reference, and contributor documentation for the
`topo` core library. Covers all 13 domain groups: types, pipeline,
terrain generation, hydrology, climate, weather, biomes, ecology,
overlays, simulation, persistence, plugins, and units.

### [Plugin Developer Guide](plugin-dev/index.md)

Everything an external plugin author needs: quick-start tutorial, SDK
reference, overlay schema authoring, manifest format, RPC protocol,
and worked examples.

### [Format Specifications](specs/index.md)

Machine-readable and human-readable specs for the on-disk formats:
`.topo` binary world files, `.topolay` overlay payloads,
`.toposchema` overlay schemas, and the unified world-bundle layout.

### HTTP/OpenAPI automation

For 1.0-facing automation, run `topo-seer` with direct HTTP enabled and use the
served OpenAPI document as the operation catalog:

```sh
stack exec topo-seer -- --headless --http 127.0.0.1:7373
curl http://127.0.0.1:7373/health
curl http://127.0.0.1:7373/openapi.json
curl http://127.0.0.1:7373/state
curl -X POST http://127.0.0.1:7373/ui/seed \
  -H 'Content-Type: application/json' \
  -d '{"seed":123}'
```

The retired MCP bridge and legacy command IPC appear only in migration/internal
compatibility notes. New clients should use the resource-oriented HTTP routes in
OpenAPI only; there is no public 1.0 command IPC exception. See the
[MCP-to-HTTP parity matrix](inventory/mcp-http-parity.md) when translating old
MCP tool or resource names.

---

## Quick Links

| Topic | Link |
|-------|------|
| 1.0 readiness roadmap | [roadmap/1.0.md](roadmap/1.0.md) |
| Architecture overview | [internal/architecture.md](internal/architecture.md) |
| Current architecture baseline | [architecture/current.md](architecture/current.md) |
| Target architecture for 1.0 | [architecture/target.md](architecture/target.md) |
| HTTP framework/API package decision | [architecture/decisions/0001-http-framework-and-api-package.md](architecture/decisions/0001-http-framework-and-api-package.md) |
| HTTP/OpenAPI migration matrix | [inventory/mcp-http-parity.md](inventory/mcp-http-parity.md) |
| Plugin protocol versioning decision | [architecture/decisions/0002-plugin-protocol-versioning.md](architecture/decisions/0002-plugin-protocol-versioning.md) |
| Public module and feature inventory | [inventory/public-surface.md](inventory/public-surface.md) |
| Pipeline stages reference | [internal/pipeline/stages.md](internal/pipeline/stages.md) |
| Core types reference | [internal/core/types.md](internal/core/types.md) |
| World generation presets | [internal/pipeline/world-gen.md](internal/pipeline/world-gen.md) |
| Overlay system | [internal/overlay/overview.md](internal/overlay/overview.md) |
| Plugin quick start | [plugin-dev/getting-started.md](plugin-dev/getting-started.md) |
| World bundle format | [specs/world-bundle-format.md](specs/world-bundle-format.md) |

## Repository Layout

```
topo/               Core terrain generation library (this documentation)
topo-seer/          SDL2 graphical application and HTTP/OpenAPI host
topo-api/           Planned standalone HTTP/OpenAPI contract package for 1.0
topo-plugin-sdk/    Plugin development SDK
topo-plugin-example/    Minimal plugin example
topo-plugin-civ-example/  Civilization overlay plugin example
```
