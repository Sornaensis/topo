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

---

## Quick Links

| Topic | Link |
|-------|------|
| Architecture overview | [internal/architecture.md](internal/architecture.md) |
| Pipeline stages reference | [internal/pipeline/stages.md](internal/pipeline/stages.md) |
| Core types reference | [internal/core/types.md](internal/core/types.md) |
| World generation presets | [internal/pipeline/world-gen.md](internal/pipeline/world-gen.md) |
| Overlay system | [internal/overlay/overview.md](internal/overlay/overview.md) |
| Plugin quick start | [plugin-dev/getting-started.md](plugin-dev/getting-started.md) |
| World bundle format | [specs/world-bundle-format.md](specs/world-bundle-format.md) |

## Repository Layout

```
topo/               Core terrain generation library (this documentation)
topo-seer/          SDL2 graphical application
topo-plugin-sdk/    Plugin development SDK
topo-plugin-example/    Minimal plugin example
topo-plugin-civ-example/  Civilization overlay plugin example
```
