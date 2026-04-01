# Format Specifications

On-disk format references for `.topo`, `.topolay`, `.toposchema`, and
the unified world-bundle directory layout.

## Contents

| Document | Description |
|----------|-------------|
| [World Bundle Format](world-bundle-format.md) | Directory layout and unified persistence contract |
| [.topo Binary Format](topo-binary-format.md) | Core terrain file binary layout |
| [.topolay Binary Format](topolay-format.md) | Overlay payload binary layout |
| [.toposchema JSON Format](toposchema-format.md) | Overlay schema JSON structure |

## Version History

| Format | Current Version | Module |
|--------|----------------|--------|
| `.topo` | 20 | `Topo.Storage` |
| `.topolay` | (header-versioned) | `Topo.Overlay.Storage` |
| `.toposchema` | (schema `version` field) | `Topo.Overlay.Schema` |
