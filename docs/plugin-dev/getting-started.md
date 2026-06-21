# Getting Started

> **Status:** Stub — to be expanded from existing `plugins.md`

## Overview

A topo plugin is a standalone Haskell executable that communicates with
topo-seer over a host-created named pipe (Windows) or Unix domain socket
(Linux/macOS) advertised through launch environment variables. The
`topo-plugin-sdk` package handles all transport and protocol details —
you only define your plugin logic.

## Quick Start

### 1. Create a project

<!-- TODO: Migrate content from docs/plugins.md -->

### 2. Define your PluginDef

<!-- TODO: Minimal example -->

### 3. Build and install

```bash
stack install --local-bin-path ~/.topo/plugins/my-plugin/
```

### 4. Run topo-seer

topo-seer discovers plugins at `~/.topo/plugins/` automatically.

## Next Steps

- [Key Concepts](concepts.md) — understand the pipeline and overlay model
- [SDK Reference](sdk-reference.md) — full type reference
- [Overlay Schemas](overlay-schemas.md) — define custom data layers
