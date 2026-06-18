# topo world generator library

- `topo`: library for large terrain data generation and manipulation
- `topo-seer`: SDL application and direct HTTP/OpenAPI host for interacting with the generator
- `topo-plugin-sdk`: helpers for external topo plugins
- `topo-plugin-example` / `topo-plugin-civ-example`: fixture plugins

## Quick start

- Build all packages: `stack build`
- Run tests: `stack test`
- Run the app: `stack exec topo-seer`
- Run the Writ worldbuilding smoke workflow:
  `stack test topo:test:topo-test --test-arguments "--match Writ"`

## Runtime dependencies

`topo-seer` is the only SDL executable. It depends on SDL2, SDL2_ttf, and
Hyperspace through the Git extra-deps pinned in `stack.yaml`; the native SDL
runtime libraries still need to be available on systems that launch the app.
The core `topo` library, HTTP headless mode, plugin SDK, and plugin examples can
be built and tested without starting the SDL UI.

## Hyperspace

topo-seer uses [Hyperspace](https://github.com/Sornaensis/hyperspace) for its
actor runtime.
