# topo world generator library

- `topo`: library for large terrain data generation and manipulation
- `topo-seer`: SDL application for interacting with the generator

## Quick start

- Build all packages: `stack build`
- Run tests: `stack test`
- Run the app: `stack exec topo-seer`

## SDL2 dependency

The `topo-seer` package depends on SDL2 and requires SDL2 development libraries
and `pkg-config` to be available on your system.

## Hyperspace

topo-seer is implemented with [Hyperspace](https://github.com/Sornaensis/hyperspace), an Actor library; you can [read the documentation here](https://github.com/Sornaensis/hyperspace/blob/master/docs/contents.md).