# Getting started with topo-seer

`topo-seer` is the SDL application and HTTP host for generating, viewing, and
operating Topo worlds.

## Prerequisites

Run the commands below from the repository root. You need:

- [Haskell Stack](https://docs.haskellstack.org/); the repository pins its
  compiler and Haskell packages through `stack.yaml`.
- Native SDL2 and SDL2_ttf libraries usable by the compiler and runtime when
  launching the graphical application. Stack does not install those native
  libraries for the host system.

## Build and test

```console
stack build
stack test
```

Both commands cover all local packages. To build only the application package,
use `stack build topo-seer`.

## Launch modes

### SDL

```console
stack run topo-seer
```

This opens the fullscreen-desktop **Topo Seer** window. SDL mode owns the
renderer, discovers plugins, and does not start HTTP unless requested.

### SDL with HTTP

```console
stack run topo-seer -- --http 127.0.0.1:7373
```

The equivalent single-argument form is `--http=127.0.0.1:7373`. Check the
server with `GET /health` and discover the live API at `GET /openapi.json`.

### Headless HTTP

```console
stack run topo-seer -- --headless --http 127.0.0.1:7373
```

Headless mode starts no window, renderer, or render loop, and it requires an
HTTP binding. The command-line headless runtime does not scan the user plugin
directory. Renderer-dependent operations, notably screenshots, are unavailable.

Unknown options and malformed `HOST:PORT` bindings stop startup. Ports must be
between 1 and 65535.

## Bind and authentication policy

Loopback bindings may run without authentication. A non-loopback binding
requires a nonempty bearer token:

```console
stack run topo-seer -- --headless --http 0.0.0.0:7373 --http-token YOUR_TOKEN
```

`--http-token=YOUR_TOKEN` is also accepted. When a token is configured, send
`Authorization: Bearer YOUR_TOKEN` on every request except `GET /health`.
Supplying a token without `--http` enables the default
`127.0.0.1:7373` binding, but an explicit binding is clearer.

## Runtime configuration

On startup, topo-seer reads `~/.topo/config.json`. Every field is optional;
missing fields use defaults and numeric settings are clamped to their supported
minimums. If the file is absent, topo-seer creates `~/.topo/` and a
`README.txt` describing the current fields. Invalid JSON is reported and the
runtime uses defaults.

Next:

- Follow the [topo-seer workflow guide](topo-seer.md).
- See the [1.0 migration guide](../migration/1.0.md) when updating an older
  installation or client.
- See the [operator guide](../operator/README.md) for the full HTTP contract.
- See the [integrator](../integrator/README.md) and
  [plugin-author](../plugin/README.md) guides for library and plugin details.
