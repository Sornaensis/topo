# RPC Client

> **Module:** `Topo.Plugin.RPC` (688 LOC)
> **Status:** Stub

## Overview

Host-side RPC client for communicating with plugin processes.
Manages connections and translates RPC calls into `PipelineStage`
and `SimNode` values.

## Key Types

- **`RPCConnection`** — handle to a connected plugin process

## Key Functions

| Function | Purpose |
|----------|---------|
| `connectPlugin` | Launch and connect to a plugin |
| `rpcGeneratorStage` | Create a `PipelineStage` backed by RPC |
| `rpcSimNode` | Create a `SimNode` backed by RPC |
| `shutdownPlugin` | Clean shutdown |
