# SDK Types Reference

> **Status:** Stub — to be expanded from existing `plugins.md`

## PluginDef

Top-level plugin definition. Use `defaultPluginDef` as a starting point.

| Field | Type | Description |
|-------|------|-------------|
| `pdName` | `Text` | Unique plugin identifier |
| `pdVersion` | `Text` | Version string |
| `pdParams` | `[ParamDef]` | User-facing parameters |
| `pdSchemaFile` | `Maybe FilePath` | Overlay schema file |
| `pdGenerator` | `Maybe GeneratorDef` | Generator participation |
| `pdSimulation` | `Maybe SimulationDef` | Simulation participation |

## ParamDef

<!-- TODO: Full reference -->

## GeneratorDef

<!-- TODO: Full reference -->

## SimulationDef

<!-- TODO: Full reference -->

## PluginContext

Runtime context provided to callbacks.

| Field | Type | Description |
|-------|------|-------------|
| `pcWorld` | `TerrainWorld` | Current terrain state |
| `pcParams` | `Map Text Value` | Current parameter values |
| `pcSeed` | `Word64` | World generation seed |
| `pcLog` | `Text -> IO ()` | Log to host |
