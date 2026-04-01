# .topolay Binary Format

> **Status:** Stub
> **Module:** `Topo.Overlay.Storage`

## Overview

Overlay payload binary files. Each overlay is stored as a single
`.topolay` file containing a header followed by chunk payloads.

## Header

In order:

1. `Word32LE` — overlay name hash (FNV-1a)
2. `Word32LE` — schema-version byte length
3. Schema-version bytes (UTF-8)
4. `Word8` — storage mode (`0x00` sparse, `0x01` dense)
5. `Word32LE` — field count
6. Repeated field descriptors:
   - `Word32LE` — field-name byte length
   - Field-name bytes (UTF-8)
   - `Word8` — field type tag
7. Provenance block:
   - `Word64LE` — `opSeed`
   - `Word32LE` — `opVersion`
   - `Word32LE` — `opSource` byte length
   - `opSource` bytes (UTF-8)
8. `Word8` — flags
   - Bit 0: chunk index table present (reserved)
   - Bit 1: zstd chunk compression (reserved)
9. `Word32LE` — chunk count

Unknown flags are rejected by readers.

## Chunk Payloads

For each chunk:

1. `Word32LE` — chunk ID
2. `Word32LE` — payload byte length
3. Payload bytes

Payload encoding depends on schema field order/types and storage mode.

## Field Type Tags

<!-- TODO: Document type tag values -->

## Compression

Reserved for future use. Currently no chunks are compressed.
