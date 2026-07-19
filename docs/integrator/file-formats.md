# Topo world and overlay file formats

The current persistence boundary is a `.topo` world plus a sibling `.topolay/`
directory. Bundle saves create the directory even when the overlay store is
empty. Use `Topo.Persistence.WorldBundle` for coordinated saves and loads. The
lower-level codecs remain useful for exact interchange and chunk loading.

## Raw `.topo` world

`Topo.Storage` writes a binary file beginning with ASCII `TOPO`, followed by a
little-endian `Word32` format version. The current version is **23**, and the
decoder accepts version 23 only.

The version-23 body is ordered as follows:

1. chunk size (`Word32le`) and hex size in kilometres (`Float32le`);
2. world provenance, its terrain/climate/biome map provenance, and metadata;
3. planet configuration (three `Float32le` values) and world slice (four
   `Float32le` values);
4. world time (`Word64le` tick and `Float64le` tick rate), world seed
   (`Word64le`), and planet age (`Float64le`);
5. generation configuration as `Word32le length + JSON bytes` (zero length is
   absent), followed by ten `Float32le` unit-scale values;
6. a counted list of length-prefixed UTF-8 overlay names;
7. counted chunk maps in this order: terrain, climate, rivers, groundwater,
   glaciers, volcanism, water bodies, and vegetation.

Each chunk-map entry is `Word32le chunkId`, `Word32le payloadLength`, then the
corresponding `Topo.Export` chunk payload. Those payload decoders enforce the
configured tile count and their exact structural length. A raw `.topo` stores
the overlay-name manifest, not overlay contents; decoding therefore starts
with an empty overlay store.

`encodeWorld`/`decodeWorld` and their provenance variants are the in-memory
contract. `saveWorld`/`loadWorld` are deliberately lower level than the bundle
API.

## `.toposchema` JSON

`Topo.Overlay.Schema` defines the schema beside each overlay. Its JSON object
contains:

- `name`, `version`, `description`, and `storage` (`"sparse"` or `"dense"`);
- ordered `fields`;
- `dependencies` with `terrain` and `overlays`.

A field requires `name`, `type`, and `default`. `indexed: true` and
`renamed_from` are optional. Scalar types are `"float"`, `"int"`, `"bool"`,
and `"text"`; a list is `{ "list": <scalar-type> }`. Missing description,
storage, dependencies, or indexed state default respectively to `""`, sparse,
no dependencies, and false.

Validation rejects empty schema names or versions, empty/duplicate fields,
nested lists, and text or list fields in a dense schema. The schema `version`
is overlay-owned compatibility text, not a world or bundle format number.

## `.topolay/` sidecar and `.topolay` data

For `world.topo`, `overlayDirPath` is `world.topolay/`. Each manifest overlay
`<name>` has:

- `<name>.toposchema` — the JSON schema;
- `<name>.topolay` — binary records and provenance.

The current binary writer emits:

1. FNV-1a overlay-name hash (`Word32le`);
2. schema-version UTF-8 length and bytes;
3. storage mode (`Word8`: sparse `0`, dense `1`);
4. field count, then each length-prefixed field name and a type byte (`0`
   float, `1` int, `2` bool, `3` text, `4` list); a list has one additional
   element-type byte;
5. provenance seed (`Word64le`), version (`Word32le`), and length-prefixed
   source text;
6. a flags byte: chunk index `0x01`, zstd `0x02`, schedule `0x04`;
7. when scheduled: interval, phase, optional last-fire tick, next-fire tick,
   and catch-up-policy tag;
8. chunk count and entries; each entry has chunk ID, stored length, optional
   uncompressed length when zstd is set, then its payload;
9. when indexed: a count followed by `(chunkId Word32le, offset Word64le)`
   entries.

Current writers always include the chunk index. Default storage is
uncompressed; `CompressionZstd` uses one zstd frame per chunk and verifies the
recorded uncompressed length. Readers also accept older sequential files that
lack an index. Sparse payloads store schema-ordered records; dense payloads
store one `Float32le` vector per field. There is no separate numbered
`.topolay` format version: schema-version text and supported flags are the
compatibility boundary.

`loadOverlayChunk` reads one sparse chunk through either the index or a scan,
including zstd decoding. Missing chunk IDs produce an empty sparse chunk;
negative IDs and dense data fail.

## World bundles and directory replacement

A world bundle has no additional wrapper or version field. Saving normalizes
the `.topo` overlay manifest from the current overlay store, writes the world,
all schema/data pairs, and any requested extra files into a fresh staging
directory, then commits the **entire containing directory**. Extra-file paths
must be contained, traversal-free relative paths; callers must enforce this
before invoking the current API. Put the `.topo` file in a dedicated bundle
directory.

For a target `.topo` path, `Topo.Persistence.WorldBundle` uses:

- target: `takeDirectory topoPath`;
- staging: `target <> ".saving"`;
- backup: `target <> ".old"`.

If a target exists, it is renamed to the backup; staging is promoted by a
directory rename; the backup is then removed. If the post-backup step or
promotion fails, restoration of the backup is attempted. This contract does
not claim filesystem synchronization beyond those rename/restore operations.

`StrictManifest` requires every named overlay schema and data file, parses each
schema, and verifies that its name matches the manifest. Extra sidecar files
not named by the manifest are ignored. `BestEffort` returns the raw world when
schema or sidecar loading fails.

The maintained `Storage`, `Provenance`, `Overlay`, and `WorldBundle`
specifications exercise exact world/provenance round trips, indexed and zstd
chunks, corruption failures, schedules, strict-manifest behavior, extra-file
commits, backup cleanup, and restoration after an injected commit failure.
