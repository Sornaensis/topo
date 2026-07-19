# Using topo-seer

This guide covers the normal SDL workflow and names the corresponding HTTP
resources where useful. Start with [Getting started](getting-started.md). For
request and response schemas, use the live `GET /openapi.json` document and the
[operator guide](../operator/README.md).

## Generate a world

Open the left **Topo** tab, choose **Chunk Size** and **Seed**, then select
**Generate**. **Random** replaces the seed before generation. A new SDL session
starts with a random seed and the world name **Untitled**.

HTTP generation uses the current UI seed and slider values. To choose a seed,
call `POST /ui/seed` with `{"seed":42}` first, then call
`POST /world/generate` with no body.

## Save, load, name, and delete worlds

Press Escape to open the **Save** and **Load** menu.

- **Save** opens **Save World**. Enter a logical name and confirm **Save**.
- **Load** opens **Load World**. The filterable list is newest first; choose a
  row and confirm **Load**.
- **Delete** in that list opens **Delete saved world?** and requires a second
  **Delete** confirmation.

A delete removes only the selected committed bundle under
`~/.topo/worlds/`. It does not change the currently loaded in-memory world and
does not remove provider-owned external data referenced by the bundle.

The matching HTTP resources are:

| Operation | Request |
|---|---|
| List | `GET /worlds` |
| Save | `POST /worlds/save` with `{"name":"my-world"}` |
| Load | `POST /worlds/load` with `{"name":"my-world"}` |
| Delete | `DELETE /worlds` with `{"name":"my-world"}` |
| Rename the current display name | `PATCH /world/name` with `{"name":"my-world"}` |

Saved-world and user-preset persistence names must be nonblank basenames. They
may contain internal spaces, but not `/` or `\`, `.` or `..`, absolute or drive
forms, control characters, Windows-reserved characters or device names, or a
trailing dot or space. The current display name changed through
`PATCH /world/name` is not a persistence path and only has to be nonempty.

## Generation presets

Open the config panel and use **Save**, **Load**, **Reset**, or **Revert**.
The catalogue contains six immutable presets:

| ID | Display name |
|---|---|
| `builtin:continental` | Continental |
| `builtin:archipelago` | Archipelago |
| `builtin:large-ocean` | Large Ocean |
| `builtin:inland-sea` | Inland Sea |
| `builtin:arid` | Arid |
| `builtin:lush` | Lush |

Built-ins are labelled `[built-in]`; the API reports `source: "builtin"` and
`read_only: true`. User presets are writable logical names stored below
`~/.topo/configs/`. Loading either kind applies its generation settings; select
**Generate** separately to build a world from them.

HTTP clients use `GET /presets`, `POST /presets` with a name, and
`POST /presets/load` with a catalogue ID or user name.

## Compose a layered view

The left **View** tab selects three independent parts:

1. **Base / terrain view**: Elevation, Biome, Moisture, Vegetation, Terrain
   Form, or Plate ID, Boundary, Hardness, Crust, Age, Height, or Velocity.
2. **Weather / sky overlay**: No Overlay, Temp, Precip, Cloud, or a loaded
   plugin overlay and field.
3. **Weather basis / source**: **Avg/Normal** or **Current** for the built-in
   weather overlays. Plugin overlays do not use this switch.

The default is Elevation with no overlay, Current basis, and opacity `1.0`.
Loading a named world resets to that default; view selection is not stored in
world or preset config snapshots.

`GET /state/views` returns the current layered state and available choices.
`POST /ui/view` updates any of the canonical fields `base_mode`,
`overlay_mode`, `plugin_overlay`, `field_index`, `weather_basis`, and
`overlay_opacity`. Plugin names and field indexes are validated, and opacity is
clamped to `[0,1]`.

For example:

```json
{
  "base_mode": "biome",
  "overlay_mode": "cloud",
  "weather_basis": "current",
  "overlay_opacity": 0.7
}
```

## Inspect and exchange overlays

The View tab provides **Overlays**, **Schema**, **Prov**, **Export**, and
**Validate**:

1. Open **Overlays** and select a loaded overlay. The other inspection actions
   operate on that selection.
2. **Schema** shows the schema name, version, description, `sparse` or `dense`
   storage, dependencies, and ordered fields. Fields carry a name, type,
   default, optional index flag, and optional `renamed_from` name.
3. **Prov** shows the public provenance header: generation seed, monotonic
   version, and source.
4. **Export** produces exact `topolay-json` service JSON containing schema,
   provenance, chunk count, payload, and diagnostics. Sparse payloads contain
   tile records with typed field values; dense payloads contain per-field float
   arrays. **Copy JSON** copies it; **Save JSON** creates a unique sanitized
   `<overlay>-*.json` file under `~/.topo/exports/`.
5. **Validate** opens **Overlay JSON Import Validation**. Paste an object with
   `schema` and `payload`, then select **Validate JSON**. This parses and
   validates only: it never inserts, adopts, or replaces overlay data.

The same operations are available through:

- `GET /overlays`
- `GET /overlays/schema?overlay=NAME`
- `GET /overlays/provenance?overlay=NAME`
- `GET /overlays/fields?overlay=NAME`
- `POST /overlays/export` with `{"overlay":"NAME"}` and optional `chunks`
- `POST /overlays/import/validate` with
  `{"schema":{...},"payload":{...}}`

See the [integrator file-format reference](../integrator/file-formats.md) for
storage contracts rather than treating the inspector as a file-format
specification.

## Capture screenshots

Screenshot capture is an HTTP/AppService operation:

```text
POST /screenshots
POST /screenshots  {"path":"session/map.png"}
```

In SDL mode, a successful response contains base64 PNG bytes,
`format: "png"`, and `source: "renderer"`. Capture without `path` works even
when file persistence is disabled. Saving requires an absolute safe
`screenshotSaveDirectory` in `~/.topo/config.json`; the request path must be a
nonempty safe relative `.png` path. Saves are create-only and never overwrite
an existing destination.

Headless mode has no renderer. It returns HTTP 503 with structured code
`unavailable` and message `screenshot capture requires the SDL renderer`, and
writes no file.

## Plugins and simulation

SDL mode scans `TOPO_PLUGIN_DIR` when it is nonempty, otherwise
`~/.topo/plugins/`. Each plugin occupies a directory with `manifest.json`.
Missing or invalid manifests remain visible as diagnostics and are not launched.

The config **Pipeline** tab shows built-in stages and discovered plugins. It can
enable or disable entries, reorder plugins, expand lifecycle diagnostics, and
edit manifest-declared parameters. `GET /plugins` provides consolidated plugin
visibility; `PATCH /plugins/enabled` and `PATCH /plugins/params` mutate supported
settings. The command-line headless runtime does not perform plugin discovery.

After terrain is loaded, **Tick** requests a simulation tick and **Auto-tick**
toggles scheduled ticking; the panel displays `Rate: N/s`. HTTP clients use
`GET /simulation`, `GET /simulation/dag`, `POST /simulation/tick`, and
`POST /simulation/auto-tick`. Manual tick count defaults to one and is bounded
to 1–100. Auto-tick `rate` is normalized and clamped to `[0,1]`; the default
`0.5` displays as `5/s`.

For packaging, manifest, protocol, and data-source contracts, continue to the
[plugin-author guide](../plugin/README.md).
