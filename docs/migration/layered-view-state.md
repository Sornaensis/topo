# Layered view state and legacy `ViewMode` migration

Topo-seer is migrating from a single `ViewMode` value to an explicit layered
view selection. The new model separates the terrain/base atlas from optional
weather, sky, or plugin overlays while keeping legacy `view_mode` names working
for existing clients.

## Layer stack

The render stack is, from bottom to top:

1. **Base atlas layer** — exactly one terrain/physical base view such as
   elevation, biome, moisture, vegetation, terrain form, or plate attributes.
2. **Weather/sky overlay layer** — optional temperature, precipitation, cloud,
   or plugin overlay field rendered over the base with `overlay_opacity`.
3. **Day/night overlay** — optional solar shading drawn as a separate top
   overlay when the UI toggle is enabled.

`ViewMode` remains as a compatibility facade. New code should pass and return a
`LayeredViewState` (`view` in JSON) whenever it needs to preserve both base and
overlay selections.

## Module ownership

| Concern | Owning modules | Notes |
|---|---|---|
| Layered state, legacy adapters, labels, API metadata | `Actor.UI.State` | Defines `BaseViewMode`, `SkyOverlayMode`, `WeatherBasis`, `LayeredViewState`, `legacyViewModeToLayeredViewState`, `layeredViewStateToViewMode`, JSON fields, temporal/source semantics, legends, and legacy names. |
| UI controls and hotkeys | `Seer.Draw`, `Seer.Draw.LeftPanel`, `Seer.Input.ViewControls`, `Seer.Input.Widgets`, `UI.WidgetTree`, `UI.Layout` | The View tab exposes base buttons, overlay buttons, weather basis buttons, day/night toggle, and plugin overlay field controls. Number keys select base views; letter keys select/cycle overlays and basis. |
| Command/AppService/HTTP adapters | `Seer.Command.Handlers.State`, `Seer.Command.Handlers.View`, `Seer.Service.State`, `Seer.Service.UI`, `Seer.HTTP.API`, `Seer.HTTP.Server` | `get_state`/`GET /state` include both `view_mode` and `view`; `get_views`/`GET /state/views` is the layered discovery surface; `set_view`/`POST /ui/view` is the preferred mutation surface; `set_view_mode`/`POST /ui/view-mode` is compatibility. |
| Base and overlay colors/geometry | `UI.TerrainColor`, `UI.TerrainRender`, `UI.OverlayExtract` | Base colors are computed independently from overlay RGBA. Plugin overlays read a named `OverlayStore` field through `UI.OverlayExtract`. |
| Atlas scheduling, cache keys, worker builds | `Actor.AtlasCache`, `Actor.AtlasManager`, `Actor.AtlasScheduler`, `Actor.AtlasWorker` | Base and overlay atlas keys are scheduled independently. Overlay/weather refreshes do not invalidate unchanged base tiles. |
| Render-thread composition | `Seer.Render.Atlas`, `Seer.Render.Frame` | Draws base atlas tiles first, optional overlay atlas tiles with draw-time alpha second, then day/night tiles. Fallback non-render-target paths still compose through the same selection semantics. |
| Persistence metadata | `Seer.World.Persist`, `Seer.World.Persist.Types`, `Topo.Persistence.WorldBundle` | Saved worlds persist terrain, overlays, and climate/weather layer metadata, not the active UI view selection. |

## Weather basis and data sources

The `weather_basis` field is UI-facing and maps to API/data-source semantics as
follows:

| Overlay | `weather_basis=average` | `weather_basis=current` |
|---|---|---|
| Temperature (`overlay_mode=weather`) | `legacy_view_mode=climate`; `temporal_basis=long_run_average`; `source_kind=climate_average`; source is core climate chunks. | `legacy_view_mode=weather`; `temporal_basis=instantaneous_current`; `source_kind=weather_snapshot`; source is current simulated `weather` data. |
| Precipitation (`overlay_mode=precipitation`) | `legacy_view_mode=precipitation`; `temporal_basis=long_run_average`; `source_kind=climate_average`; source is core climate chunks. | `legacy_view_mode=precipitation_current`; `temporal_basis=instantaneous_current`; `source_kind=weather_snapshot`; source is current simulated `weather` data. |
| Cloud/storm (`overlay_mode=cloud`) | `legacy_view_mode=cloud_typical`; `temporal_basis=typical_normal`; `source_kind=weather_normals`; source is the generated `weather_normals` overlay. | `legacy_view_mode=cloud`; `temporal_basis=instantaneous_current`; `source_kind=weather_snapshot`; source is current simulated `weather` data. |
| Plugin overlay (`overlay_mode=plugin`) | Not applicable; basis controls are disabled. | Not applicable; plugin overlay values come from the selected overlay field. |

The generated `weather_normals` layer is implemented. Typical cloud/weather
normal views should document and use that layer rather than describing normals
as missing work.

Built-in climate, weather, and normals are deterministic generated/simulated
data. They are not external live observations. Future externally supplied layers
must identify their source through plugin/data-resource metadata instead of
pretending to be built-in weather.

## Atlas data-version semantics

Layered keys keep render invalidation local to the data that changed:

| Layer/key | Data version source |
|---|---|
| Base elevation, biome, moisture, plate, terrain-form layers | `tsVersion` |
| Base vegetation layer | max of `tsVersion` and `tsVegetationVersion` |
| Temperature/precipitation average overlays | `tsClimateVersion` |
| Temperature/precipitation current overlays | `tsWeatherVersion` |
| Cloud average overlay | max of `tsClimateVersion` and `tsOverlayVersion` because the rendered data comes from `weather_normals` overlay payloads derived from climate/config. |
| Cloud current overlay | `tsWeatherVersion` |
| Plugin overlay field | `tsOverlayVersion` |
| Day/night overlay | Separate day/night key derived from solar/world-time inputs; it is not a `ViewMode`. |

`overlay_opacity` is applied at draw time for render-target atlas overlays, so
changing opacity should not rebuild overlay atlas textures. Compatibility and
fallback code may still include opacity in selection fingerprints where a single
pre-composited key is required.

## API migration path

Prefer the layered routes and fields:

| Legacy surface | Preferred layered surface |
|---|---|
| `GET /state` top-level `view_mode` | `GET /state` `view` object, or `GET /state/views` for state plus choices |
| `GET /state/view-modes` / `get_view_modes` | `GET /state/views` / `get_views` |
| `POST /ui/view-mode` with `{ "mode": ... }` | `POST /ui/view` with `{ "base_mode": ..., "overlay_mode": ..., "weather_basis": ... }` |
| `POST /commands/set_view_mode` | Not a public automation target; use `POST /ui/view` or `POST /ui/view-mode` while migrating. |

Layered request fields accepted by `POST /ui/view`:

- `base_mode` or `base`: one of the base mode names (`elevation`, `biome`,
  `moisture`, `vegetation`, `terrain_form`, plate fields, etc.).
- `overlay_mode` or `overlay`: `null`, `none`, `weather`, `precipitation`,
  `cloud`, `plugin`, or `overlay:<name>`.
- `plugin_overlay`: overlay name when `overlay_mode` is `plugin`.
- `field_index` or `overlay_field`: zero-based plugin overlay field index.
- `weather_basis`, `basis`, or `temporal_basis`: `average`/`long_run_average`
  or `current`/`instantaneous_current` for built-in weather overlays.
- `overlay_opacity`: `0.0` to `1.0`.

Example layered requests:

```json
{ "base_mode": "biome" }
```

```json
{
  "base_mode": "biome",
  "overlay_mode": "cloud",
  "weather_basis": "average",
  "overlay_opacity": 0.55
}
```

```json
{
  "base_mode": "elevation",
  "overlay_mode": "plugin",
  "plugin_overlay": "civilization",
  "field_index": 1
}
```

## Legacy `view_mode` mapping

Legacy one-dimensional names remain accepted, but they cannot represent a
non-elevation base plus an overlay at the same time. Setting a legacy overlay
mode resets the layered base to the default elevation base.

| Legacy `view_mode` name | Layered interpretation |
|---|---|
| `elevation`, `biome`, `moisture`, `vegetation`, `terrain_form`, plate modes | `base_mode=<same>`, `overlay_mode=null`, `weather_basis=current` default |
| `climate` | `base_mode=elevation`, `overlay_mode=weather`, `weather_basis=average` |
| `weather` | `base_mode=elevation`, `overlay_mode=weather`, `weather_basis=current` |
| `precipitation` | `base_mode=elevation`, `overlay_mode=precipitation`, `weather_basis=average` |
| `precipitation_current` / `current_precipitation` | `base_mode=elevation`, `overlay_mode=precipitation`, `weather_basis=current` |
| `cloud_typical` / `typical_cloud` | `base_mode=elevation`, `overlay_mode=cloud`, `weather_basis=average` |
| `cloud` | `base_mode=elevation`, `overlay_mode=cloud`, `weather_basis=current` |
| `overlay:<name>` | `base_mode=elevation`, `overlay_mode=plugin`, `plugin_overlay=<name>`, `field_index` from request or `0` |

`POST /ui/view-mode` also accepts `basis` or `temporal_basis` for weather-mode
families. For example, `{ "mode": "cloud", "basis": "typical" }` maps to
`cloud_typical`, and `{ "mode": "precipitation", "basis": "current" }` maps to
`precipitation_current`.

## Compatibility and deprecation plan

The following response fields and routes are compatibility surfaces:

- Top-level `view_mode` in state responses.
- `legacy_view_mode` inside layered response objects.
- `view_modes` one-dimensional lists.
- `POST /ui/view-mode` and the internal command method `set_view_mode`.

They are deprecated for new clients because they lose layered information.
There is no removal scheduled within HTTP API major version `1`; removing or
breaking them requires a future API major version or a documented migration
window. During the 1.x line, clients should treat them as read/write aliases and
store the layered `view` object as authoritative.

## Persistence and old-world behavior

World save/load persists data layers, not the transient UI selection:

- Core `.topo` stores terrain and climate chunks.
- `world.topolay/` stores sidecar overlays such as `weather`,
  `weather_normals`, and plugin overlays.
- `meta.json.weather_layers` records layer name, basis, source kind, and storage
  for climate/weather-oriented data.
- `config.json` intentionally excludes `view`, `view_mode`, `viewSelection`,
  `weather_basis`, and `overlay_opacity`.

Loading an old or new world starts the UI from `defaultLayeredViewState`:
`base_mode=elevation`, no sky overlay, `weather_basis=current`, and full overlay
opacity. Old config snapshots that contain legacy layered keys are parsed for the
configuration fields only; view-selection keys are ignored.

## Non-goals

- No screenshot-specific view override is part of this migration. Screenshot
  routes capture the current UI/render state unless a future task deliberately
  adds an override contract.
- No new weather-normal generation work is tracked here. Typical cloud/weather
  normals are already represented by the generated `weather_normals` overlay;
  documentation should describe its source/basis instead of listing it as
  missing.
- This migration does not make built-in weather an external live data source.
