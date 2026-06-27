# MCP-to-HTTP Parity Matrix

This matrix is the M4 migration checklist for the retired `topo-mcp` bridge. It
maps every MCP protocol behavior, advertised tool, advertised resource, and the
command method each item used to the direct `topo-seer` HTTP/OpenAPI target or
to an explicit waiver.

Sources used for this inventory:

- Removed MCP package at `44b7a33^`: `Topo.MCP.Server`, `Topo.MCP.Tools`,
  `Topo.MCP.Resources`, and their tests.
- Current HTTP route table: `topo-seer/src/Seer/HTTP/Server.hs`.
- Current service operation metadata: `topo-seer/src/Seer/Service/*`.

Checklist conventions:

- **Primary HTTP target** is the preferred public HTTP/OpenAPI route.
- **Command fallback** means `POST /commands/<method>`, generated for every
  current AppService command method. It exists for compatibility and tests; new
  external automation should prefer the primary resource-oriented route.
- **Mapped** means MCP-era behavior has a direct HTTP/OpenAPI route. Payload
  validation and response parity belong to the M4 test-porting task.
- **Waived** means the behavior was MCP session, stdio transport, or
  bridge-internal behavior and is intentionally not part of the public HTTP API.

## MCP protocol and bridge behavior

| MCP / bridge behavior | Old behavior | HTTP/OpenAPI target or waiver | Status |
|---|---|---|---|
| stdio JSON-RPC transport | `topo-mcp` read newline-delimited JSON-RPC from stdin/stdout. | Direct WAI/Warp HTTP server in `topo-seer`; no stdio public transport. | Waived: MCP transport only. |
| `initialize` | Returned protocol version `2024-11-05`, tools/resources capabilities, and `topo-mcp` server info. | `GET /version` (`meta.version`) and `GET /openapi.json` (`meta.openapi`). | Mapped. |
| `initialized` notification | Accepted without response. | No HTTP equivalent. | Waived: MCP session notification only. |
| `ping` | Returned an empty JSON-RPC result. | `GET /health` (`meta.health`) returns health JSON. | Mapped. |
| `tools/list` | Returned all MCP `ToolDef` records and input schemas. | `GET /openapi.json` plus this matrix; OpenAPI is the public operation catalog. | Mapped. |
| `tools/call` | Parsed `{name, arguments}` and routed to one command method. | Use each tool row's primary HTTP route, or `POST /commands/<method>` for command-compatible dispatch. | Mapped. |
| `resources/list` | Returned 11 static resources and 5 templates. | `GET /openapi.json` plus the resource matrix below. | Mapped. |
| `resources/read` | Parsed `topo://...` URI, called a command method, and returned JSON text content. | Use each resource row's HTTP route; HTTP returns JSON directly. | Mapped. |
| Unknown JSON-RPC method / tool / resource | JSON-RPC error or MCP tool error content. | HTTP route miss returns `404`; service validation failures use HTTP error envelopes. | Mapped as HTTP error semantics. |
| MCP-to-seer command IPC | `topo-mcp` opened named pipe / Unix socket and sent length-prefixed `SeerCommand`. | Direct HTTP calls route through AppService. Remaining command IPC is internal/test compatibility, not a 1.0 public path. | Waived: bridge plumbing only. |

## MCP resources/read parity

All retired MCP resources had `application/json` MIME type. HTTP returns JSON
with normal `application/json` response headers.

| MCP resource URI or template | Old command method | Primary HTTP/OpenAPI target | Command fallback | Status / notes |
|---|---:|---|---|---|
| `topo://state` | `get_state` | `GET /state` (`state.get`) | `POST /commands/get_state` | Mapped. |
| `topo://sliders` | `get_sliders` | `GET /config/sliders` (`config.sliders.list`) | `POST /commands/get_sliders` | Mapped. |
| `topo://sliders/{tab}` | `get_sliders` with `tab` | `GET /config/sliders?tab={tab}` (`config.sliders.list`) | `POST /commands/get_sliders` | Mapped; checklist item for test-porting: assert optional `tab` query parity. |
| `topo://slider/{name}` | `get_slider` | `POST /config/sliders/get` (`config.sliders.get`) body `{ "name": ... }` | `POST /commands/get_slider` | Mapped. |
| `topo://view-modes` | `get_view_modes` | `GET /state/view-modes` (`state.viewModes`) | `POST /commands/get_view_modes` | Mapped. |
| `topo://editor/state` | `editor_get_state` | `GET /editor` (`editor.state`) | `POST /commands/editor_get_state` | Mapped. |
| `topo://world` | `get_world_meta` | `GET /world` (`world.meta`) | `POST /commands/get_world_meta` | Mapped. |
| `topo://generation-status` | `get_generation_status` | `GET /world/generation-status` (`world.generationStatus`) | `POST /commands/get_generation_status` | Mapped. |
| `topo://chunks` | `get_chunks` | `GET /terrain/chunks` (`terrain.chunks`) | `POST /commands/get_chunks` | Mapped. |
| `topo://chunk/{id}` | `get_chunk_summary` | `GET /terrain/chunk-summary?chunk={id}` (`terrain.chunkSummary`) | `POST /commands/get_chunk_summary` | Mapped. |
| `topo://hex/{q}/{r}` | `get_hex` | `GET /terrain/hex?q={q}&r={r}` (`terrain.hex`) | `POST /commands/get_hex` | Mapped. |
| `topo://terrain-stats` | `get_terrain_stats` | `GET /terrain/stats` (`terrain.stats`) | `POST /commands/get_terrain_stats` | Mapped. |
| `topo://overlays` | `get_overlays` | `GET /overlays` (`overlays.list`) or `GET /terrain/overlays` (`terrain.overlays`) | `POST /commands/get_overlays` | Mapped. |
| `topo://worlds` | `list_worlds` | `GET /worlds` (`worlds.list`) | `POST /commands/list_worlds` | Mapped. |
| `topo://presets` | `list_presets` | `GET /presets` (`presets.list`) | `POST /commands/list_presets` | Mapped. |
| `topo://enums/{type}` | `get_enums` | `GET /config/enums?type={type}` (`config.enums`) | `POST /commands/get_enums` | Mapped. |

## MCP tools/call parity

| MCP tool name | Old command method | Primary HTTP/OpenAPI target | Command fallback | Status / notes |
|---|---:|---|---|---|
| `get_state` | `get_state` | `GET /state` (`state.get`) | `POST /commands/get_state` | Mapped. |
| `list_sliders` | `get_sliders` | `GET /config/sliders` (`config.sliders.list`) | `POST /commands/get_sliders` | Mapped; optional `tab` remains query/body input. |
| `get_slider` | `get_slider` | `POST /config/sliders/get` (`config.sliders.get`) | `POST /commands/get_slider` | Mapped. |
| `set_slider` | `set_slider` | `POST /config/sliders` (`config.sliders.set`) | `POST /commands/set_slider` | Mapped. |
| `set_seed` | `set_seed` | `POST /ui/seed` (`ui.seed.set`) | `POST /commands/set_seed` | Mapped. |
| `set_view_mode` | `set_view_mode` | `POST /ui/view-mode` (`ui.viewMode.set`) | `POST /commands/set_view_mode` | Mapped. |
| `set_config_tab` | `set_config_tab` | `POST /ui/config-tab` (`ui.configTab.set`) | `POST /commands/set_config_tab` | Mapped. |
| `get_view_modes` | `get_view_modes` | `GET /state/view-modes` (`state.viewModes`) | `POST /commands/get_view_modes` | Mapped. |
| `generate` | `generate` | `POST /world/generate` (`world.generate`) | `POST /commands/generate` | Mapped. |
| `editor_toggle` | `editor_toggle` | `POST /editor/toggle` (`editor.toggle`) | `POST /commands/editor_toggle` | Mapped. |
| `editor_set_tool` | `editor_set_tool` | `POST /editor/tool` (`editor.tool.set`) | `POST /commands/editor_set_tool` | Mapped. |
| `editor_set_brush` | `editor_set_brush` | `PATCH /editor/brush` (`editor.brush.set`) | `POST /commands/editor_set_brush` | Mapped. |
| `editor_brush_stroke` | `editor_brush_stroke` | `POST /editor/brush-stroke` (`editor.brushStroke`) | `POST /commands/editor_brush_stroke` | Mapped. |
| `editor_brush_line` | `editor_brush_line` | `POST /editor/brush-line` (`editor.brushLine`) | `POST /commands/editor_brush_line` | Mapped. |
| `editor_set_biome` | `editor_set_biome` | `POST /editor/biome` (`editor.biome.set`) | `POST /commands/editor_set_biome` | Mapped. |
| `editor_set_form` | `editor_set_form` | `POST /editor/form` (`editor.form.set`) | `POST /commands/editor_set_form` | Mapped. |
| `editor_set_hardness` | `editor_set_hardness` | `POST /editor/hardness` (`editor.hardness.set`) | `POST /commands/editor_set_hardness` | Mapped. |
| `editor_undo` | `editor_undo` | `POST /editor/undo` (`editor.undo`) | `POST /commands/editor_undo` | Mapped. |
| `editor_redo` | `editor_redo` | `POST /editor/redo` (`editor.redo`) | `POST /commands/editor_redo` | Mapped. |
| `editor_get_state` | `editor_get_state` | `GET /editor` (`editor.state`) | `POST /commands/editor_get_state` | Mapped. |
| `get_enums` | `get_enums` | `GET /config/enums?type={type}` (`config.enums`) | `POST /commands/get_enums` | Mapped. |
| `get_world_meta` | `get_world_meta` | `GET /world` (`world.meta`) | `POST /commands/get_world_meta` | Mapped. |
| `get_generation_status` | `get_generation_status` | `GET /world/generation-status` (`world.generationStatus`) | `POST /commands/get_generation_status` | Mapped. |
| `inspect_hex` | `get_hex` | `GET /terrain/hex?q={q}&r={r}` (`terrain.hex`) | `POST /commands/get_hex` | Mapped; tool name differed from command method. |
| `get_chunks` | `get_chunks` | `GET /terrain/chunks` (`terrain.chunks`) | `POST /commands/get_chunks` | Mapped. |
| `get_chunk_summary` | `get_chunk_summary` | `GET /terrain/chunk-summary?chunk={chunk}` (`terrain.chunkSummary`) | `POST /commands/get_chunk_summary` | Mapped. |
| `get_terrain_stats` | `get_terrain_stats` | `GET /terrain/stats` (`terrain.stats`) | `POST /commands/get_terrain_stats` | Mapped. |
| `get_overlays` | `get_overlays` | `GET /overlays` (`overlays.list`) or `GET /terrain/overlays` (`terrain.overlays`) | `POST /commands/get_overlays` | Mapped. |
| `list_worlds` | `list_worlds` | `GET /worlds` (`worlds.list`) | `POST /commands/list_worlds` | Mapped. |
| `set_sliders` | `set_sliders` | `PATCH /config/sliders` (`config.sliders.setMany`) | `POST /commands/set_sliders` | Mapped. |
| `reset_sliders` | `reset_sliders` | `POST /config/sliders/reset` (`config.sliders.reset`) | `POST /commands/reset_sliders` | Mapped. |
| `select_hex` | `select_hex` | `POST /ui/select-hex` (`ui.hex.select`) | `POST /commands/select_hex` | Mapped. |
| `save_world` | `save_world` | `POST /worlds/save` (`worlds.save`) | `POST /commands/save_world` | Mapped. |
| `load_world` | `load_world` | `POST /worlds/load` (`worlds.load`) | `POST /commands/load_world` | Mapped. |
| `list_presets` | `list_presets` | `GET /presets` (`presets.list`) | `POST /commands/list_presets` | Mapped. |
| `save_preset` | `save_preset` | `POST /presets` (`presets.save`) | `POST /commands/save_preset` | Mapped. |
| `load_preset` | `load_preset` | `POST /presets/load` (`presets.load`) | `POST /commands/load_preset` | Mapped. |
| `take_screenshot` | `take_screenshot` | `POST /screenshots` (`screenshots.take`) | `POST /commands/take_screenshot` | Mapped; MCP image content becomes JSON with base64 PNG fields. |
| `set_camera` | `set_camera` | `PUT /camera` (`camera.set`) or `PUT /ui/camera` (`ui.camera.set`) | `POST /commands/set_camera` | Mapped. |
| `get_camera` | `get_camera` | `GET /camera` (`camera.get`) or `GET /ui/camera` (`ui.camera.get`) | `POST /commands/get_camera` | Mapped. |
| `zoom_to_chunk` | `zoom_to_chunk` | `POST /camera/zoom-to-chunk` (`camera.zoomToChunk`) or `POST /ui/camera/zoom-to-chunk` (`ui.camera.zoomToChunk`) | `POST /commands/zoom_to_chunk` | Mapped. |
| `get_logs` | `get_logs` | `GET /logs?level=&limit=&offset=` (`logs.get`) | `POST /commands/get_logs` | Mapped. |
| `set_world_name` | `set_world_name` | `PATCH /world/name` (`world.name.set`) | `POST /commands/set_world_name` | Mapped. |
| `get_pipeline` | `get_pipeline` | `GET /pipeline` (`pipeline.get`) | `POST /commands/get_pipeline` | Mapped. |
| `set_stage_enabled` | `set_stage_enabled` | `PATCH /pipeline/stages` (`pipeline.stage.setEnabled`) | `POST /commands/set_stage_enabled` | Mapped. |
| `list_plugins` | `list_plugins` | `GET /plugins` (`plugins.list`); status views also at `/plugins/status`, `/plugins/state`, `/plugins/dependencies` | `POST /commands/list_plugins` | Mapped. |
| `set_plugin_enabled` | `set_plugin_enabled` | `PATCH /plugins/enabled` (`plugins.setEnabled`) | `POST /commands/set_plugin_enabled` | Mapped; HTTP body uses command field `name` plus `enabled` rather than the retired MCP schema's `plugin` key. |
| `set_plugin_param` | `set_plugin_param` | `PATCH /plugins/params` (`plugins.params.set`) | `POST /commands/set_plugin_param` | Mapped. |
| `get_sim_state` | `get_sim_state` | `GET /simulation` (`simulation.state`) | `POST /commands/get_sim_state` | Mapped. |
| `set_sim_auto_tick` | `set_sim_auto_tick` | `POST /simulation/auto-tick` (`simulation.autoTick.set`) | `POST /commands/set_sim_auto_tick` | Mapped. |
| `sim_tick` | `sim_tick` | `POST /simulation/tick` (`simulation.tick`) | `POST /commands/sim_tick` | Mapped. |
| `get_config_summary` | `get_config_summary` | `GET /config/summary` (`config.summary`) | `POST /commands/get_config_summary` | Mapped. |
| `find_hexes` | `find_hexes` | `POST /terrain/search` (`terrain.search`) | `POST /commands/find_hexes` | Mapped. |
| `export_terrain_data` | `export_terrain_data` | `POST /terrain/export` (`terrain.export`) | `POST /commands/export_terrain_data` | Mapped. |
| `set_left_panel` | `set_left_panel` | `PUT /ui/left-panel` (`ui.leftPanel.set`) | `POST /commands/set_left_panel` | Mapped. |
| `set_left_tab` | `set_left_tab` | `PUT /ui/left-tab` (`ui.leftTab.set`) | `POST /commands/set_left_tab` | Mapped. |
| `toggle_config_panel` | `toggle_config_panel` | `POST /ui/config-panel/toggle` (`ui.configPanel.toggle`) | `POST /commands/toggle_config_panel` | Mapped. |
| `set_log_collapsed` | `set_log_collapsed` | `PUT /ui/log/collapsed` (`ui.logCollapsed.set`) | `POST /commands/set_log_collapsed` | Mapped. |
| `set_log_level` | `set_log_level` | `PUT /ui/log/level` (`ui.logLevel.set`) | `POST /commands/set_log_level` | Mapped. |
| `get_ui_panels` | `get_ui_panels` | `GET /ui/panels` (`ui.panels.get`) | `POST /commands/get_ui_panels` | Mapped. |
| `set_overlay` | `set_overlay` | `PUT /overlays/current` (`overlays.current.set`) or `POST /ui/overlay` (`ui.overlay.set`) | `POST /commands/set_overlay` | Mapped. |
| `list_overlay_fields` | `list_overlay_fields` | `GET /overlays/fields?overlay=` (`overlays.fields.list`) or `GET /ui/overlay-fields?overlay=` (`ui.overlayFields.list`) | `POST /commands/list_overlay_fields` | Mapped. |
| `cycle_overlay` | `cycle_overlay` | `POST /overlays/cycle` (`overlays.cycle`) or `POST /ui/overlay/cycle` (`ui.overlay.cycle`) | `POST /commands/cycle_overlay` | Mapped. |
| `cycle_overlay_field` | `cycle_overlay_field` | `POST /overlays/fields/cycle` (`overlays.field.cycle`) or `POST /ui/overlay-field/cycle` (`ui.overlayField.cycle`) | `POST /commands/cycle_overlay_field` | Mapped. |
| `get_ui_state` | `get_ui_state` | `GET /ui/state` (`ui.state`) | `POST /commands/get_ui_state` | Mapped. |
| `data_list_plugins` | `data_list_plugins` | `GET /data/plugins` (`data.plugins.list`) | `POST /commands/data_list_plugins` | Mapped. |
| `data_list_resources` | `data_list_resources` | `GET /data/resources?plugin={plugin}` (`data.resources.list`) | `POST /commands/data_list_resources` | Mapped. |
| `data_list_records` | `data_list_records` | `GET /data/records?plugin={plugin}&resource={resource}&page_size=&page_offset=` (`data.records.list`) | `POST /commands/data_list_records` | Mapped. |
| `data_get_record` | `data_get_record` | `POST /data/records/get` (`data.records.get`) | `POST /commands/data_get_record` | Mapped. |
| `data_create_record` | `data_create_record` | `POST /data/records` (`data.records.create`) | `POST /commands/data_create_record` | Mapped. |
| `data_update_record` | `data_update_record` | `PUT /data/records` (`data.records.update`) | `POST /commands/data_update_record` | Mapped. |
| `data_delete_record` | `data_delete_record` | `DELETE /data/records` (`data.records.delete`) | `POST /commands/data_delete_record` | Mapped. |
| `data_get_state` | `data_get_state` | `GET /data/state` (`data.state`) | `POST /commands/data_get_state` | Mapped. |
| `click_widget` | `click_widget` | `POST /ui/widgets/click` (`ui.widgets.click`) | `POST /commands/click_widget` | Mapped. |
| `list_widgets` | `list_widgets` | `GET /ui/widgets` (`ui.widgets.list`) | `POST /commands/list_widgets` | Mapped. |
| `get_widget_state` | `get_widget_state` | `GET /ui/widget-state?widget_id={widget_id}` (`ui.widgetState.get`) | `POST /commands/get_widget_state` | Mapped. |
| `viewport_scroll` | `viewport_scroll` | `POST /ui/viewport/scroll` (`ui.viewport.scroll`) | `POST /commands/viewport_scroll` | Mapped. |
| `viewport_click` | `viewport_click` | `POST /ui/viewport/click` (`ui.viewport.click`) | `POST /commands/viewport_click` | Mapped. |
| `viewport_drag` | `viewport_drag` | `POST /ui/viewport/drag` (`ui.viewport.drag`) | `POST /commands/viewport_drag` | Mapped. |
| `viewport_hover` | `viewport_hover` | `POST /ui/viewport/hover` (`ui.viewport.hover`) | `POST /commands/viewport_hover` | Mapped. |
| `get_dialog_state` | `get_dialog_state` | `GET /ui/dialog` (`ui.dialog.get`) | `POST /commands/get_dialog_state` | Mapped. |
| `set_dialog_text` | `set_dialog_text` | `PUT /ui/dialog/text` (`ui.dialogText.set`) | `POST /commands/set_dialog_text` | Mapped. |
| `dialog_confirm` | `dialog_confirm` | `POST /ui/dialog/confirm` (`ui.dialog.confirm`) | `POST /commands/dialog_confirm` | Mapped. |
| `dialog_cancel` | `dialog_cancel` | `POST /ui/dialog/cancel` (`ui.dialog.cancel`) | `POST /commands/dialog_cancel` | Mapped. |
| `send_key` | `send_key` | `POST /ui/key` (`ui.key.send`) | `POST /commands/send_key` | Mapped. |

## Non-MCP current operations

The current HTTP/AppService surface also includes operations that were not
advertised by the retired MCP package, including `GET /simulation/dag`
(`simulation.dag`) and internal event publishing (`publish_event`). They are not
MCP parity requirements, but they remain covered by current HTTP/service route
coverage and should not be used to justify keeping `topo-mcp`.

## Migration checklist summary

- 85 retired MCP tools are mapped.
- 11 retired static resources are mapped.
- 5 retired resource templates are mapped.
- MCP protocol/session/stdio behavior is waived as bridge-only.
- Every mapped command method has an HTTP route and a command-compatible
  fallback route generated from AppService metadata.
- Package-removal gate: no required Topo 1.0 automation capability needs the
  retired `topo-mcp` package after the mapped HTTP routes are used.
