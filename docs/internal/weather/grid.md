# Weather Grid

> **Module:** `Topo.Weather.Grid` (221 LOC)
> **Status:** Stub

## Overview

Conversion between weather overlay data and global weather grids.

## Overlay Schema

Weather uses a 6-field dense overlay:

| Field | Type | Description |
|-------|------|-------------|
| temperature | Float | Current temperature |
| humidity | Float | Relative humidity |
| windDirection | Float | Wind direction (radians) |
| windSpeed | Float | Wind speed |
| pressure | Float | Atmospheric pressure |
| precipitation | Float | Current precipitation |

## Functions

- Grid builders: overlay → per-field arrays
- Grid writers: per-field arrays → overlay
