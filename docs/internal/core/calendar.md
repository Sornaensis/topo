# Calendar & Time

> **Module:** `Topo.Calendar` (199 LOC)
> **Status:** Stub

## Overview

`Topo.Calendar` provides the temporal model: a monotonic tick counter
mapped to calendar dates via orbital parameters.

## Key Types

- **`WorldTime`** — monotonic tick counter
- **`CalendarDate`** — human-readable date derived from tick
- **`CalendarConfig`** — orbital parameters (year length, etc.)
- **`PlanetAge`** — geological time scale

## Relationship to Weather

Weather simulation advances one `WorldTime` tick per step. The calendar
converts ticks to seasonal position for climate/weather calculations.
