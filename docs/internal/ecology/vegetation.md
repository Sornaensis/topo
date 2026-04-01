# Vegetation

> **Module:** `Topo.Vegetation` (837 LOC)
> **Status:** Stub

## Overview

Two-phase vegetation model that bridges the gap between raw terrain
data and biome-aware ecological cover.

## Phase 1: Bootstrap (pre-climate)

- Estimates vegetation cover and albedo from terrain data alone
- Provides initial albedo for the climate model's energy budget
- Quick, approximate values

## Phase 2: Biome Feedback (post-biome)

- Re-derives cover from assigned biome
- Blends with bootstrap estimates
- Per-biome density and climate lookups
- Final canopy cover, vegetation density

## Key Data

Per-biome lookup tables for:
- Base vegetation density
- Climate suitability curves
- Canopy height ranges
