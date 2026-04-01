# Water Table

> **Module:** `Topo.WaterTable` (464 LOC)
> **Status:** Stub

## Overview

Subsurface water model. Computes water table depth, infiltration
capacity, and root-zone moisture.

## Model

- Topographic assumption + diffusion for water table depth
- Infiltration capacity from soil type and depth
- Root-zone moisture feeds vegetation and fertility
- Overwrites `tcFertility` with improved moisture-aware formula
