# Rivers

> **Module:** `Topo.River` (442 LOC)
> **Status:** Stub

## Overview

Converts flow-direction and discharge grids into per-tile river
segments with hex-edge encoding.

## Key Concepts

- River segments encoded as hex edge pairs (entry/exit edges)
- Minimum discharge threshold for river formation
- Network pruning removes insignificant tributaries
- Coastal exits where rivers meet ocean/lake

## Configuration

<!-- TODO: Document RiverConfig -->
