# Biome Refinement

> **Modules:** `Topo.Biome.Refine` (325 LOC), `Topo.Biome.Refine.*` (14 sub-modules)
> **Status:** Stub

## Overview

After primary biome classification, refinement dispatches each tile to
a family-specific refiner that assigns a detailed sub-biome.

## Refinement Families

| Family | Sub-biomes |
|--------|------------|
| Alpine | Scree, alpine tundra, alpine meadow |
| Coastal | Mangrove, dunes, rocky shore, salt marsh, estuary, coastal scrub |
| Desert | Hot, cold, rocky, sand, salt flat |
| Forest | Cloud forest, montane, temperate rainforest, tropical dry, deciduous, coniferous, tropical seasonal |
| Grassland | Steppe, prairie, meadow |
| Ocean | Deep ocean, coral reef |
| Rainforest | Tropical, temperate |
| Savanna | Tropical savanna, dry savanna |
| Shrubland | Mediterranean, chaparral |
| Snow | Ice cap, permafrost |
| Swamp | Wetland, bog |
| Taiga | Boreal forest, sparse taiga |
| Tundra | Arctic, alpine, polar desert |
| Volcanic | Volcanic wasteland, fumarole |

## Configuration

Each family has its own refinement config with presets for arid and
lush worlds.
