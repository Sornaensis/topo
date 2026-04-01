# Climate Model

> **Module:** `Topo.Climate` (1063 LOC)
> **Status:** Stub

## Overview

The climate generation engine. Computes per-tile temperature, wind,
moisture, and precipitation from planetary parameters and terrain.

## Model Components

1. **Temperature** — latitude→temperature curve + elevation lapse rate
   + ocean moderation + convective cooling feedback
2. **Wind** — global wind belts (trade winds, westerlies, polar easterlies)
3. **Moisture transport** — advection along wind vectors
4. **Orographic precipitation** — uplift on windward slopes
5. **Rain shadow** — drying on leeward slopes
6. **ITCZ convergence** — wind convergence field drives equatorial precipitation band

## ITCZ Convergence Model

> **Module:** `Topo.Climate.ITCZ`

The Intertropical Convergence Zone (ITCZ) is modeled via a wind convergence
field computed on the hex grid, replacing the earlier static Gaussian.

### Convergence Field

`convergenceField` computes per-tile convergence as the negative divergence
of the wind vector field. For each tile, it sums the dot product of the
wind vector with the outward normal for each hex neighbor edge, then negates:

- **Positive** values → converging air → uplift → precipitation
- **Negative** values → diverging air → subsidence → drying
- **Near-zero** → uniform/parallel flow, no vertical motion

### ITCZ Band

`itczBand` combines the convergence field with a latitude-dependent Gaussian
envelope to produce an ITCZ intensity grid:

    intensity = scale × max(0, convergence - threshold) × exp(-lat² / (2 × width²))

This ensures the ITCZ is strongest where winds physically converge (typically
near the equator where trade winds meet) and is modulated by terrain — mountains
can split theband, valleys can concentrate it.

### Seasonal Migration

`seasonalITCZShift` models the ITCZ's seasonal north-south migration as a
sinusoidal function of orbital phase:

    lat_itcz = baseLat + migrationScale × axialTilt × sin(phase)

This function is shared between the climate generation stage and the weather
simulation tick, ensuring consistent ITCZ positioning.

### Convective Cooling

Where convergence and precipitation are both strong, a cooling correction is
applied to the temperature grid:

    T_corrected = T - coolingScale × precip × max(0, convergence)

This approximates latent heat transport: surface moisture evaporates (cooling
the surface), rises in the convergence zone, and condenses at altitude. The
effect prevents equatorial temperature saturation, producing realistic ~25-35°C
equatorial temperatures instead of an unrealistic 50°C ceiling.

### Configuration

The ITCZ model uses `ConvergenceConfig` with five parameters:
- `convIntensityScale` — overall strength multiplier
- `convLatitudeWidth` — Gaussian envelope width in degrees
- `convCenterLat` — center latitude for the envelope
- `convMinThreshold` — minimum convergence to trigger the band
- `convStrengthTotal` — total strength normalization

These are constructed from the existing `moistITCZStrength` and `moistITCZWidth`
fields in `MoistureConfig`, requiring no config migration.

## Moisture Transport Tuning

The moisture transport loop runs 36 iterations of advection-condensation
per tile. Precipitation is accumulated condensation across all iterations
plus orographic and plate-bias contributions, clamped to [0,1]. There is
no normalization step, so parameter sensitivity compounds across iterations.

### Key Parameters (`MoistureConfig`)

Three parameters were tuned from their original defaults to achieve
realistic biome diversity (particularly Tropical Rainforest coverage):

| Parameter | Original | Tuned | Rationale |
|-----------|----------|-------|-----------|
| `moistITCZStrength` | 0.15 | 0.25 | Per-iteration ITCZ boost (~0.007/iter). Needed to produce meaningful equatorial precipitation enhancement. |
| `moistConvectiveThreshold` | 0.80 | 0.75 | RH threshold for convective rain. 5% reduction allows convection to fire in realistically humid conditions without flooding. |
| `moistConvectiveRate` | 0.08 | 0.10 | Moderate convective rain rate increase. Works with the lower threshold to produce adequate tropical precipitation. |

### Sensitivity Notes

- **Convective threshold is the most sensitive parameter.** Because it is
  evaluated on every iteration (36 total), small changes compound. Dropping
  below 0.65 causes a precipitation explosion (80%+ Rainforest land coverage).
- **ITCZ strength** is divided by iteration count internally, so raw values
  like 0.25 translate to ~0.007 per iteration — modest per-step effect.
- **Condensation rate** (0.20) and **recycle rate** (0.35) were tested at
  higher values but reverted — they amplified precipitation too broadly
  across all latitudes rather than targeting equatorial regions.

### Cross-Seed Validation

Results with the tuned parameters across multiple seeds:

| Seed | Land Tiles | RF % Land | Savanna % | Desert % | Precip Mean |
|------|-----------|-----------|-----------|----------|-------------|
| 42 | ~20,500 | 4.2% | 31.2% | 22.9% | 0.52 |
| 7777777 | ~13,900 | 0.4% | 14.0% | 22.4% | 0.52 |
| 123456 | ~25,800 | 6.7% | 25.1% | 18.9% | 0.51 |
| 999999 | ~11,900 | 3.3% | 16.7% | 28.7% | 0.53 |
| 314159 | ~25,400 | 3.3% | 28.7% | 18.3% | 0.51 |

Rainforest consistently appears at 0.4–6.7% of land tiles (previously
0.002–0.05%). Ocean-heavy worlds (seed 7777777) naturally have fewer
equatorial land tiles and thus lower absolute RF counts.

## Pipeline Stage

`generateClimateStage` — runs the full climate model over the terrain.

## Convergence

Climate and biome classification iterate until stable (the convergence
loop). Vegetation albedo feeds back into temperature, which feeds back
into biome assignment.

## Design Notes

At 1063 LOC this module has been partially split (Config, Evaporation,
ITCZ moved to sub-modules) but the generation logic core remains large.
