# Terrain Forms & Modifiers

> **Modules:** `Topo.Parameters.TerrainForm` (classification), `Topo.TerrainForm.Modifiers` (modifier tables)

## Overview

Terrain form classification assigns one of 15 geomorphic types to each hex tile
based on slope, relief, curvature, substrate hardness, micro-relief, and
elevation above sea level. The classifier is deterministic, total, and
guard-ordered — the first matching guard wins.

Modifier tables then assign per-form multipliers consumed by downstream stages
(erosion, glacier, hydrology).

## Terrain Forms (15 types)

| Form | Primary inputs | Geomorphic meaning |
|------|---------------|-------------------|
| `FormCliff` | maxSlope > `tfcCliffSlope` | Near-vertical faces |
| `FormCanyon` | negative curvature + high 3-ring relief + steep opposing walls + hard rock | Deep incisions in resistant substrate |
| `FormBadlands` | high maxSlope + soft substrate + slope asymmetry | Heavily eroded soft-rock terrain |
| `FormMountainous` | high top-3 slope OR high 3-ring relief | Major mountain terrain |
| `FormPass` | ridge-like cross-profile + local minimum | Saddle between peaks |
| `FormRidge` | steep opposing slopes + low axial slope | Linear crest feature |
| `FormEscarpment` | steep one face + gentle opposite | Asymmetric cliff/scarp |
| `FormMesa` | flat top + high 2-ring edge relief + hard cap rock + elevation | Flat-topped isolated elevation |
| `FormPlateau` | low slope + elevation + low 2-ring relief + low micro-relief | Extensive elevated flat |
| `FormValley` | negative curvature | Topographic low between slopes |
| `FormDepression` | local minimum (no curvature match) | Enclosed basin or sink |
| `FormFoothill` | moderate slope + elevation band | Transition between plains and mountains |
| `FormHilly` | moderate slope + 2-ring relief (or micro-relief assisted) | Rolling terrain with notable relief |
| `FormRolling` | average slope above threshold (or micro-relief assisted) | Gently undulating terrain |
| `FormFlat` | default / no other guard matches | Level or near-level terrain |

## Classification Cascade

Guards are evaluated top-to-bottom. The first match determines the form.
The ordering groups steep/macroscale features first, then meso-scale,
then low-relief promotions.

```
┌─ maxSlope > cliffSlope ──────────────────────► FormCliff
│
├─ valley curvature + 3-ring relief              FormCanyon
│  + steep opposing walls + hard rock ──────────►
│
├─ high maxSlope + soft rock + asymmetry ───────► FormBadlands
│
├─ high top3Slope OR high 3-ring relief ────────► FormMountainous
│
├─ ridge cross-profile + local minimum ─────────► FormPass
├─ steep opposing pair + low axial slope ───────► FormRidge
├─ steep one face + gentle opposite ────────────► FormEscarpment
│
├─ flat top + edge relief + hard cap + elev ────► FormMesa
│
├─ low slope + elev + low relief2                 FormPlateau
│  + microRelief ≤ plateauMaxMicroRelief ──────►
│
├─ negative curvature ─────────────────────────► FormValley
├─ local minimum ──────────────────────────────► FormDepression
│
├─ moderate slope + elevation band ─────────────► FormFoothill
│
├─ slope/relief above hilly threshold           ┐
│  OR micro-relief-assisted hilly promotion ────► FormHilly
│
├─ avgSlope > rollingSlope                      ┐
│  OR near-rolling + high micro-relief ─────────► FormRolling
│
└─ otherwise ──────────────────────────────────► FormFlat
```

## Flat → Plateau → Rolling/Hilly Transition Flow

The most common misclassification pattern is **over-plateauing**: tiles that
should be rolling or hilly end up as plateau because they have low slope and
sufficient elevation. Two mechanisms control this:

### 1. Plateau inhibition via micro-relief cap (secondary lever)

The plateau guard requires `microRelief ≤ tfcPlateauMaxMicroRelief`. Tiles
with significant sub-tile texture are rejected from plateau even when slope
and elevation qualify. These tiles fall through to the hilly/rolling guards
below.

**Knob:** `tfcPlateauMaxMicroRelief` (default: 0.5)

**Caution:** Lowering this aggressively converts rough plateaus to **flat**
(the default fallback), not to rolling/hilly, unless the rolling/hilly
promotion thresholds are also relaxed.

### 2. Rolling/hilly promotion via micro-relief (primary lever)

Tiles near but below the rolling slope threshold can be promoted to
`FormRolling` if their micro-relief is high enough:

```
isRolling = avgSlope > rollingSlope
         || (avgSlope > rollingSlope * rollingNearFactor
             && effectiveMicroRelief >= microReliefRollingMin)
```

Similarly, tiles can be promoted to `FormHilly` when micro-relief is
high enough, using relaxed slope and relief thresholds:

```
isHilly = (top3Slope > hillSlope && relief2 > hillRelief)
       || (effectiveMicroRelief >= microReliefHillyMin
           && top3Slope > hillSlope * hillySlopeScale
           && relief2 > hillRelief * hillyReliefScale)
```

**Knobs:**
- `tfcRollingNearFactor` (default: 0.85) — how close to the rolling
  threshold a tile must be for micro-relief to promote it.
- `tfcMicroReliefRollingMin` (default: 0.62) — minimum micro-relief for
  rolling promotion.
- `tfcMicroReliefHillyMin` (default: 0.70) — minimum micro-relief for
  hilly promotion.
- `tfcMicroReliefHillySlopeScale` (default: 0.85) — slope threshold
  relaxation for micro-relief-assisted hilly.
- `tfcMicroReliefHillyReliefScale` (default: 0.85) — relief threshold
  relaxation for micro-relief-assisted hilly.

### 3. Hardness-gated attenuation

Micro-relief contribution is attenuated in soft substrate to prevent
noise-driven over-promotion in sedimentary lowlands:

```
effectiveMicroRelief
  | hardness < softHardnessThreshold = microRelief * softAttenuation
  | otherwise                        = microRelief
```

**Knobs:**
- `tfcMicroReliefSoftHardnessThreshold` (default: 0.35) — hardness below
  which attenuation applies.
- `tfcMicroReliefSoftAttenuation` (default: 0.75) — multiplicative damping
  factor.

This keeps hardness **orthogonal** to the stored micro-relief index — the
attenuation is applied in the classifier, not baked into `tcMicroRelief`.

## Modifier Tables

Each terrain form has multiplier values consumed by:
- Erosion (hydraulic rate, thermal rate)
- Glacier (accumulation rate, melt rate)
- Hydrology (infiltration, runoff)

`TerrainFormModifiersConfig` allows tuning per-form multipliers
without changing generator logic. Currently rolling, hilly, and foothill
use `neutralModifiers` (all 1.0). If their class share grows
significantly under the new classifier, non-neutral modifiers may be
needed to maintain erosion balance.

## Configuration

All classification thresholds are in `TerrainFormConfig`. Slope
thresholds are in **physical-slope space**: raw normalised elevation
deltas multiplied by `tfcElevGradient` (default: 0.574). This
conversion is applied once inside `classifyTerrainForm`.
