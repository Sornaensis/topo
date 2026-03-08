{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Terrain-form configuration and classification helpers.
module Topo.Parameters.TerrainForm
  ( TerrainFormConfig(..)
  , defaultTerrainFormConfig
  , computeReliefIndex
  , classifyTerrainForm
  ) where

import Data.List (foldl')
import GHC.Generics (Generic)
import Topo.Config.JSON
  ( FromJSON(..)
  , ToJSON(..)
  , configOptions
  , genericParseJSON
  , genericToJSON
  , mergeDefaults
  )
import Topo.Hex (HexDirection(..), allHexDirections, dsSlopeIn, hexOpposite)
import Topo.Math (clamp01)
import Topo.Types

-- | Terrain form classification thresholds.
--
-- All slope thresholds are in /physical-slope/ space: raw normalised
-- elevation deltas multiplied by 'tfcElevGradient' (default 0.574).
-- This converts unit-interval deltas to approximate rise/run ratios.
-- The conversion is applied once inside 'classifyTerrainForm'.
--
-- Relief thresholds for hill/mountain/canyon/mesa use multi-ring
-- neighbourhoods ('tcRelief2Ring', 'tcRelief3Ring') to capture regional
-- context beyond the 6-tile 1-ring neighbourhood.
data TerrainFormConfig = TerrainFormConfig
  { tfcElevGradient  :: !Float
  -- ^ Elevation gradient scale factor. Raw normalised slope deltas are
  --   multiplied by this to produce physical-slope values used by all
  --   threshold comparisons (default: 0.574, derived from 'usElevGradient').
  , tfcCliffSlope      :: !Float
  -- ^ Physical slope above which terrain is classified as 'FormCliff'.
  , tfcMountainSlope   :: !Float
  -- ^ Physical top-3 slope threshold for 'FormMountainous'.
  , tfcMountainRelief  :: !Float
  -- ^ 3-ring relief threshold for 'FormMountainous'.
  , tfcValleyCurvature :: !Float
  -- ^ Curvature magnitude for 'FormValley' (positive value; compared as
  --   curvature < -this).
  , tfcHillSlope       :: !Float
  -- ^ Physical top-3 slope threshold for 'FormHilly'.
  , tfcHillRelief      :: !Float
  -- ^ 2-ring relief threshold for 'FormHilly'.
  , tfcRollingSlope    :: !Float
  -- ^ Physical average slope threshold for 'FormRolling'.
  , tfcRollingNearFactor :: !Float
  -- ^ Near-threshold factor for micro-relief-assisted rolling
  -- promotion. Applied as @tfcRollingSlope * tfcRollingNearFactor@.

  -- Ridge thresholds
  , tfcRidgeMinSlope     :: !Float
  -- ^ Min physical slope on the steep axis for 'FormRidge'.
  , tfcRidgeMaxAxisSlope :: !Float
  -- ^ Max physical slope along the ridge axis.
  , tfcRidgeMinAsymmetry :: !Float
  -- ^ Min asymmetry between steep and axial physical slopes.

  -- Escarpment thresholds
  , tfcEscarpmentMinSlope    :: !Float
  -- ^ Min physical slope on the steep face for 'FormEscarpment'.
  , tfcEscarpmentMaxOppSlope :: !Float
  -- ^ Max physical slope on the opposite (gentle) face.

  -- Plateau thresholds
  , tfcPlateauMaxSlope       :: !Float
  -- ^ Max physical slope anywhere for 'FormPlateau'.
  , tfcPlateauMinElevASL     :: !Float
  -- ^ Min elevation above sea level for 'FormPlateau'.
  , tfcPlateauMaxRelief2Ring :: !Float
  -- ^ Max 2-ring relief for 'FormPlateau'. Prevents gentle mountain
  --   slopes from being classified as plateau (default: 0.03).
  , tfcPlateauMaxMicroRelief :: !Float
  -- ^ Max micro-relief for 'FormPlateau'. Inhibits over-classification
  --   of rough elevated flats as plateau.

  -- Badlands thresholds
  , tfcBadlandsMinMaxSlope  :: !Float
  -- ^ Min physical 'dsMaxSlope' for 'FormBadlands'.
  , tfcBadlandsMaxHardness  :: !Float
  -- ^ Max substrate hardness; badlands form in soft rock.
  , tfcBadlandsMinAsymmetry :: !Float
  -- ^ Min slope asymmetry for 'FormBadlands'.

  -- Pass thresholds
  , tfcPassMaxAxisSlope  :: !Float
  -- ^ Max physical slope along the pass axis.
  , tfcPassMinCrossSlope :: !Float
  -- ^ Min physical slope perpendicular to the axis.

  -- Canyon thresholds
  , tfcCanyonMinRelief    :: !Float
  -- ^ Min 3-ring relief for 'FormCanyon'.
  , tfcCanyonMinWallSlope :: !Float
  -- ^ Min physical slope on steep walls.
  , tfcCanyonMinHardness  :: !Float
  -- ^ Min hardness; canyons carve through hard rock.

  -- Mesa thresholds
  , tfcMesaMaxTopSlope   :: !Float
  -- ^ Max physical slope on the mesa top.
  , tfcMesaMinEdgeRelief :: !Float
  -- ^ Min 2-ring relief at mesa edges.
  , tfcMesaMinHardness   :: !Float
  -- ^ Min hardness; cap rock.
  , tfcMesaMinElevASL    :: !Float
  -- ^ Min elevation above sea level.

  -- Foothill thresholds
  , tfcFoothillMinSlope  :: !Float
  -- ^ Min physical top-3 slope for 'FormFoothill'.
  , tfcFoothillMaxSlope  :: !Float
  -- ^ Max physical top-3 slope for 'FormFoothill'.
  , tfcFoothillMinElevASL :: !Float
  -- ^ Min elevation above sea level.
  , tfcFoothillMaxElevASL :: !Float
  -- ^ Max elevation; above this is mountain, not foothill.
  , tfcMicroReliefRollingMin :: !Float
  -- ^ Minimum micro-relief needed for low-slope tiles near the rolling
  --   threshold to classify as 'FormRolling'.
  , tfcMicroReliefHillyMin :: !Float
  -- ^ Minimum micro-relief needed for micro-relief-assisted hilly
  --   classification.
  , tfcMicroReliefHillySlopeScale :: !Float
  -- ^ Scale applied to the hilly slope threshold when micro-relief is high.
  , tfcMicroReliefHillyReliefScale :: !Float
  -- ^ Scale applied to the hilly 2-ring relief threshold when micro-relief
  --   is high.
  , tfcMicroReliefSoftHardnessThreshold :: !Float
  -- ^ If hardness is below this threshold, micro-relief contribution is
  --   attenuated in near-threshold rolling/hilly promotion.
  , tfcMicroReliefSoftAttenuation :: !Float
  -- ^ Multiplicative attenuation applied to micro-relief when hardness is
  --   below 'tfcMicroReliefSoftHardnessThreshold'.
  } deriving (Eq, Show, Generic)

instance ToJSON TerrainFormConfig where
  toJSON = genericToJSON (configOptions "tfc")

instance FromJSON TerrainFormConfig where
  parseJSON v = genericParseJSON (configOptions "tfc")
                  (mergeDefaults (toJSON defaultTerrainFormConfig) v)

-- | Default terrain form thresholds.
--
-- All slope values are in physical-slope space (raw delta × 0.574).
-- Relief values are in normalised elevation units @[0,1]@.
defaultTerrainFormConfig :: TerrainFormConfig
defaultTerrainFormConfig = TerrainFormConfig
  { tfcElevGradient    = 0.574
  , tfcCliffSlope      = 0.15
  , tfcMountainSlope   = 0.06
  , tfcMountainRelief  = 0.08
  , tfcValleyCurvature = 0.15
  , tfcHillSlope       = 0.025
  , tfcHillRelief      = 0.04
  , tfcRollingSlope    = 0.008
  , tfcRollingNearFactor = 0.85
  , tfcRidgeMinSlope     = 0.07
  , tfcRidgeMaxAxisSlope = 0.035
  , tfcRidgeMinAsymmetry = 0.045
  , tfcEscarpmentMinSlope    = 0.085
  , tfcEscarpmentMaxOppSlope = 0.023
  , tfcPlateauMaxSlope       = 0.015
  , tfcPlateauMinElevASL     = 0.08
  , tfcPlateauMaxRelief2Ring = 0.03
  , tfcPlateauMaxMicroRelief = 0.5
  , tfcBadlandsMinMaxSlope  = 0.085
  , tfcBadlandsMaxHardness  = 0.35
  , tfcBadlandsMinAsymmetry = 0.035
  , tfcPassMaxAxisSlope  = 0.023
  , tfcPassMinCrossSlope = 0.057
  , tfcCanyonMinRelief    = 0.08
  , tfcCanyonMinWallSlope = 0.10
  , tfcCanyonMinHardness  = 0.40
  , tfcMesaMaxTopSlope   = 0.015
  , tfcMesaMinEdgeRelief = 0.05
  , tfcMesaMinHardness   = 0.45
  , tfcMesaMinElevASL    = 0.05
  , tfcFoothillMinSlope   = 0.015
  , tfcFoothillMaxSlope   = 0.055
  , tfcFoothillMinElevASL = 0.02
  , tfcFoothillMaxElevASL = 0.12
  , tfcMicroReliefRollingMin = 0.62
  , tfcMicroReliefHillyMin = 0.70
  , tfcMicroReliefHillySlopeScale = 0.85
  , tfcMicroReliefHillyReliefScale = 0.85
  , tfcMicroReliefSoftHardnessThreshold = 0.35
  , tfcMicroReliefSoftAttenuation = 0.75
  }

-- | Compute the fused sub-tile relief index in @[0,1]@.
--
-- The ring-relief term is always available as the base signal. Optional
-- noise and erosion terms are fused with explicit non-negative weights.
--
-- If both optional weights are zero (or both optional terms are absent),
-- this function returns the ring-only fallback used by pre-erosion
-- classification.
computeReliefIndex
  :: Float
  -> Float
  -> Float
  -> Maybe Float
  -> Maybe Float
  -> Float
  -> Float
  -> Float
computeReliefIndex relief1 relief2 relief3 noiseTerm erosionTerm noiseWeight erosionWeight =
  let !ringTerm = clamp01 ((relief1 + relief2 + relief3) / 3)
      !wNoise = max 0 noiseWeight
      !wErosion = max 0 erosionWeight
      !weightedSum =
        maybe 0 (\value -> wNoise * clamp01 value) noiseTerm
          + maybe 0 (\value -> wErosion * clamp01 value) erosionTerm
      !weightTotal =
        maybe 0 (const wNoise) noiseTerm
          + maybe 0 (const wErosion) erosionTerm
  in if weightTotal <= 0
       then ringTerm
       else clamp01 ((ringTerm + weightedSum) / (1 + weightTotal))

-- | Classify terrain form from directional slope, multi-ring relief,
-- curvature, local-minimum flag, substrate properties, and elevation
-- above sea level.
--
-- All slope comparisons use physical-slope values obtained by scaling the
-- raw normalised 'DirectionalSlope' by 'tfcElevGradient'.
--
-- Decision cascade (first match wins):
--
--    1. 'FormCliff'       — @physMaxS > cliffThreshold@
--    2. 'FormCanyon'      — valley curvature + high 3-ring relief + steep
--                           opposite walls + hard rock
--    3. 'FormBadlands'    — high max physical slope + soft substrate + high
--                           asymmetry
--    4. 'FormMountainous' — high top-3 physical slope OR high 3-ring relief
--    5. 'FormPass'        — ridge-like cross-axis profile + local minimum
--    6. 'FormRidge'       — steep on opposite pair, flat along axis
--    7. 'FormEscarpment'  — steep face on one side, gentle on opposite
--    8. 'FormMesa'        — flat top + steep 2-ring relief + hard cap + elevated
--    9. 'FormPlateau'     — low physical slope + high elevation + low 2-ring relief
--   10. 'FormValley'      — strongly negative curvature
--   11. 'FormDepression'  — local minimum
--   12. 'FormFoothill'    — moderate top-3 slope + moderate elevation
--   13. 'FormHilly'       — moderate top-3 slope + 2-ring relief
--   14. 'FormRolling'     — physical avg slope above rolling threshold
--   15. 'FormFlat'        — otherwise
{-# INLINE classifyTerrainForm #-}
classifyTerrainForm
  :: TerrainFormConfig
  -> DirectionalSlope
  -> Float
  -> Float
  -> Float
  -> Float
  -> Bool
  -> Float
  -> Float
  -> Float
  -> TerrainForm
classifyTerrainForm cfg ds _relief1 relief2 relief3 curvature localMin hardness microRelief elevASL
  | maxSlope > tfcCliffSlope cfg
  = FormCliff
  | curvature < negate (tfcValleyCurvature cfg)
    && relief3 > tfcCanyonMinRelief cfg
    && hasSteepOppositePair (tfcCanyonMinWallSlope cfg)
    && hardness >= tfcCanyonMinHardness cfg
  = FormCanyon
  | maxSlope > tfcBadlandsMinMaxSlope cfg
    && hardness <= tfcBadlandsMaxHardness cfg
    && asymmetry > tfcBadlandsMinAsymmetry cfg
  = FormBadlands
  | top3Slope > tfcMountainSlope cfg || relief3 > tfcMountainRelief cfg
  = FormMountainous
  | localMin && hasPassProfile
  = FormPass
  | hasRidgeProfile
  = FormRidge
  | hasEscarpmentProfile
  = FormEscarpment
  | maxSlope <= tfcMesaMaxTopSlope cfg
    && relief2 > tfcMesaMinEdgeRelief cfg
    && hardness >= tfcMesaMinHardness cfg
    && elevASL >= tfcMesaMinElevASL cfg
  = FormMesa
  | maxSlope <= tfcPlateauMaxSlope cfg
    && elevASL >= tfcPlateauMinElevASL cfg
    && relief2 <= tfcPlateauMaxRelief2Ring cfg
    && microRelief <= tfcPlateauMaxMicroRelief cfg
  = FormPlateau
  | curvature < negate (tfcValleyCurvature cfg)
  = FormValley
  | localMin
  = FormDepression
  | top3Slope >= tfcFoothillMinSlope cfg && top3Slope <= tfcFoothillMaxSlope cfg
    && elevASL >= tfcFoothillMinElevASL cfg
    && elevASL <= tfcFoothillMaxElevASL cfg
  = FormFoothill
  | isHilly
  = FormHilly
  | isRolling
  = FormRolling
  | otherwise
  = FormFlat
  where
    !gradientScale = tfcElevGradient cfg
    !physicalSlope = DirectionalSlope
      (dsSlopeE ds * gradientScale)
      (dsSlopeNE ds * gradientScale)
      (dsSlopeNW ds * gradientScale)
      (dsSlopeW ds * gradientScale)
      (dsSlopeSW ds * gradientScale)
      (dsSlopeSE ds * gradientScale)
    !avgSlope = dsAvgSlope physicalSlope
    !maxSlope = dsMaxSlope physicalSlope
    !top3Slope = dsTop3Slope physicalSlope
    !asymmetry = dsAsymmetry physicalSlope
    !effectiveMicroRelief
      | hardness < tfcMicroReliefSoftHardnessThreshold cfg =
          clamp01 (microRelief * tfcMicroReliefSoftAttenuation cfg)
      | otherwise = microRelief
    !rollingSlopeNear = tfcRollingSlope cfg * tfcRollingNearFactor cfg
    !isRolling =
      avgSlope > tfcRollingSlope cfg
        || ( avgSlope > rollingSlopeNear
             && effectiveMicroRelief >= tfcMicroReliefRollingMin cfg
           )
    !isHilly =
      (top3Slope > tfcHillSlope cfg && relief2 > tfcHillRelief cfg)
        || ( effectiveMicroRelief >= tfcMicroReliefHillyMin cfg
             && top3Slope > tfcHillSlope cfg * tfcMicroReliefHillySlopeScale cfg
             && relief2 > tfcHillRelief cfg * tfcMicroReliefHillyReliefScale cfg
           )

    hasSteepOppositePair :: Float -> Bool
    hasSteepOppositePair threshold =
      any
        (\direction ->
          let opposite = hexOpposite direction
              !slopeA = abs (dsSlopeIn direction physicalSlope)
              !slopeB = abs (dsSlopeIn opposite physicalSlope)
          in slopeA >= threshold && slopeB >= threshold
        )
        [HexE, HexNE, HexNW]

    hasRidgeProfile :: Bool
    hasRidgeProfile =
      any
        (\direction ->
          let opposite = hexOpposite direction
              !slopeA = abs (dsSlopeIn direction physicalSlope)
              !slopeB = abs (dsSlopeIn opposite physicalSlope)
              !axisMaxSlope = maxExcludingPair direction opposite
          in slopeA >= tfcRidgeMinSlope cfg
               && slopeB >= tfcRidgeMinSlope cfg
               && axisMaxSlope <= tfcRidgeMaxAxisSlope cfg
               && (slopeA + slopeB) / 2 - axisMaxSlope >= tfcRidgeMinAsymmetry cfg
        )
        [HexE, HexNE, HexNW]

    hasPassProfile :: Bool
    hasPassProfile =
      any
        (\direction ->
          let opposite = hexOpposite direction
              !slopeA = abs (dsSlopeIn direction physicalSlope)
              !slopeB = abs (dsSlopeIn opposite physicalSlope)
              !crossAverage = (slopeA + slopeB) / 2
          in crossAverage >= tfcPassMinCrossSlope cfg
               && maxExcludingPair direction opposite <= tfcPassMaxAxisSlope cfg
        )
        [HexE, HexNE, HexNW]

    hasEscarpmentProfile :: Bool
    hasEscarpmentProfile =
      any
        (\direction ->
          let opposite = hexOpposite direction
              !slopeA = abs (dsSlopeIn direction physicalSlope)
              !slopeB = abs (dsSlopeIn opposite physicalSlope)
          in slopeA >= tfcEscarpmentMinSlope cfg
               && slopeB <= tfcEscarpmentMaxOppSlope cfg
        )
        allHexDirections

    maxExcludingPair :: HexDirection -> HexDirection -> Float
    maxExcludingPair excludedA excludedB =
      foldl'
        (\acc direction ->
          if direction == excludedA || direction == excludedB
            then acc
            else max acc (abs (dsSlopeIn direction physicalSlope))
        )
        0
        allHexDirections