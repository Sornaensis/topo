module Spec.Support.FloatApprox
  ( approxEqAbs
  , approxEqRel
  , approxEqAbsRel
  ) where

-- | Absolute-tolerance float comparison.
approxEqAbs :: Float -> Float -> Float -> Bool
approxEqAbs epsilon expected actual =
  abs (actual - expected) <= epsilon

-- | Relative-tolerance float comparison, scaled by operand magnitude.
approxEqRel :: Float -> Float -> Float -> Bool
approxEqRel epsilon expected actual =
  let scale = max 1 (max (abs expected) (abs actual))
  in abs (actual - expected) <= epsilon * scale

-- | Combined absolute/relative float comparison.
approxEqAbsRel :: Float -> Float -> Float -> Float -> Bool
approxEqAbsRel absEpsilon relEpsilon expected actual =
  approxEqAbs absEpsilon expected actual || approxEqRel relEpsilon expected actual