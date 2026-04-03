-- | Discrete zoom stages mapping camera zoom levels to atlas geometry parameters.
--
-- At each stage the terrain is baked with a specific @hexRadius@ (pixel
-- radius of each hex in the source geometry) and an @atlasScale@
-- (composition multiplier applied when building the atlas texture).
-- The camera zoom controls pan and display but the baked sizes do not
-- change continuously — they jump between the four stages below.
module Seer.Render.ZoomStage
  ( ZoomStage(..)
  , allZoomStages
  , stageForZoom
  , maxCameraZoom
  ) where

-- | A discrete zoom stage with associated geometry parameters.
data ZoomStage = ZoomStage
  { zsHexRadius  :: !Int
    -- ^ Hex radius in pixels used when baking chunk geometry.
  , zsAtlasScale :: !Int
    -- ^ Composition scale multiplier applied to the baked geometry when
    --   building atlas tiles.
  , zsZoomMin    :: !Float
    -- ^ Inclusive lower bound of the camera zoom range for this stage.
  , zsZoomMax    :: !Float
    -- ^ Exclusive upper bound of the camera zoom range for this stage.
  } deriving (Eq, Ord, Show)

-- | Maximum camera zoom level supported by the renderer.
maxCameraZoom :: Float
maxCameraZoom = 8.0

-- | All zoom stages in ascending zoom-range order.
--
-- Together the ranges cover @[0, maxCameraZoom]@.  Each stage is chosen
-- so that the baked geometry resolution roughly matches the on-screen
-- pixel density, keeping atlas tiles sharp without wasting texture memory.
--
-- The @hexRadius@ values form a geometric ramp (ratio ≈ 1.7×) so the
-- quality increase across stage transitions is perceptually even.  At
-- zoom @z@ the screen pixel size per hex is @renderHexRadiusPx × z = 6z@,
-- and @hexRadius × atlasScale@ should equal or exceed that to be crisp.
--
-- | Stage | Zoom range  | hexRadius | atlasScale | Baked px/hex | Screen px at midpoint |
-- |-------|-------------|-----------|------------|--------------|----------------------|
-- | 0     | 0.0 – 1.0   | 6         | 1          | 6            | 3                    |
-- | 1     | 1.0 – 2.2   | 10        | 2          | 20           | 10.5                 |
-- | 2     | 2.2 – 4.0   | 18        | 2          | 36           | 21                   |
-- | 3     | 4.0 – 6.0   | 32        | 1          | 32           | 30                   |
-- | 4     | 6.0 – 8.0   | 50        | 1          | 50           | 42                   |
allZoomStages :: [ZoomStage]
allZoomStages =
  [ ZoomStage { zsHexRadius = 6,  zsAtlasScale = 1, zsZoomMin = 0.0, zsZoomMax = 1.0 }
  , ZoomStage { zsHexRadius = 10, zsAtlasScale = 2, zsZoomMin = 1.0, zsZoomMax = 2.2 }
  , ZoomStage { zsHexRadius = 18, zsAtlasScale = 2, zsZoomMin = 2.2, zsZoomMax = 4.0 }
  , ZoomStage { zsHexRadius = 32, zsAtlasScale = 1, zsZoomMin = 4.0, zsZoomMax = 6.0 }
  , ZoomStage { zsHexRadius = 50, zsAtlasScale = 1, zsZoomMin = 6.0, zsZoomMax = 8.0 }
  ]

-- | Select the zoom stage for the given camera zoom level.
--
-- The zoom is clamped to @[0, maxCameraZoom]@.  If no stage's upper bound
-- exceeds the clamped zoom (i.e. at exactly @maxCameraZoom@), the last
-- stage is returned.
stageForZoom :: Float -> ZoomStage
stageForZoom z =
  let clamped = max 0.0 (min maxCameraZoom z)
  in case filter (\s -> clamped < zsZoomMax s) allZoomStages of
       (s:_) -> s
       []    -> last allZoomStages
