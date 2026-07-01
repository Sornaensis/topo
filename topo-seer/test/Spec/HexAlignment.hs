module Spec.HexAlignment (spec) where

import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IntMap
import Linear (V2(..))
import Test.Hspec
import Topo (ChunkCoord(..), ChunkId(..), WorldConfig(..), chunkIdFromCoord)
import Actor.AtlasWorker (atlasWorkerPaddedViewport)
import Seer.Draw.Overlay (RectInt(..), hexSpans, transformRect)
import Seer.Render.Viewport (visibleChunkKeys)
import UI.HexGeometry
  ( hexFillRectAt
  , hexSpanTextureSize
  , normalizeHexBounds
  , renderHexRadiusPx
  , screenPixelToAxial
  , screenToAxial
  , transformWorldRect
  )
import UI.Widgets (Rect(..))

spec :: Spec
spec = describe "shared hex alignment" $ do
  it "picks documented center pixels" $ do
    let cases =
          [ ((40, 80), (0, 0))
          , ((50, 80), (1, 0))
          , ((45, 89), (0, 1))
          , ((76, 89), (3, 1))
          , ((30, 98), (-2, 2))
          , ((144, 116), (8, 4))
          ]
    forM_ cases $ \((sx, sy), expected) ->
      screenToAxial renderHexRadiusPx sx sy `shouldBe` expected

  it "picks documented edge and corner pixels" $ do
    let cases =
          [ ((44, 80), (0, 0))
          , ((45, 80), (1, 0))
          , ((44, 77), (0, 0))
          , ((45, 76), (1, -1))
          , ((45, 77), (1, 0))
          ]
    forM_ cases $ \((sx, sy), expected) ->
      screenToAxial renderHexRadiusPx sx sy `shouldBe` expected

  it "does not quantize world coordinates before picking under pan and zoom" $
    screenPixelToAxial renderHexRadiusPx (0.15, 0) 2.0 (90, 160)
      `shouldBe` (0, 0)

  it "uses floor/ceiling pan-zoom rect transforms" $ do
    let worldRect = Rect (V2 34 74, V2 12 12)
        expected = Rect (V2 75 161, V2 27 28)
    transformWorldRect (0.25, -0.5) 2.2 worldRect `shouldBe` expected
    let RectInt overlayPos overlaySize = transformRect (0.25, -0.5) 2.2 (RectInt (V2 34 74) (V2 12 12))
    (overlayPos, overlaySize) `shouldBe` (V2 75 161, V2 27 28)

  it "keeps hover and brush masks on the semantic fill rect" $ do
    let fillRect@(Rect (_, V2 fillW fillH)) = hexFillRectAt renderHexRadiusPx 0 0
    fillRect `shouldBe` Rect (V2 34 74, V2 12 12)
    hexSpanTextureSize (hexSpans renderHexRadiusPx) `shouldBe` (fillW, fillH)

  it "normalizes atlas stage bounds around the fixed origin" $ do
    normalizeHexBounds 10 (hexFillRectAt 10 0 0)
      `shouldBe` hexFillRectAt renderHexRadiusPx 0 0
    normalizeHexBounds 10 (hexFillRectAt 10 1 0)
      `shouldBe` hexFillRectAt renderHexRadiusPx 1 0
    normalizeHexBounds 10 (hexFillRectAt 10 3 0)
      `shouldBe` hexFillRectAt renderHexRadiusPx 3 0
    normalizeHexBounds 10 (hexFillRectAt 10 3 1)
      `shouldBe` hexFillRectAt renderHexRadiusPx 3 1

  it "keeps viewport chunk culling padded around exact fill bounds" $ do
    let cfg = WorldConfig { wcChunkSize = 1 }
        ChunkId key = chunkIdFromCoord (ChunkCoord 0 0)
        visible = visibleChunkKeys cfg (-34, -74) 1.0 (12, 12) (IntMap.singleton key ())
    visible `shouldBe` [key]

  it "pads atlas-worker viewport symmetrically in the base frame" $ do
    let cfg = WorldConfig { wcChunkSize = 1 }
        (paddedPan, paddedZoom, paddedWin) = atlasWorkerPaddedViewport cfg (10, 20) 2.0 (100, 80)
    paddedPan `shouldBe` (34, 44)
    paddedZoom `shouldBe` 2.0
    paddedWin `shouldBe` (196, 176)
