{-# LANGUAGE OverloadedStrings #-}

module Spec.EditorHistory (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Seer.Editor.History
  ( EditAction(..)
  , EditHistory(..)
  , emptyHistory
  , pushEdit
  , undoEdit
  , redoEdit
  )
import Topo (WorldConfig(..))
import Topo.Types (TerrainChunk(..))
import Spec.EditorBrush (emptyTerrainChunk)

spec :: Spec
spec = describe "Editor.History" $ do
  let cfg = WorldConfig { wcChunkSize = 4 }
      chunk0 = emptyTerrainChunk cfg
      chunk1 = chunk0 { tcElevation = U.replicate 16 0.5 }
      action1 = EditAction
        { eaDescription = "Raise"
        , eaOldChunks   = IntMap.singleton 0 chunk0
        , eaNewChunks   = IntMap.singleton 0 chunk1
        }
      action2 = EditAction
        { eaDescription = "Lower"
        , eaOldChunks   = IntMap.singleton 0 chunk1
        , eaNewChunks   = IntMap.singleton 0 chunk0
        }

  ---------------------------------------------------------------------------
  -- emptyHistory
  ---------------------------------------------------------------------------
  describe "emptyHistory" $ do
    it "starts with empty undo and redo stacks" $ do
      let hist = emptyHistory 50
      ehUndo hist `shouldBe` Seq.empty
      ehRedo hist `shouldBe` Seq.empty

    it "clamps maxSize to at least 1" $ do
      ehMaxSize (emptyHistory 0) `shouldBe` 1
      ehMaxSize (emptyHistory (-5)) `shouldBe` 1

  ---------------------------------------------------------------------------
  -- pushEdit
  ---------------------------------------------------------------------------
  describe "pushEdit" $ do
    it "adds an action to the undo stack" $ do
      let hist = pushEdit action1 (emptyHistory 50)
      Seq.length (ehUndo hist) `shouldBe` 1

    it "clears the redo stack on new edit" $ do
      let hist0 = emptyHistory 50
          hist1 = pushEdit action1 hist0
          -- Undo to put action1 on redo stack
          Just (_, hist2) = undoEdit hist1
      Seq.length (ehRedo hist2) `shouldBe` 1
      -- New edit clears redo
      let hist3 = pushEdit action2 hist2
      ehRedo hist3 `shouldBe` Seq.empty

    it "trims oldest entries when exceeding maxSize" $ do
      let hist0 = emptyHistory 2
          hist1 = pushEdit action1 hist0
          hist2 = pushEdit action2 hist1
          hist3 = pushEdit action1 hist2  -- should trim oldest
      Seq.length (ehUndo hist3) `shouldBe` 2

  ---------------------------------------------------------------------------
  -- undoEdit
  ---------------------------------------------------------------------------
  describe "undoEdit" $ do
    it "returns Nothing on empty history" $ do
      undoEdit (emptyHistory 50) `shouldBe` Nothing

    it "pops the most recent action" $ do
      let hist = pushEdit action1 (pushEdit action2 (emptyHistory 50))
      case undoEdit hist of
        Nothing -> expectationFailure "expected Just"
        Just (action, _) -> eaDescription action `shouldBe` "Raise"

    it "moves the undone action to the redo stack" $ do
      let hist = pushEdit action1 (emptyHistory 50)
      case undoEdit hist of
        Nothing -> expectationFailure "expected Just"
        Just (_, hist') -> Seq.length (ehRedo hist') `shouldBe` 1

  ---------------------------------------------------------------------------
  -- redoEdit
  ---------------------------------------------------------------------------
  describe "redoEdit" $ do
    it "returns Nothing when redo stack is empty" $ do
      redoEdit (emptyHistory 50) `shouldBe` Nothing

    it "pops the most recent redo action" $ do
      let hist0 = pushEdit action1 (emptyHistory 50)
          Just (_, hist1) = undoEdit hist0
      case redoEdit hist1 of
        Nothing -> expectationFailure "expected Just"
        Just (action, _) -> eaDescription action `shouldBe` "Raise"

    it "moves the redone action back to undo" $ do
      let hist0 = pushEdit action1 (emptyHistory 50)
          Just (_, hist1) = undoEdit hist0
          Just (_, hist2) = redoEdit hist1
      Seq.length (ehUndo hist2) `shouldBe` 1
      Seq.length (ehRedo hist2) `shouldBe` 0

  ---------------------------------------------------------------------------
  -- Round-trip
  ---------------------------------------------------------------------------
  describe "undo/redo round-trip" $ do
    it "undo then redo preserves the action" $ do
      let hist0 = pushEdit action1 (emptyHistory 50)
          Just (undone, hist1) = undoEdit hist0
          Just (redone, _hist2) = redoEdit hist1
      eaDescription undone `shouldBe` eaDescription redone
      eaOldChunks undone `shouldBe` eaOldChunks redone
      eaNewChunks undone `shouldBe` eaNewChunks redone
