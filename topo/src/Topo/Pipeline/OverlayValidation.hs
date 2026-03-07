{-# LANGUAGE OverloadedStrings #-}

module Topo.Pipeline.OverlayValidation
  ( OverlayStageDeps(..)
  , validateOverlayDependencies
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import Topo.Pipeline.Stage (StageId, stageCanonicalName)

data OverlayStageDeps = OverlayStageDeps
  { osdStageId :: !StageId
  , osdReads :: ![Text]
  , osdProduces :: !(Maybe Text)
  }

validateOverlayDependencies :: [Text] -> [OverlayStageDeps] -> Either Text ()
validateOverlayDependencies initialNames stages = go initialOverlays stages
  where
    initialOverlays = Set.fromList initialNames

    go :: Set Text -> [OverlayStageDeps] -> Either Text ()
    go _ [] = Right ()
    go available (stage:rest) = do
      case filter (`Set.notMember` available) (osdReads stage) of
        [] -> Right ()
        (missing:_) ->
          Left
            ("stage " <> stageCanonicalName (osdStageId stage)
              <> " requires missing overlay " <> missing)
      let available' =
            case osdProduces stage of
              Nothing -> available
              Just produced -> Set.insert produced available
      go available' rest