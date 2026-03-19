{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | UI action dispatch actor that runs UI-triggered work off the render thread.
module Actor.UiActions
  ( UiActions
  , UiAction(..)
  , ActorHandles(..)
  , UiActionRequest(..)
  , uiActionsActorDef
  , submitUiAction
  ) where

import Actor.Log (LogEntry)
import Actor.Terrain
  ( TerrainGenProgress
  , TerrainGenResult
  , TerrainReplyOps
  )
import Actor.UiActions.Command
  ( ActorHandles(..)
  , UiAction(..)
  , UiActionRequest(..)
  , runUiAction
  )
import Actor.UiActions.Terrain
  ( UiActionHandles(..)
  , applyTerrainResult
  , handleTerrainLog
  , handleTerrainProgress
  )
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)


data UiActionsState = UiActionsState
  { uasRunning :: !Bool
  , uasHandles :: !(Maybe UiActionHandles)
  }

emptyUiActionsState :: UiActionsState
emptyUiActionsState = UiActionsState
  { uasRunning = False
  , uasHandles = Nothing
  }

[hyperspace|
actor UiActions
  state UiActionsState
  lifetime Singleton
  schedule pinned 1
  noDeps

  reply TerrainReplyOps

  mailbox Unbounded

  cast run :: UiActionRequest
  cast progress :: TerrainGenProgress
  cast result :: TerrainGenResult
  cast logMessage :: LogEntry

  initial emptyUiActionsState
  on_ run = \req st -> do
    runUiAction req
    pure (rememberHandles req st)
  on_ progress = \progressMsg st -> do
    withHandles st (\handles -> handleTerrainProgress handles progressMsg)
    pure st
  on_ result = \resultMsg st -> do
    withHandles st (\handles -> applyTerrainResult handles resultMsg)
    pure st
  on_ logMessage = \entry st -> do
    withHandles st (\handles -> handleTerrainLog handles entry)
    pure st
|]

submitUiAction :: ActorHandle UiActions (Protocol UiActions) -> UiActionRequest -> IO ()
submitUiAction handle req =
  cast @"run" handle #run req

rememberHandles :: UiActionRequest -> UiActionsState -> UiActionsState
rememberHandles req st =
  st
    { uasHandles = Just UiActionHandles
        { uahLog = ahLogHandle handles
        , uahData = ahDataHandle handles
        , uahUi = ahUiHandle handles
        , uahAtlas = ahAtlasManagerHandle handles
        , uahDataSnapshotRef = ahDataSnapshotRef handles
        , uahTerrainSnapshotRef = ahTerrainSnapshotRef handles
        , uahSnapshotVersionRef = ahSnapshotVersionRef handles
        }
    }
  where
    handles = uarActorHandles req

withHandles :: UiActionsState -> (UiActionHandles -> IO ()) -> IO ()
withHandles st action =
  case uasHandles st of
    Nothing -> pure ()
    Just handles -> action handles
