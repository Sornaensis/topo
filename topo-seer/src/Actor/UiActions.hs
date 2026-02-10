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
  , UiActionRequest(..)
  , uiActionsActorDef
  , submitUiAction
  , setUiActionsSnapshotRef
  ) where

import Actor.Log (LogEntry)
import Actor.SnapshotReceiver (SnapshotRef)
import Actor.Terrain
  ( TerrainGenProgress
  , TerrainGenResult
  )
import Actor.UiActions.Command
  ( UiAction(..)
  , UiActionRequest(..)
  , runUiAction
  )
import Actor.UiActions.Protocol (TerrainReplyOps)
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
  , uasSnapshotRef :: !(Maybe SnapshotRef)
  }

emptyUiActionsState :: UiActionsState
emptyUiActionsState = UiActionsState
  { uasRunning = False
  , uasHandles = Nothing
  , uasSnapshotRef = Nothing
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
  cast setSnapshotRef :: SnapshotRef

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
  onPure_ setSnapshotRef = \ref st -> st { uasSnapshotRef = Just ref }
|]

submitUiAction :: ActorHandle UiActions (Protocol UiActions) -> UiActionRequest -> IO ()
submitUiAction handle req =
  cast @"run" handle #run req

-- | Register the snapshot IORef for direct writes from terrain completion.
setUiActionsSnapshotRef :: ActorHandle UiActions (Protocol UiActions) -> SnapshotRef -> IO ()
setUiActionsSnapshotRef handle ref =
  cast @"setSnapshotRef" handle #setSnapshotRef ref

rememberHandles :: UiActionRequest -> UiActionsState -> UiActionsState
rememberHandles req st =
  st
    { uasHandles = Just UiActionHandles
        { uahLog = uarLogHandle req
        , uahData = uarDataHandle req
        , uahUi = uarUiHandle req
        , uahAtlas = uarAtlasHandle req
        , uahSnapshot = uarSnapshotHandle req
        , uahSnapshotRef = uasSnapshotRef st
        }
    }

withHandles :: UiActionsState -> (UiActionHandles -> IO ()) -> IO ()
withHandles st action =
  case uasHandles st of
    Nothing -> pure ()
    Just handles -> action handles
