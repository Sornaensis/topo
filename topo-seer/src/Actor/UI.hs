module Actor.UI
  ( module Actor.UI.Setters
  , Ui
  , ConfigTab(..)
  , configRowCount
  , LeftTab(..)
  , UiMenuMode(..)
  , ViewMode(..)
  , UiState(..)
  , emptyUiState
  , uiActorDef
  , UiSnapshotReply
  , requestUiSnapshot
  , getUiSnapshot
  ) where

import Actor.UI.Setters
import Actor.UI.State
  ( ConfigTab(..)
  , LeftTab(..)
  , Ui
  , UiMenuMode(..)
  , UiSnapshotReply
  , UiState(..)
  , ViewMode(..)
  , configRowCount
  , emptyUiState
  , getUiSnapshot
  , requestUiSnapshot
  , uiActorDef
  )