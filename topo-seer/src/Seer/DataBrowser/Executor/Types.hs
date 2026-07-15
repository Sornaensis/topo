{-# LANGUAGE StrictData #-}

module Seer.DataBrowser.Executor.Types
  ( DataBrowserExecutor(..)
  , ExecutorState(..)
  , Worker(..)
  ) where

import Actor.UI.State (Ui)
import Control.Concurrent (MVar, ThreadId)
import qualified Data.Map.Strict as Map
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.DataBrowser.Lifecycle (DataBrowserAppAction)
import Seer.DataBrowser.Model
  ( DataBrowserBeginResult
  , DataBrowserCompletion
  , DataBrowserRequestId
  , DataBrowserWorkerRequest
  )

data DataBrowserExecutor = DataBrowserExecutor
  { dbeUiHandle :: !(ActorHandle Ui (Protocol Ui))
  , dbeState :: !(MVar ExecutorState)
  , dbeBeginAction :: !(DataBrowserAppAction -> IO DataBrowserBeginResult)
  , dbeCompleteRequest :: !(DataBrowserCompletion -> IO Bool)
  }

data ExecutorState = ExecutorState
  { esClosing :: !Bool
  , esSubmissions :: ![MVar ()]
  , esWorkers :: !(Map.Map DataBrowserRequestId Worker)
  }

data Worker = Worker
  { workerThread :: !ThreadId
  , workerDone :: !(MVar ())
  , workerRequest :: !DataBrowserWorkerRequest
  }
