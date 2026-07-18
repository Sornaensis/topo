{-# LANGUAGE StrictData #-}

module Seer.OverlayInspector.Executor.Types
  ( OverlayInspectorExecutor(..)
  , OverlayInspectorExecutorState(..)
  , OverlayInspectorWorker(..)
  ) where

import Control.Concurrent (MVar, ThreadId)
import qualified Data.Map.Strict as Map
import Seer.OverlayInspector.Model

data OverlayInspectorExecutor = OverlayInspectorExecutor
  { oieState :: !(MVar OverlayInspectorExecutorState)
  , oieBeginAction :: !(OverlayInspectorAction -> IO OverlayInspectorBeginResult)
  , oieCompleteRequest :: !(OverlayInspectorCompletion -> IO Bool)
  }

data OverlayInspectorExecutorState = OverlayInspectorExecutorState
  { oiesClosing :: !Bool
  , oiesWorkers :: !(Map.Map OverlayInspectorRequestId OverlayInspectorWorker)
  }

data OverlayInspectorWorker = OverlayInspectorWorker
  { oiwThread :: !ThreadId
  , oiwDone :: !(MVar ())
  , oiwRequest :: !OverlayInspectorRequest
  }
