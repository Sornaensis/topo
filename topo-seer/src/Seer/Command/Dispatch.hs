-- | Command dispatch: route 'SeerCommand' methods to handler functions.
module Seer.Command.Dispatch
  ( CommandContext(..)
  , dispatchCommand
  , dispatchCommandMethods
  ) where

import Topo.Command.Types (SeerCommand, SeerResponse)
import Data.Text (Text)

import Seer.Command.Context (CommandContext(..))
import Seer.Command.AppServiceAdapter
  ( appServiceCommandMethods
  , commandAppService
  , dispatchAppServiceCommand
  )

-- | Dispatch a 'SeerCommand' through the command-backed AppService adapter.
dispatchCommand :: CommandContext -> SeerCommand -> IO SeerResponse
dispatchCommand = dispatchAppServiceCommand commandAppService

-- | Command methods supported by the command-backed AppService adapter.
dispatchCommandMethods :: [Text]
dispatchCommandMethods = appServiceCommandMethods commandAppService
