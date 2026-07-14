-- | Command dispatch: route 'SeerCommand' methods to handler functions.
module Seer.Command.Dispatch
  ( CommandContext(..)
  , dispatchCommand
  , dispatchCommandWithService
  , dispatchCommandMethods
  ) where

import Topo.Command.Types (SeerCommand, SeerResponse)
import Data.Text (Text)

import Seer.Command.Context (CommandContext(..))
import Seer.Service.AppService (AppService)
import Seer.Command.AppServiceAdapter
  ( appServiceCommandMethods
  , commandAppService
  , dispatchAppServiceCommand
  )

-- | Dispatch a 'SeerCommand' through the GUI renderer-backed AppService.
dispatchCommand :: CommandContext -> SeerCommand -> IO SeerResponse
dispatchCommand = dispatchCommandWithService commandAppService

-- | Dispatch through the AppService selected by the owning runtime.
dispatchCommandWithService
  :: AppService -> CommandContext -> SeerCommand -> IO SeerResponse
dispatchCommandWithService = dispatchAppServiceCommand

-- | Command methods supported by the command-backed AppService adapter.
dispatchCommandMethods :: [Text]
dispatchCommandMethods = appServiceCommandMethods commandAppService
