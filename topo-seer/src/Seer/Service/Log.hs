{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Log
  ( LogService(..)
  , logServiceGroup
  , logServiceOperationSpecs
  ) where

import Seer.Service.Types

data LogService = LogService
  { logGet :: !ServiceHandler
  }

logServiceGroup :: ServiceGroupSpec
logServiceGroup = ServiceGroupSpec "logs" logServiceOperationSpecs

logServiceOperationSpecs :: [ServiceOperationSpec]
logServiceOperationSpecs =
  [ operationSpec "logs.get" "get_logs" "Read application log entries."
  ]
