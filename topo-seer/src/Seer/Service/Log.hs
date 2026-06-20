{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Log
  ( LogService(..)
  , LogGetRequest(..)
  , LogEntrySummary(..)
  , LogGetResponse(..)
  , logGetOperation
  , logServiceGroup
  , logServiceOperationSpecs
  ) where

import Data.Text (Text)

import Actor.Log (LogLevel)
import Seer.Service.Types

data LogService = LogService
  { logGet :: !ServiceHandler
  }

data LogGetRequest = LogGetRequest
  { logGetMinLevel :: !(Maybe LogLevel)
  , logGetLimit :: !(Maybe Int)
  , logGetOffset :: !(Maybe Int)
  } deriving (Eq, Show)

data LogEntrySummary = LogEntrySummary
  { logEntrySummaryLevel :: !LogLevel
  , logEntrySummaryMessage :: !Text
  } deriving (Eq, Show)

data LogGetResponse = LogGetResponse
  { logGetResponseCount :: !Int
  , logGetResponseTotal :: !Int
  , logGetResponseEntries :: ![LogEntrySummary]
  } deriving (Eq, Show)

logServiceGroup :: ServiceGroupSpec
logServiceGroup = ServiceGroupSpec "logs" logServiceOperationSpecs

logServiceOperationSpecs :: [ServiceOperationSpec]
logServiceOperationSpecs =
  [ typedServiceOperationSpec logGetOperation
  ]

logGetOperation :: TypedServiceOperation LogGetRequest LogGetResponse
logGetOperation = typedOperation $
  operationSpec "logs.get" "get_logs" "Read application log entries."
