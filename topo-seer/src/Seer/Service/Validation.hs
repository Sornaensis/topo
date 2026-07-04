{-# LANGUAGE OverloadedStrings #-}

module Seer.Service.Validation
  ( RequestValidator
  , FieldSpec
  , FieldValueKind(..)
  , appServiceRequestValidators
  , serviceRequestBodyValue
  , validateAppServiceRequest
  , validateServiceHandler
  , validateFields
  , validateObjectFields
  , requiredText
  , requiredBool
  , requiredNumber
  , requiredInt
  , requiredWord64
  , requiredObject
  , requiredArray
  , requiredAny
  , optionalText
  , optionalBool
  , optionalNumber
  , optionalInt
  , optionalWord64
  , optionalObject
  , optionalArray
  , optionalAny
  ) where

import Seer.Service.Types

validateServiceHandler :: TypedServiceOperation request response -> RawServiceHandler -> ServiceHandler request response
validateServiceHandler operation handler =
  rawServiceHandler operation $ \ctx request -> do
    let params = serviceRequestBodyValue request
        normalizedRequest = ServiceRequest (Just params)
        method = serviceOperationMethod (typedServiceOperationSpec operation)
    case validateAppServiceRequest method params of
      Left err -> pure (Left err)
      Right () -> handler ctx normalizedRequest
