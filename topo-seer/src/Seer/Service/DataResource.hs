{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.DataResource
  ( DataResourceService(..)
  , DataResourcePluginSummary(..)
  , DataResourceListPluginsRequest(..)
  , DataResourceListPluginsResponse(..)
  , DataResourceListResourcesRequest(..)
  , DataResourceListResourcesResponse(..)
  , DataResourceListRecordsRequest(..)
  , DataResourceListRecordsResponse(..)
  , DataResourceGetRecordRequest(..)
  , DataResourceGetRecordResponse(..)
  , DataResourceCreateRecordRequest(..)
  , DataResourceCreateRecordResponse(..)
  , DataResourceUpdateRecordRequest(..)
  , DataResourceUpdateRecordResponse(..)
  , DataResourceDeleteRecordRequest(..)
  , DataResourceDeleteRecordResponse(..)
  , DataResourceStateRequest(..)
  , DataResourceStateResponse(..)
  , dataResourceListPluginsOperation
  , dataResourceListResourcesOperation
  , dataResourceListRecordsOperation
  , dataResourceGetRecordOperation
  , dataResourceCreateRecordOperation
  , dataResourceUpdateRecordOperation
  , dataResourceDeleteRecordOperation
  , dataResourceStateOperation
  , dataResourceServiceGroup
  , dataResourceServiceOperationSpecs
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Topo.Plugin.DataResource (DataResourceSchema)
import Topo.Plugin.RPC.DataService (DataRecord, MutateResult, QueryResource, QueryResult)

import Seer.Service.Types

data DataResourceService = DataResourceService
  { dataListPlugins :: !ServiceHandler
  , dataListResources :: !ServiceHandler
  , dataListRecords :: !ServiceHandler
  , dataGetRecord :: !ServiceHandler
  , dataCreateRecord :: !ServiceHandler
  , dataUpdateRecord :: !ServiceHandler
  , dataDeleteRecord :: !ServiceHandler
  , dataGetState :: !ServiceHandler
  }

data DataResourcePluginSummary = DataResourcePluginSummary
  { dataResourcePluginName :: !Text
  , dataResourcePluginResources :: ![Text]
  } deriving (Eq, Show)

data DataResourceListPluginsRequest = DataResourceListPluginsRequest
  deriving (Eq, Show)

newtype DataResourceListPluginsResponse = DataResourceListPluginsResponse
  { dataResourcePlugins :: [DataResourcePluginSummary]
  } deriving (Eq, Show)

newtype DataResourceListResourcesRequest = DataResourceListResourcesRequest
  { dataResourceResourcesPlugin :: Text
  } deriving (Eq, Show)

data DataResourceListResourcesResponse = DataResourceListResourcesResponse
  { dataResourceResourcesPlugin :: !Text
  , dataResourceSchemas :: ![DataResourceSchema]
  } deriving (Eq, Show)

data DataResourceListRecordsRequest = DataResourceListRecordsRequest
  { dataResourceRecordsPlugin :: !Text
  , dataResourceRecordsQuery :: !QueryResource
  } deriving (Eq, Show)

data DataResourceListRecordsResponse = DataResourceListRecordsResponse
  { dataResourceRecordsPlugin :: !Text
  , dataResourceRecordsResult :: !QueryResult
  } deriving (Eq, Show)

data DataResourceGetRecordRequest = DataResourceGetRecordRequest
  { dataResourceGetPlugin :: !Text
  , dataResourceGetResource :: !Text
  , dataResourceGetKey :: !Value
  } deriving (Eq, Show)

data DataResourceGetRecordResponse = DataResourceGetRecordResponse
  { dataResourceGetPlugin :: !Text
  , dataResourceGetResource :: !Text
  , dataResourceRecord :: !DataRecord
  } deriving (Eq, Show)

data DataResourceCreateRecordRequest = DataResourceCreateRecordRequest
  { dataResourceCreatePlugin :: !Text
  , dataResourceCreateResource :: !Text
  , dataResourceCreateRecord :: !DataRecord
  } deriving (Eq, Show)

data DataResourceCreateRecordResponse = DataResourceCreateRecordResponse
  { dataResourceCreatedPlugin :: !Text
  , dataResourceCreatedResource :: !Text
  , dataResourceCreateResult :: !MutateResult
  } deriving (Eq, Show)

data DataResourceUpdateRecordRequest = DataResourceUpdateRecordRequest
  { dataResourceUpdatePlugin :: !Text
  , dataResourceUpdateResource :: !Text
  , dataResourceUpdateKey :: !Value
  , dataResourceUpdateRecord :: !DataRecord
  } deriving (Eq, Show)

data DataResourceUpdateRecordResponse = DataResourceUpdateRecordResponse
  { dataResourceUpdatedPlugin :: !Text
  , dataResourceUpdatedResource :: !Text
  , dataResourceUpdateResult :: !MutateResult
  } deriving (Eq, Show)

data DataResourceDeleteRecordRequest = DataResourceDeleteRecordRequest
  { dataResourceDeletePlugin :: !Text
  , dataResourceDeleteResource :: !Text
  , dataResourceDeleteKey :: !Value
  } deriving (Eq, Show)

data DataResourceDeleteRecordResponse = DataResourceDeleteRecordResponse
  { dataResourceDeletedPlugin :: !Text
  , dataResourceDeletedResource :: !Text
  , dataResourceDeleteResult :: !MutateResult
  } deriving (Eq, Show)

data DataResourceStateRequest = DataResourceStateRequest
  deriving (Eq, Show)

data DataResourceStateResponse = DataResourceStateResponse
  { dataResourceSelectedPlugin :: !(Maybe Text)
  , dataResourceSelectedResource :: !(Maybe Text)
  , dataResourceRecordCount :: !Int
  , dataResourceTotalCount :: !Int
  , dataResourcePageOffset :: !Int
  , dataResourceLoading :: !Bool
  , dataResourceAsyncStatus :: !AsyncStatusSnapshot
  , dataResourceSelectedRecordKey :: !(Maybe Value)
  , dataResourceEditMode :: !Bool
  , dataResourceCreateMode :: !Bool
  , dataResourceHasSelection :: !Bool
  } deriving (Eq, Show)

dataResourceServiceGroup :: ServiceGroupSpec
dataResourceServiceGroup = ServiceGroupSpec "data-resources" dataResourceServiceOperationSpecs

dataResourceServiceOperationSpecs :: [ServiceOperationSpec]
dataResourceServiceOperationSpecs =
  [ typedServiceOperationSpec dataResourceListPluginsOperation
  , typedServiceOperationSpec dataResourceListResourcesOperation
  , typedServiceOperationSpec dataResourceListRecordsOperation
  , typedServiceOperationSpec dataResourceGetRecordOperation
  , typedServiceOperationSpec dataResourceCreateRecordOperation
  , typedServiceOperationSpec dataResourceUpdateRecordOperation
  , typedServiceOperationSpec dataResourceDeleteRecordOperation
  , typedServiceOperationSpec dataResourceStateOperation
  ]

dataResourceListPluginsOperation :: TypedServiceOperation DataResourceListPluginsRequest DataResourceListPluginsResponse
dataResourceListPluginsOperation = typedOperation $
  operationSpec "data.plugins.list" "data_list_plugins" "List plugins that expose data resources."

dataResourceListResourcesOperation :: TypedServiceOperation DataResourceListResourcesRequest DataResourceListResourcesResponse
dataResourceListResourcesOperation = typedOperation $
  operationSpec "data.resources.list" "data_list_resources" "List data resources for a plugin."

dataResourceListRecordsOperation :: TypedServiceOperation DataResourceListRecordsRequest DataResourceListRecordsResponse
dataResourceListRecordsOperation = typedOperation $
  operationSpec "data.records.list" "data_list_records" "List records in a data resource."

dataResourceGetRecordOperation :: TypedServiceOperation DataResourceGetRecordRequest DataResourceGetRecordResponse
dataResourceGetRecordOperation = typedOperation $
  operationSpec "data.records.get" "data_get_record" "Read one data-resource record."

dataResourceCreateRecordOperation :: TypedServiceOperation DataResourceCreateRecordRequest DataResourceCreateRecordResponse
dataResourceCreateRecordOperation = typedOperation $
  operationSpec "data.records.create" "data_create_record" "Create a data-resource record."

dataResourceUpdateRecordOperation :: TypedServiceOperation DataResourceUpdateRecordRequest DataResourceUpdateRecordResponse
dataResourceUpdateRecordOperation = typedOperation $
  operationSpec "data.records.update" "data_update_record" "Update a data-resource record."

dataResourceDeleteRecordOperation :: TypedServiceOperation DataResourceDeleteRecordRequest DataResourceDeleteRecordResponse
dataResourceDeleteRecordOperation = typedOperation $
  operationSpec "data.records.delete" "data_delete_record" "Delete a data-resource record."

dataResourceStateOperation :: TypedServiceOperation DataResourceStateRequest DataResourceStateResponse
dataResourceStateOperation = typedOperation $
  operationSpec "data.state" "data_get_state" "Read data-browser state."
