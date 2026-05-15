{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.DataResource
  ( DataResourceService(..)
  , dataResourceServiceGroup
  , dataResourceServiceOperationSpecs
  ) where

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

dataResourceServiceGroup :: ServiceGroupSpec
dataResourceServiceGroup = ServiceGroupSpec "data-resources" dataResourceServiceOperationSpecs

dataResourceServiceOperationSpecs :: [ServiceOperationSpec]
dataResourceServiceOperationSpecs =
  [ operationSpec "data.plugins.list" "data_list_plugins" "List plugins that expose data resources."
  , operationSpec "data.resources.list" "data_list_resources" "List data resources for a plugin."
  , operationSpec "data.records.list" "data_list_records" "List records in a data resource."
  , operationSpec "data.records.get" "data_get_record" "Read one data-resource record."
  , operationSpec "data.records.create" "data_create_record" "Create a data-resource record."
  , operationSpec "data.records.update" "data_update_record" "Update a data-resource record."
  , operationSpec "data.records.delete" "data_delete_record" "Delete a data-resource record."
  , operationSpec "data.state" "data_get_state" "Read data-browser state."
  ]
