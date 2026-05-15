{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.World
  ( WorldService(..)
  , worldServiceGroup
  , worldServiceOperationSpecs
  ) where

import Seer.Service.Types

data WorldService = WorldService
  { worldGenerate :: !ServiceHandler
  , worldGetMeta :: !ServiceHandler
  , worldGetGenerationStatus :: !ServiceHandler
  , worldList :: !ServiceHandler
  , worldSave :: !ServiceHandler
  , worldLoad :: !ServiceHandler
  , worldSetName :: !ServiceHandler
  }

worldServiceGroup :: ServiceGroupSpec
worldServiceGroup = ServiceGroupSpec "world" worldServiceOperationSpecs

worldServiceOperationSpecs :: [ServiceOperationSpec]
worldServiceOperationSpecs =
  [ operationSpec "world.generate" "generate" "Start terrain/world generation."
  , operationSpec "world.meta" "get_world_meta" "Read world metadata."
  , operationSpec "world.generationStatus" "get_generation_status" "Read async generation status."
  , operationSpec "world.list" "list_worlds" "List saved worlds."
  , operationSpec "world.save" "save_world" "Save the current world."
  , operationSpec "world.load" "load_world" "Load a saved world."
  , operationSpec "world.setName" "set_world_name" "Rename the current world."
  ]
