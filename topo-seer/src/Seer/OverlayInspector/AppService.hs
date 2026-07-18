{-# LANGUAGE OverloadedStrings #-}

-- | AppService-backed interpreter for overlay inspector requests.
module Seer.OverlayInspector.AppService
  ( OverlayInspectorRunService
  , performOverlayInspectorRequest
  , overlayInspectorServiceErrorValue
  ) where

import Data.Aeson (Value(..), object, (.=))
import Data.Text (Text)
import Seer.OverlayInspector.Model
import Seer.Service.Context (NestedServiceRunner)
import Seer.Service.Types (ServiceError, serviceErrorValue)

-- | Injected transport-neutral AppService operation runner.
type OverlayInspectorRunService = NestedServiceRunner

performOverlayInspectorRequest
  :: OverlayInspectorRunService
  -> OverlayInspectorRequest
  -> IO OverlayInspectorWorkerOutcome
performOverlayInspectorRequest runService request = do
  result <- runService method params
  pure $ case result of
    Right payload -> OverlayInspectorWorkerSucceeded payload
    Left errorValue -> OverlayInspectorWorkerFailed errorValue
  where
    (method, params) = case request of
      OverlayInspectorManagerRequest -> ("get_overlays", Null)
      OverlayInspectorSchemaRequest name ->
        ("get_overlay_schema", object ["overlay" .= name])
      OverlayInspectorProvenanceRequest name ->
        ("get_overlay_provenance", object ["overlay" .= name])
      OverlayInspectorExportRequest name ->
        ("export_overlay_data", object ["overlay" .= name])
      -- Validation owns a complete draft, matching the HTTP/AppService body.
      OverlayInspectorImportValidationRequest draft ->
        ("validate_overlay_import", draft)

overlayInspectorServiceErrorValue :: ServiceError -> Value
overlayInspectorServiceErrorValue = serviceErrorValue
