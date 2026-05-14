{-# LANGUAGE OverloadedStrings #-}

module Spec.Headless (spec) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Test.Hspec

import Seer.Command.Dispatch (dispatchCommand)
import Seer.Headless
  ( HeadlessConfig(..)
  , defaultHeadlessConfig
  , headlessCommandContext
  , withHeadlessApp
  )
import Topo.Command.Types (SeerCommand(..), SeerResponse(..))

spec :: Spec
spec = describe "Seer.Headless" $ do
  it "starts actors and dispatches commands without an SDL window" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      rsp <- dispatchCommand (headlessCommandContext app) SeerCommand
        { scId = 1
        , scMethod = "get_state"
        , scParams = Null
        }
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object result -> do
          KM.member "seed" result `shouldBe` True
          KM.member "view_mode" result `shouldBe` True
        _ -> expectationFailure "expected JSON object result"

  it "uses the configured deterministic seed for repeatable tests" $
    withHeadlessApp defaultHeadlessConfig { hcSeed = 123 } $ \app -> do
      rsp <- dispatchCommand (headlessCommandContext app) SeerCommand
        { scId = 2
        , scMethod = "get_state"
        , scParams = Null
        }
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object result -> KM.lookup "seed" result `shouldBe` Just (Number 123)
        _ -> expectationFailure "expected JSON object result"
