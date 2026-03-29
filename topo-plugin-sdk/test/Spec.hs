module Main (main) where

import Test.Hspec
import qualified Spec.Payload
import qualified Spec.Runner

main :: IO ()
main = hspec $ do
  Spec.Runner.spec
  Spec.Payload.spec
