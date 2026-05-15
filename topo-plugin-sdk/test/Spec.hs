module Main (main) where

import System.Environment (getArgs)
import Test.Hspec
import qualified Spec.FixtureHarness
import qualified Spec.Payload
import qualified Spec.Runner
import Topo.Plugin.SDK.Test.Fixtures (runFixtureCli)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--topo-plugin-fixture":_ -> runFixtureCli
    _ -> hspec $ do
      Spec.Runner.spec
      Spec.Payload.spec
      Spec.FixtureHarness.spec
