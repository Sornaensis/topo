module Main (main) where

import Test.Hspec
import qualified Spec.Types
import qualified Spec.Resources
import qualified Spec.Tools
import qualified Spec.Server

main :: IO ()
main = hspec $ do
  Spec.Types.spec
  Spec.Resources.spec
  Spec.Tools.spec
  Spec.Server.spec
