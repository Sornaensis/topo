module Main (main) where

import System.IO (hSetEncoding, stdout, stderr, utf8)
import Test.Hspec
import qualified Spec.Types
import qualified Spec.Resources
import qualified Spec.Tools
import qualified Spec.Server

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hspec $ do
    Spec.Types.spec
    Spec.Resources.spec
    Spec.Tools.spec
    Spec.Server.spec
