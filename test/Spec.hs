module Main where

import qualified DefinitionSpec
import qualified ImportNormalizeSpec
import qualified TargetsSpec
import Test.Hspec

main :: IO ()
main =
  hspec do
    DefinitionSpec.spec
    ImportNormalizeSpec.spec
    TargetsSpec.spec
