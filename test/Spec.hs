module Main where

import qualified DefinitionSpec
import qualified TargetsSpec
import Test.Hspec

main :: IO ()
main =
  hspec do
    DefinitionSpec.spec
    TargetsSpec.spec
