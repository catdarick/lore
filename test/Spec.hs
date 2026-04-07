module Main where

import qualified DefinitionSpec
import Test.Hspec

main :: IO ()
main =
  hspec do
    DefinitionSpec.spec
