module Main
  ( main,
  )
where

import qualified DefinitionSpec
import qualified ImportNormalizeSpec
import qualified InterpreterSpec
import qualified TargetsSpec
import Test.Hspec

main :: IO ()
main =
  hspec do
    DefinitionSpec.spec
    ImportNormalizeSpec.spec
    InterpreterSpec.spec
    TargetsSpec.spec
