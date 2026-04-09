module Main
  ( main,
  )
where

import qualified DefinitionSpec
import qualified ImportNormalizeSpec
import qualified InterpreterSpec
import qualified LoggerSpec
import qualified LookupSpec
import qualified TargetsSpec
import Test.Hspec

main :: IO ()
main =
  hspec do
    DefinitionSpec.spec
    ImportNormalizeSpec.spec
    InterpreterSpec.spec
    LoggerSpec.spec
    LookupSpec.spec
    TargetsSpec.spec
