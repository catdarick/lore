module Main
  ( main,
  )
where

import qualified CreateTemporalModuleSpec
import qualified FindReferencesSpec
import qualified GetDefinitionSpec
import qualified LookupInstancesSpec
import qualified ProtocolRequestSpec
import Test.Hspec

main :: IO ()
main =
  hspec do
    GetDefinitionSpec.spec
    CreateTemporalModuleSpec.spec
    FindReferencesSpec.spec
    LookupInstancesSpec.spec
    ProtocolRequestSpec.spec
