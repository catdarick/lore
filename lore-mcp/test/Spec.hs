module Main
  ( main,
  )
where

import qualified CreateTemporalModuleSpec
import qualified DiscoverProjectSpec
import qualified FindReferencesSpec
import qualified GetDefinitionSpec
import qualified LookupInstancesSpec
import qualified ProtocolRequestSpec
import qualified SearchSymbolsSpec
import Test.Hspec

main :: IO ()
main =
  hspec do
    GetDefinitionSpec.spec
    CreateTemporalModuleSpec.spec
    DiscoverProjectSpec.spec
    FindReferencesSpec.spec
    LookupInstancesSpec.spec
    SearchSymbolsSpec.spec
    ProtocolRequestSpec.spec
