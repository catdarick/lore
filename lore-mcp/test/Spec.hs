module Main
  ( main,
  )
where

import qualified CreateTemporalModuleSpec
import qualified DiscoverDirectorySpec
import qualified DiscoverProjectSpec
import qualified FindReferencesSpec
import qualified GetDefinitionSpec
import qualified LookupInstancesSpec
import qualified LoreDocMarkdownSpec
import qualified ProtocolRequestSpec
import qualified ReloadHomeModulesSpec
import qualified RunTestSuiteSpec
import qualified SearchSymbolsSpec
import Test.Hspec
import qualified ToolBlockedSpec

main :: IO ()
main =
  hspec do
    GetDefinitionSpec.spec
    CreateTemporalModuleSpec.spec
    DiscoverDirectorySpec.spec
    DiscoverProjectSpec.spec
    FindReferencesSpec.spec
    LoreDocMarkdownSpec.spec
    LookupInstancesSpec.spec
    SearchSymbolsSpec.spec
    ToolBlockedSpec.spec
    ReloadHomeModulesSpec.spec
    RunTestSuiteSpec.spec
    ProtocolRequestSpec.spec
