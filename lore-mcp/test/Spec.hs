module Main
  ( main,
  )
where

import qualified CreateTemporalModuleSpec
import qualified DiscoverDirectorySpec
import qualified DiscoverProjectSpec
import qualified FindDeadCodeSpec
import qualified FindReferencesSpec
import qualified GetDefinitionSpec
import qualified KnowledgeCacheRpcSpec
import qualified LookupInstancesSpec
import qualified LoreDocMarkdownSpec
import qualified McpConfigSpec
import qualified McpMonadSpec
import qualified ProtocolRequestSpec
import qualified ProtocolServerSpec
import qualified ReloadHomeModulesSpec
import qualified ResolveInstanceSpec
import qualified RunTestSuiteOutcomeSpec
import qualified RunTestSuiteSpec
import qualified SearchSymbolsSpec
import qualified StructuredToolRpcSpec
import Test.Hspec
import qualified ToolBlockedSpec

main :: IO ()
main =
  hspec do
    GetDefinitionSpec.spec
    CreateTemporalModuleSpec.spec
    DiscoverDirectorySpec.spec
    DiscoverProjectSpec.spec
    FindDeadCodeSpec.spec
    FindReferencesSpec.spec
    LoreDocMarkdownSpec.spec
    McpConfigSpec.spec
    McpMonadSpec.spec
    LookupInstancesSpec.spec
    SearchSymbolsSpec.spec
    ResolveInstanceSpec.spec
    ToolBlockedSpec.spec
    ReloadHomeModulesSpec.spec
    RunTestSuiteSpec.spec
    RunTestSuiteOutcomeSpec.spec
    ProtocolRequestSpec.spec
    ProtocolServerSpec.spec
    StructuredToolRpcSpec.spec
    KnowledgeCacheRpcSpec.spec
