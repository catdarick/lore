module Main
  ( main,
  )
where

import qualified ConfigSpec
import qualified DeadCodeSpec
import qualified DefinitionSpec
import qualified HomeModulesSpec
import qualified ImportCleanupApplySpec
import qualified ImportCleanupEditSpec
import qualified ImportCleanupImportListParserSpec
import qualified ImportCleanupResolveSpec
import qualified ImportCleanupRewriteSpec
import qualified InterpreterSpec
import qualified LoggerSpec
import qualified LookupSearchSpec
import qualified LookupSpec
import qualified ModulePatternSpec
import qualified PackageDiscoverySpec
import qualified PackageEnvironmentSpec
import qualified RedundantImportsSpec
import qualified SourceEditSpec
import qualified TemporalModulesSpec
import Test.Hspec
import qualified TestSupportSpec
import qualified ValueTypeHeadSpec

main :: IO ()
main =
  hspec do
    ConfigSpec.spec
    DefinitionSpec.spec
    DeadCodeSpec.spec
    ImportCleanupApplySpec.spec
    ImportCleanupEditSpec.spec
    ImportCleanupImportListParserSpec.spec
    ImportCleanupResolveSpec.spec
    ImportCleanupRewriteSpec.spec
    InterpreterSpec.spec
    LoggerSpec.spec
    ModulePatternSpec.spec
    LookupSearchSpec.spec
    LookupSpec.spec
    PackageDiscoverySpec.spec
    PackageEnvironmentSpec.spec
    SourceEditSpec.spec
    HomeModulesSpec.spec
    TemporalModulesSpec.spec
    ValueTypeHeadSpec.spec
    RedundantImportsSpec.spec
    TestSupportSpec.spec
