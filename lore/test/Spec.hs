module Main
  ( main,
  )
where

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
import qualified PackageDiscoverySpec
import qualified PackageEnvironmentSpec
import qualified RedundantImportsSpec
import qualified SourceEditSpec
import qualified TemporalModulesSpec
import Test.Hspec
import qualified TestSupportSpec

main :: IO ()
main =
  hspec do
    DefinitionSpec.spec
    DeadCodeSpec.spec
    ImportCleanupApplySpec.spec
    ImportCleanupEditSpec.spec
    ImportCleanupImportListParserSpec.spec
    ImportCleanupResolveSpec.spec
    ImportCleanupRewriteSpec.spec
    InterpreterSpec.spec
    LoggerSpec.spec
    LookupSearchSpec.spec
    LookupSpec.spec
    PackageDiscoverySpec.spec
    PackageEnvironmentSpec.spec
    SourceEditSpec.spec
    HomeModulesSpec.spec
    TemporalModulesSpec.spec
    RedundantImportsSpec.spec
    TestSupportSpec.spec
