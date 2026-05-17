module Lore.Internal.TestSuite
  ( RunTestSuiteOptions (..),
    TestSuiteComponentStatus (..),
    TestSuiteComponentResult (..),
    runTestSuite,
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import Data.List (sortOn)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified GHC
import Lore.Diagnostics (Diagnostic)
import Lore.Internal.Interpreter (executeStatementRaw)
import Lore.Internal.Lookup.ModSummaries (getCachedModSummariesByFile)
import Lore.Internal.Package (ComponentData (..), ComponentKind (..), PackageData (..), componentMainModulePathCandidates, firstExistingPath, prepareComponentsData)
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.Cache.Types (GeneratedMainTarget (..), GeneratedMainTargetKey (..), GeneratedMainTargetsRegistry (..))
import Lore.Internal.SourcePath (normalizeSourceFilePathM)
import Lore.Monad (MonadLore)
import System.FilePath (normalise)

data RunTestSuiteOptions = RunTestSuiteOptions
  { packageName :: Maybe String,
    testArguments :: [String]
  }

data TestSuiteComponentStatus
  = TestSuiteComponentSetupFailure String
  | TestSuiteComponentExecutionFailure [Diagnostic]
  | TestSuiteComponentExecutionSuccess String

data TestSuiteComponentResult = TestSuiteComponentResult
  { packageName :: String,
    componentName :: String,
    moduleName :: Maybe String,
    status :: TestSuiteComponentStatus
  }

runTestSuite :: (MonadLore m) => RunTestSuiteOptions -> m [TestSuiteComponentResult]
runTestSuite RunTestSuiteOptions {packageName = packageFilter, testArguments} = do
  packages <- prepareComponentsData
  generatedMainTargetsByKey <- lookupGeneratedMainTargetsByKey
  modSummariesByFile <- getCachedModSummariesByFile
  let testComponents =
        [ (pkg.packageName, pkg.packageRoot, component)
        | pkg <- sortOn (.packageName) packages,
          packageMatches packageFilter pkg.packageName,
          component <- sortOn (.componentName) pkg.components,
          component.componentKind == ComponentKindTest
        ]
  forM testComponents \(pkgName, pkgRoot, component) -> do
    maybeMainPath <- firstExistingPath (componentMainModulePathCandidates pkgRoot component)
    case maybeMainPath of
      Nothing ->
        pure
          TestSuiteComponentResult
            { packageName = pkgName,
              componentName = component.componentName,
              moduleName = Nothing,
              status = TestSuiteComponentSetupFailure "main module path does not resolve to an existing file"
            }
      Just mainPath -> do
        resolvedModuleName <- resolveEntryModuleName pkgName component.componentName mainPath generatedMainTargetsByKey modSummariesByFile
        case resolvedModuleName of
          Left reason ->
            pure
              TestSuiteComponentResult
                { packageName = pkgName,
                  componentName = component.componentName,
                  moduleName = Nothing,
                  status = TestSuiteComponentSetupFailure reason
                }
          Right entryModuleName -> do
            let statement = renderRunStatement entryModuleName testArguments
            runResult <- executeStatementRaw (T.pack statement)
            pure
              TestSuiteComponentResult
                { packageName = pkgName,
                  componentName = component.componentName,
                  moduleName = Just entryModuleName,
                  status =
                    case runResult of
                      Left runDiagnostics ->
                        TestSuiteComponentExecutionFailure runDiagnostics
                      Right renderedOutput ->
                        TestSuiteComponentExecutionSuccess renderedOutput
                }

packageMatches :: Maybe String -> String -> Bool
packageMatches maybePackageName packageName =
  case maybePackageName of
    Nothing -> True
    Just expectedPackageName -> expectedPackageName == packageName

resolveEntryModuleName ::
  (MonadLore m) =>
  String ->
  String ->
  FilePath ->
  Map.Map GeneratedMainTargetKey GeneratedMainTarget ->
  Map.Map FilePath GHC.ModSummary ->
  m (Either String String)
resolveEntryModuleName packageName componentName mainPath generatedMainTargetsByKey modSummariesByFile =
  case Map.lookup generatedTargetKey generatedMainTargetsByKey of
    Just generatedMainTarget ->
      pure (Right generatedMainTarget.generatedMainModuleName)
    Nothing -> do
      normalizedMainPath <- normalizeSourceFilePathM mainPath
      let candidatePaths =
            [ mainPath,
              normalise mainPath,
              normalizedMainPath,
              normalise normalizedMainPath
            ]
      pure $
        case firstMatchingSummary candidatePaths modSummariesByFile of
          Just modSummary ->
            Right (GHC.moduleNameString (GHC.moduleName (GHC.ms_mod modSummary)))
          Nothing ->
            Left ("entry module is not present in loaded module graph: " <> mainPath)
  where
    generatedTargetKey =
      GeneratedMainTargetKey
        { generatedMainPackageName = packageName,
          generatedMainComponentName = componentName,
          generatedMainOriginalPath = mainPath
        }

firstMatchingSummary :: [FilePath] -> Map.Map FilePath GHC.ModSummary -> Maybe GHC.ModSummary
firstMatchingSummary candidatePaths modSummariesByFile =
  go candidatePaths
  where
    go [] = Nothing
    go (candidatePath : restPaths) =
      case Map.lookup candidatePath modSummariesByFile of
        Just summary -> Just summary
        Nothing -> go restPaths

lookupGeneratedMainTargetsByKey :: (MonadLore m) => m (Map.Map GeneratedMainTargetKey GeneratedMainTarget)
lookupGeneratedMainTargetsByKey = do
  registryVar <- asks generatedMainTargetsRegistryVar
  GeneratedMainTargetsRegistry generatedMainTargetsByKey <- liftIO (MVar.readMVar registryVar)
  pure generatedMainTargetsByKey

renderRunStatement :: String -> [String] -> String
renderRunStatement entryModuleName args =
  "System.Environment.withArgs "
    <> show args
    <> " "
    <> entryModuleName
    <> ".main"
