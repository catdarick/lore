module Lore.Internal.HomeModules.Result
  ( HomeModulesLoadSummary (..),
    LoadHomeModulesResult (..),
  )
where

import Lore.Diagnostics (Diagnostic)
import Lore.Internal.ProjectEnvironment.Types (ProjectEnvironmentFailure)

data LoadHomeModulesResult
  = LoadHomeModulesCompleted HomeModulesLoadSummary
  | LoadHomeModulesPreparationFailed ProjectEnvironmentFailure
  deriving (Eq, Show)

data HomeModulesLoadSummary = HomeModulesLoadSummary
  { homeModulesCompilationSucceeded :: Bool,
    homeModulesDiagnostics :: [Diagnostic],
    homeModulesLoaded :: Int,
    homeModulesFailed :: Int,
    homeModulesAutofixed :: Int,
    homeModulesAutofixedFiles :: [FilePath],
    homeModulesAutofixSummaryByFile :: [(FilePath, [String])],
    homeModulesTotal :: Int
  }
  deriving (Eq, Show)
