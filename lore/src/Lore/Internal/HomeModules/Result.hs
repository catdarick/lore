module Lore.Internal.HomeModules.Result
  ( LoadHomeModulesResult (..),
  )
where

import Lore.Diagnostics (Diagnostic)

data LoadHomeModulesResult = LoadHomeModulesResult
  { loadHomeModulesDiagnostics :: [Diagnostic],
    loadHomeModulesSucceeded :: Bool,
    loadHomeModulesLoaded :: Int,
    loadHomeModulesFailed :: Int,
    loadHomeModulesAutofixed :: Int,
    loadHomeModulesAutofixedFiles :: [FilePath],
    loadHomeModulesAutofixSummaryByFile :: [(FilePath, [String])],
    loadHomeModulesTotal :: Int
  }
  deriving (Eq, Show)
