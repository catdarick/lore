module Lore.Internal.Targets.Result
  ( LoadTargetsResult (..),
  )
where

import Lore.Diagnostics (Diagnostic)

data LoadTargetsResult = LoadTargetsResult
  { loadTargetsDiagnostics :: [Diagnostic],
    loadTargetsSucceeded :: Bool,
    loadTargetsModulesLoaded :: Int,
    loadTargetsModulesFailed :: Int,
    loadTargetsModulesAutofixed :: Int,
    loadTargetsAutofixedFiles :: [FilePath],
    loadTargetsAutofixSummaryByFile :: [(FilePath, [String])],
    loadTargetsModulesTotal :: Int
  }
  deriving (Eq, Show)
