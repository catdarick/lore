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
    loadTargetsModulesTotal :: Int
  }
  deriving (Eq, Show)
