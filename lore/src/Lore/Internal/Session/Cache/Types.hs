module Lore.Internal.Session.Cache.Types
  ( InterpreterContextCache (..),
    LastLoadTargetsResultCache (..),
    TemporalModulesRegistry (..),
  )
where

import qualified GHC
import Lore.Internal.Targets.Result (LoadTargetsResult)

newtype InterpreterContextCache = InterpreterContextCache
  { cachedInterpreterModuleNames :: Maybe [GHC.ModuleName]
  }

newtype LastLoadTargetsResultCache = LastLoadTargetsResultCache
  { cachedLastLoadTargetsResult :: Maybe LoadTargetsResult
  }

data TemporalModulesRegistry = TemporalModulesRegistry
  { temporalModulesDirectory :: Maybe FilePath,
    registeredTemporalModulePaths :: [FilePath]
  }
