module Lore.Internal.Session.Cache.Types
  ( InterpreterContextCache (..),
    LastLoadTargetsResultCache (..),
    GeneratedMainTargetKey (..),
    GeneratedMainTarget (..),
    GeneratedMainTargetsRegistry (..),
    TemporalModulesRegistry (..),
  )
where

import qualified Data.Map as Map
import qualified GHC
import Lore.Internal.Targets.Result (LoadTargetsResult)

newtype InterpreterContextCache = InterpreterContextCache
  { cachedInterpreterModuleNames :: Maybe [GHC.ModuleName]
  }

newtype LastLoadTargetsResultCache = LastLoadTargetsResultCache
  { cachedLastLoadTargetsResult :: Maybe LoadTargetsResult
  }

data GeneratedMainTargetKey = GeneratedMainTargetKey
  { generatedMainPackageName :: String,
    generatedMainComponentName :: String,
    generatedMainOriginalPath :: FilePath
  }
  deriving (Eq, Ord, Show)

data GeneratedMainTarget = GeneratedMainTarget
  { generatedMainModuleName :: String,
    generatedMainPath :: FilePath
  }
  deriving (Eq, Show)

newtype GeneratedMainTargetsRegistry = GeneratedMainTargetsRegistry
  { generatedMainTargetsByKey :: Map.Map GeneratedMainTargetKey GeneratedMainTarget
  }

data TemporalModulesRegistry = TemporalModulesRegistry
  { temporalModulesDirectory :: Maybe FilePath,
    registeredTemporalModulePaths :: [FilePath]
  }
