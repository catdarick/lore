module Lore.Internal.Session.Cache.Types
  ( InterpreterContextCache (..),
    LastLoadHomeModulesResultCache (..),
    GeneratedMainModuleKey (..),
    GeneratedMainModule (..),
    GeneratedMainModulesRegistry (..),
    TemporalModulesRegistry (..),
  )
where

import qualified Data.Map as Map
import qualified GHC
import Lore.Internal.HomeModules.Result (LoadHomeModulesResult)

newtype InterpreterContextCache = InterpreterContextCache
  { cachedInterpreterModuleNames :: Maybe [GHC.ModuleName]
  }

newtype LastLoadHomeModulesResultCache = LastLoadHomeModulesResultCache
  { cachedLastLoadHomeModulesResult :: Maybe LoadHomeModulesResult
  }

data GeneratedMainModuleKey = GeneratedMainModuleKey
  { generatedMainPackageName :: String,
    generatedMainComponentName :: String,
    generatedMainOriginalPath :: FilePath
  }
  deriving (Eq, Ord, Show)

data GeneratedMainModule = GeneratedMainModule
  { generatedMainModuleName :: String,
    generatedMainPath :: FilePath
  }
  deriving (Eq, Show)

newtype GeneratedMainModulesRegistry = GeneratedMainModulesRegistry
  { generatedMainModulesByKey :: Map.Map GeneratedMainModuleKey GeneratedMainModule
  }

data TemporalModulesRegistry = TemporalModulesRegistry
  { temporalModulesDirectory :: Maybe FilePath,
    registeredTemporalModulePaths :: [FilePath]
  }
