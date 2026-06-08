{-# LANGUAGE DeriveAnyClass #-}

module Lore.Internal.Definition.Cache.Types
  ( ModuleCache (..),
    ParsedModuleFactsCache,
    TypedModuleFactsCache,
    CoreModuleFactsCache,
    ParsedOccurrenceModuleIndexCache (..),
    DefinitionIndexStatus (..),
    DefinitionModuleIndexCache (..),
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Types (DefinitionModuleIndex, MinimalCoreModuleFacts, MinimalTypedModuleFacts, ParsedModuleFacts, ParsedOccurrenceModuleIndex)

newtype ModuleCache a = ModuleCache
  { moduleCacheEntries :: Map.Map GHC.Module a
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

type ParsedModuleFactsCache = ModuleCache ParsedModuleFacts

type TypedModuleFactsCache = ModuleCache MinimalTypedModuleFacts

type CoreModuleFactsCache = ModuleCache MinimalCoreModuleFacts

newtype ParsedOccurrenceModuleIndexCache = ParsedOccurrenceModuleIndexCache
  { cachedParsedOccurrenceModuleIndex :: Maybe ParsedOccurrenceModuleIndex
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data DefinitionIndexStatus
  = DefinitionIndexUnavailable
  | DefinitionIndexAvailable DefinitionModuleIndex
  deriving stock (Generic)

newtype DefinitionModuleIndexCache = DefinitionModuleIndexCache
  { cachedDefinitionModuleIndexes :: Map.Map GHC.Module DefinitionIndexStatus
  }
  deriving stock (Generic)
