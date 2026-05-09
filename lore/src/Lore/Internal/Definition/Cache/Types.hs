{-# LANGUAGE DeriveAnyClass #-}

module Lore.Internal.Definition.Cache.Types
  ( ParsedModuleFactsCache (..),
    TypedModuleFactsCache (..),
    CoreModuleFactsCache (..),
    ParsedOccurrenceModuleIndexCache (..),
    CachedDefinitionModuleIndex (..),
    DefinitionModuleIndexCache (..),
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Types (DefinitionModuleIndex, MinimalCoreModuleFacts, MinimalTypedModuleFacts, ParsedModuleFacts, ParsedOccurrenceModuleIndex)

newtype ParsedModuleFactsCache = ParsedModuleFactsCache
  { cachedParsedModuleFactsByModule :: Map.Map GHC.Module ParsedModuleFacts
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

newtype TypedModuleFactsCache = TypedModuleFactsCache
  { cachedTypedModuleFactsByModule :: Map.Map GHC.Module MinimalTypedModuleFacts
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

newtype CoreModuleFactsCache = CoreModuleFactsCache
  { cachedCoreModuleFactsByModule :: Map.Map GHC.Module MinimalCoreModuleFacts
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

newtype ParsedOccurrenceModuleIndexCache = ParsedOccurrenceModuleIndexCache
  { cachedParsedOccurrenceModuleIndex :: Maybe ParsedOccurrenceModuleIndex
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data CachedDefinitionModuleIndex
  = CachedDefinitionModuleIndexAvailable DefinitionModuleIndex
  | CachedDefinitionModuleIndexUnavailable
  deriving stock (Eq, Generic)

newtype DefinitionModuleIndexCache = DefinitionModuleIndexCache
  { cachedDefinitionModuleIndexes :: Map.Map GHC.Module CachedDefinitionModuleIndex
  }
  deriving stock (Generic)
