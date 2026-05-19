module Lore.Internal.Session.CacheInvalidation
  ( invalidateCachesForHomeModuleConfigurationChange,
    invalidateCachesAfterSourceEdits,
    retainCachesForLoadedModules,
    invalidateDefinitionDerivedCaches,
  )
where

import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.CoreModuleFacts (retainCoreModuleFactsCacheForLoadedModules)
import Lore.Internal.Definition.Cache.DefinitionModuleIndex (invalidateDefinitionModuleIndexCache)
import Lore.Internal.Definition.Cache.ParsedModuleFacts (retainParsedModuleFactsCacheForLoadedModules)
import Lore.Internal.Definition.Cache.ParsedOccurrenceModuleIndex (invalidateParsedOccurrenceModuleIndexCache)
import Lore.Internal.Definition.Cache.TypedModuleFacts (retainTypedModuleFactsCacheForLoadedModules)
import Lore.Internal.Interpreter (invalidateInterpreterContextCache)
import Lore.Internal.Lookup.ModSummaries (invalidateModSummariesCache)
import Lore.Internal.Lookup.NameToInstances (invalidateNameToInstancesIndexCache)
import Lore.Internal.Lookup.SymbolsMap (invalidateHomeSymbolsIndexCache)
import Lore.Monad (MonadLore)

invalidateCachesForHomeModuleConfigurationChange :: (MonadLore m) => m ()
invalidateCachesForHomeModuleConfigurationChange = do
  invalidateInterpreterContextCache
  invalidateModSummariesCache
  invalidateHomeSymbolsIndexCache
  invalidateNameToInstancesIndexCache
  invalidateDefinitionDerivedCaches

invalidateCachesAfterSourceEdits :: (MonadLore m) => m ()
invalidateCachesAfterSourceEdits = do
  invalidateHomeSymbolsIndexCache
  invalidateModSummariesCache
  invalidateNameToInstancesIndexCache
  invalidateDefinitionDerivedCaches

invalidateDefinitionDerivedCaches :: (MonadLore m) => m ()
invalidateDefinitionDerivedCaches = do
  invalidateParsedOccurrenceModuleIndexCache
  invalidateDefinitionModuleIndexCache

retainCachesForLoadedModules :: (MonadLore m) => Set.Set GHC.Module -> m ()
retainCachesForLoadedModules loadedModules = do
  invalidateDefinitionDerivedCaches
  retainParsedModuleFactsCacheForLoadedModules loadedModules
  retainTypedModuleFactsCacheForLoadedModules loadedModules
  retainCoreModuleFactsCacheForLoadedModules loadedModules
