module Lore.Internal.Session.CacheInvalidation
  ( CacheInvalidationCause (..),
    invalidateSessionCaches,
    invalidateCachesForHomeModuleConfigurationChange,
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
import Lore.Internal.Lookup.InstanceEnvironment (invalidateInstanceEnvironmentInputsCache)
import Lore.Internal.Lookup.ModSummaries (invalidateModSummariesCache)
import Lore.Internal.Lookup.NameToInstances (invalidateNameToInstancesIndexCache)
import Lore.Internal.Lookup.SymbolsMap (invalidateHomeSymbolsIndexCache)
import Lore.Monad (MonadLore)

data CacheInvalidationCause
  = HomeModuleConfigurationChanged
  | SourceEdited
  | LoadedModulesRetained (Set.Set GHC.Module)

invalidateSessionCaches :: (MonadLore m) => CacheInvalidationCause -> m ()
invalidateSessionCaches = \case
  HomeModuleConfigurationChanged -> do
    invalidateInterpreterContextCache
    invalidateModSummariesCache
    invalidateHomeSymbolsIndexCache
    invalidateNameToInstancesIndexCache
    invalidateInstanceEnvironmentInputsCache
    invalidateDefinitionDerivedCaches
  SourceEdited -> do
    invalidateHomeSymbolsIndexCache
    invalidateModSummariesCache
    invalidateNameToInstancesIndexCache
    invalidateInstanceEnvironmentInputsCache
    invalidateDefinitionDerivedCaches
  LoadedModulesRetained loadedModules -> do
    invalidateDefinitionDerivedCaches
    retainParsedModuleFactsCacheForLoadedModules loadedModules
    retainTypedModuleFactsCacheForLoadedModules loadedModules
    retainCoreModuleFactsCacheForLoadedModules loadedModules

invalidateCachesForHomeModuleConfigurationChange :: (MonadLore m) => m ()
invalidateCachesForHomeModuleConfigurationChange =
  invalidateSessionCaches HomeModuleConfigurationChanged

invalidateCachesAfterSourceEdits :: (MonadLore m) => m ()
invalidateCachesAfterSourceEdits =
  invalidateSessionCaches SourceEdited

invalidateDefinitionDerivedCaches :: (MonadLore m) => m ()
invalidateDefinitionDerivedCaches = do
  invalidateParsedOccurrenceModuleIndexCache
  invalidateDefinitionModuleIndexCache

retainCachesForLoadedModules :: (MonadLore m) => Set.Set GHC.Module -> m ()
retainCachesForLoadedModules =
  invalidateSessionCaches . LoadedModulesRetained
