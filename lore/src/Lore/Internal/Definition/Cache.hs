module Lore.Internal.Definition.Cache
  ( getParsedOccurrenceModuleIndex,
    getParsedModuleFacts,
    cacheDefinitionModuleIndex,
    filterReferenceCaches,
    lookupDefinitionModuleIndexCache,
    invalidateReferenceCaches,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Definition.Types (DefinitionModuleIndex, ParsedModuleCache (..), ParsedModuleFacts, ParsedOccurrenceModuleIndex)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, readMVar)

getParsedOccurrenceModuleIndex ::
  (MonadLore m) =>
  m ParsedOccurrenceModuleIndex ->
  m ParsedOccurrenceModuleIndex
getParsedOccurrenceModuleIndex prepareIndex = do
  cacheVar <- asks parsedOccurrenceModuleIndexCache
  modifyMVar cacheVar \cache ->
    case cache of
      Just parsedOccurrenceModuleIndex ->
        pure (cache, parsedOccurrenceModuleIndex)
      Nothing -> do
        parsedOccurrenceModuleIndex <- prepareIndex
        pure (Just parsedOccurrenceModuleIndex, parsedOccurrenceModuleIndex)

lookupDefinitionModuleIndexCache ::
  (MonadLore m) =>
  GHC.Module ->
  m (Maybe (Maybe DefinitionModuleIndex))
lookupDefinitionModuleIndexCache homeModule = do
  cacheVar <- asks definitionModuleIndexCache
  modifyMVar cacheVar \cache ->
    pure (cache, Map.lookup homeModule cache)

cacheDefinitionModuleIndex ::
  (MonadLore m) =>
  GHC.Module ->
  Maybe DefinitionModuleIndex ->
  m ()
cacheDefinitionModuleIndex homeModule moduleAnalysis = do
  cacheVar <- asks definitionModuleIndexCache
  modifyMVar cacheVar \cache ->
    pure (Map.insert homeModule moduleAnalysis cache, ())

getParsedModuleFacts :: (MonadLore m) => GHC.Module -> m (Maybe ParsedModuleFacts)
getParsedModuleFacts homeModule = do
  parsedModuleCacheVar <- asks referenceParsedModuleCache
  parsedModuleCache <- liftIO (readMVar parsedModuleCacheVar)
  case Map.lookup homeModule parsedModuleCache of
    Just (ParsedModuleFactsCache parsedFacts) ->
      pure (Just parsedFacts)
    Nothing ->
      pure Nothing

filterReferenceCaches ::
  (MonadLore m) =>
  Set.Set GHC.Module ->
  m ()
filterReferenceCaches loadedModules = do
  occurrenceCacheVar <- asks parsedOccurrenceModuleIndexCache
  moduleIndexCacheVar <- asks definitionModuleIndexCache
  typedModuleCacheVar <- asks referenceTypedModuleCache
  minimalCoreFactsCacheVar <- asks referenceMinimalCoreModuleFactsCache
  parsedModuleCacheVar <- asks referenceParsedModuleCache
  modifyMVar occurrenceCacheVar \_ ->
    pure (Nothing, ())
  modifyMVar moduleIndexCacheVar \_ ->
    pure (Map.empty, ())
  modifyMVar typedModuleCacheVar \cache ->
    pure (Map.restrictKeys cache loadedModules, ())
  modifyMVar minimalCoreFactsCacheVar \cache ->
    pure (Map.restrictKeys cache loadedModules, ())
  modifyMVar parsedModuleCacheVar \cache ->
    pure (Map.restrictKeys cache loadedModules, ())

invalidateReferenceCaches :: (MonadLore m) => m ()
invalidateReferenceCaches = do
  occurrenceCacheVar <- asks parsedOccurrenceModuleIndexCache
  moduleIndexCacheVar <- asks definitionModuleIndexCache
  typedModuleCacheVar <- asks referenceTypedModuleCache
  minimalCoreFactsCacheVar <- asks referenceMinimalCoreModuleFactsCache
  parsedModuleCacheVar <- asks referenceParsedModuleCache
  modifyMVar occurrenceCacheVar \_ ->
    pure (Nothing, ())
  modifyMVar moduleIndexCacheVar \_ ->
    pure (Map.empty, ())
  modifyMVar typedModuleCacheVar \_ ->
    pure (Map.empty, ())
  modifyMVar minimalCoreFactsCacheVar \_ ->
    pure (Map.empty, ())
  modifyMVar parsedModuleCacheVar \_ ->
    pure (Map.empty, ())
