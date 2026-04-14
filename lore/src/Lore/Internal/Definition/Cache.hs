module Lore.Internal.Definition.Cache
  ( getReferenceOccurrenceIndex,
    cacheReferenceModuleAnalysis,
    filterReferenceCaches,
    lookupReferenceModuleAnalysisCache,
    invalidateReferenceCaches,
  )
where

import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Definition.Types (ReferenceModuleAnalysis, ReferenceOccurrenceIndex)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar)

getReferenceOccurrenceIndex ::
  (MonadLore m) =>
  m ReferenceOccurrenceIndex ->
  m ReferenceOccurrenceIndex
getReferenceOccurrenceIndex prepareIndex = do
  cacheVar <- asks referenceOccurrenceIndexCache
  modifyMVar cacheVar \cache ->
    case cache of
      Just referenceOccurrenceIndex ->
        pure (cache, referenceOccurrenceIndex)
      Nothing -> do
        referenceOccurrenceIndex <- prepareIndex
        pure (Just referenceOccurrenceIndex, referenceOccurrenceIndex)

lookupReferenceModuleAnalysisCache ::
  (MonadLore m) =>
  GHC.Module ->
  m (Maybe (Maybe ReferenceModuleAnalysis))
lookupReferenceModuleAnalysisCache homeModule = do
  cacheVar <- asks referenceModuleAnalysisCache
  modifyMVar cacheVar \cache ->
    pure (cache, Map.lookup homeModule cache)

cacheReferenceModuleAnalysis ::
  (MonadLore m) =>
  GHC.Module ->
  Maybe ReferenceModuleAnalysis ->
  m ()
cacheReferenceModuleAnalysis homeModule moduleAnalysis = do
  cacheVar <- asks referenceModuleAnalysisCache
  modifyMVar cacheVar \cache ->
    pure (Map.insert homeModule moduleAnalysis cache, ())

filterReferenceCaches ::
  (MonadLore m) =>
  Set.Set GHC.Module ->
  m ()
filterReferenceCaches loadedModules = do
  occurrenceCacheVar <- asks referenceOccurrenceIndexCache
  analysisCacheVar <- asks referenceModuleAnalysisCache
  typedModuleCacheVar <- asks referenceTypedModuleCache
  minimalCoreFactsCacheVar <- asks referenceMinimalCoreModuleFactsCache
  parsedModuleCacheVar <- asks referenceParsedModuleCache
  modifyMVar occurrenceCacheVar \_ ->
    pure (Nothing, ())
  modifyMVar analysisCacheVar \_ ->
    pure (Map.empty, ())
  modifyMVar typedModuleCacheVar \cache ->
    pure (Map.restrictKeys cache loadedModules, ())
  modifyMVar minimalCoreFactsCacheVar \cache ->
    pure (Map.restrictKeys cache loadedModules, ())
  modifyMVar parsedModuleCacheVar \cache ->
    pure (Map.restrictKeys cache loadedModules, ())

invalidateReferenceCaches :: (MonadLore m) => m ()
invalidateReferenceCaches = do
  occurrenceCacheVar <- asks referenceOccurrenceIndexCache
  analysisCacheVar <- asks referenceModuleAnalysisCache
  typedModuleCacheVar <- asks referenceTypedModuleCache
  minimalCoreFactsCacheVar <- asks referenceMinimalCoreModuleFactsCache
  parsedModuleCacheVar <- asks referenceParsedModuleCache
  modifyMVar occurrenceCacheVar \_ ->
    pure (Nothing, ())
  modifyMVar analysisCacheVar \_ ->
    pure (Map.empty, ())
  modifyMVar typedModuleCacheVar \_ ->
    pure (Map.empty, ())
  modifyMVar minimalCoreFactsCacheVar \_ ->
    pure (Map.empty, ())
  modifyMVar parsedModuleCacheVar \_ ->
    pure (Map.empty, ())
