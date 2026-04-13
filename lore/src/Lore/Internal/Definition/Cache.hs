module Lore.Internal.Definition.Cache
  ( getReferenceOccurrenceIndex,
    lookupReferenceModuleAnalysisCache,
    cacheReferenceModuleAnalysis,
    invalidateReferenceCaches,
  )
where

import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
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

invalidateReferenceCaches :: (MonadLore m) => m ()
invalidateReferenceCaches = do
  occurrenceIndexCacheVar <- asks referenceOccurrenceIndexCache
  analysisCacheVar <- asks referenceModuleAnalysisCache
  modifyMVar occurrenceIndexCacheVar \_ ->
    pure (Nothing, ())
  modifyMVar analysisCacheVar \_ ->
    pure (Map.empty, ())
