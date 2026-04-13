module Lore.Internal.Definition.Cache
  ( lookupReferenceModuleSearchCache,
    cacheReferenceModuleSearch,
    lookupReferenceModuleAnalysisCache,
    cacheReferenceModuleAnalysis,
    invalidateReferenceCaches,
  )
where

import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import qualified GHC
import Lore.Internal.Definition.Types (ReferenceModuleAnalysis, ReferenceModuleSearch)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar)

lookupReferenceModuleSearchCache ::
  (MonadLore m) =>
  GHC.Module ->
  m (Maybe (Maybe ReferenceModuleSearch))
lookupReferenceModuleSearchCache homeModule = do
  cacheVar <- asks referenceModuleSearchCache
  modifyMVar cacheVar \cache ->
    pure (cache, Map.lookup homeModule cache)

cacheReferenceModuleSearch ::
  (MonadLore m) =>
  GHC.Module ->
  Maybe ReferenceModuleSearch ->
  m ()
cacheReferenceModuleSearch homeModule moduleSearch = do
  cacheVar <- asks referenceModuleSearchCache
  modifyMVar cacheVar \cache ->
    pure (Map.insert homeModule moduleSearch cache, ())

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
  searchCacheVar <- asks referenceModuleSearchCache
  analysisCacheVar <- asks referenceModuleAnalysisCache
  modifyMVar searchCacheVar \_ ->
    pure (Map.empty, ())
  modifyMVar analysisCacheVar \_ ->
    pure (Map.empty, ())
