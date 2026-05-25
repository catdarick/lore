module Lore.Internal.Definition.Cache.DefinitionModuleIndex
  ( DefinitionModuleIndexCache (..),
    CachedDefinitionModuleIndex (..),
    emptyDefinitionModuleIndexCache,
    getCachedDefinitionModuleIndex,
    getCachedDefinitionModuleIndexes,
    lookupDefinitionModuleIndexCache,
    storeDefinitionModuleIndexCache,
    storeDefinitionModuleIndexCacheInContext,
    invalidateDefinitionModuleIndexCacheForModuleInContext,
    invalidateDefinitionModuleIndexCache,
  )
where

import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified GHC.Plugins as GHC
import Lore.Internal.Cache.Types (CacheLookup (..))
import Lore.Internal.Definition.Analysis (buildDefinitionModuleIndex)
import Lore.Internal.Definition.Cache.ModuleArtifacts (DefinitionModuleArtifacts (..), lookupDefinitionModuleArtifacts)
import Lore.Internal.Definition.Cache.Types
  ( CachedDefinitionModuleIndex (..),
    DefinitionModuleIndexCache (..),
  )
import Lore.Internal.Definition.Types
  ( DefinitionModuleIndex,
  )
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, modifyMVar_)

emptyDefinitionModuleIndexCache :: DefinitionModuleIndexCache
emptyDefinitionModuleIndexCache =
  DefinitionModuleIndexCache Map.empty

getCachedDefinitionModuleIndex ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  GHC.Module ->
  m (Maybe DefinitionModuleIndex)
getCachedDefinitionModuleIndex modSummaries homeModule = do
  cachedModuleIndex <- lookupDefinitionModuleIndexCache homeModule
  case cachedModuleIndex of
    CacheHit moduleIndex ->
      pure moduleIndex
    CacheMiss -> do
      maybePreparedModuleIndex <- buildDefinitionModuleIndexFromCachedArtifacts modSummaries homeModule
      storeDefinitionModuleIndexCache homeModule maybePreparedModuleIndex
      pure maybePreparedModuleIndex

getCachedDefinitionModuleIndexes ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  [GHC.Module] ->
  m [DefinitionModuleIndex]
getCachedDefinitionModuleIndexes modSummaries homeModules =
  catMaybes <$> traverse (getCachedDefinitionModuleIndex modSummaries) homeModules

lookupDefinitionModuleIndexCache ::
  (MonadLore m) =>
  GHC.Module ->
  m (CacheLookup (Maybe DefinitionModuleIndex))
lookupDefinitionModuleIndexCache homeModule = do
  cacheVar <- asks definitionModuleIndexCacheVar
  modifyMVar cacheVar $ \cacheState@(DefinitionModuleIndexCache moduleIndexes) ->
    pure
      ( cacheState,
        case Map.lookup homeModule moduleIndexes of
          Nothing -> CacheMiss
          Just (CachedDefinitionModuleIndexAvailable moduleIndex) -> CacheHit (Just moduleIndex)
          Just CachedDefinitionModuleIndexUnavailable -> CacheHit Nothing
      )

buildDefinitionModuleIndexFromCachedArtifacts ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  GHC.Module ->
  m (Maybe DefinitionModuleIndex)
buildDefinitionModuleIndexFromCachedArtifacts modSummaries homeModule
  | Map.notMember homeModule modSummaries =
      pure Nothing
  | otherwise = do
      maybeArtifacts <- lookupDefinitionModuleArtifacts homeModule
      case maybeArtifacts of
        Just DefinitionModuleArtifacts {definitionArtifactParsedFacts, definitionArtifactTypedFacts, definitionArtifactCoreFacts} -> do
          let moduleIndex =
                buildDefinitionModuleIndex homeModule definitionArtifactParsedFacts definitionArtifactTypedFacts definitionArtifactCoreFacts
          pure (Just moduleIndex)
        Nothing -> do
          Log.debug $ "Cached definition artifacts missing for " <> GHC.moduleNameString (GHC.moduleName homeModule)
          pure Nothing

storeDefinitionModuleIndexCache ::
  (MonadLore m) =>
  GHC.Module ->
  Maybe DefinitionModuleIndex ->
  m ()
storeDefinitionModuleIndexCache homeModule maybeModuleIndex = do
  sessionContext <- asks id
  liftIO (storeDefinitionModuleIndexCacheInContext sessionContext homeModule maybeModuleIndex)

storeDefinitionModuleIndexCacheInContext ::
  SessionContext ->
  GHC.Module ->
  Maybe DefinitionModuleIndex ->
  IO ()
storeDefinitionModuleIndexCacheInContext sessionContext homeModule maybeModuleIndex =
  modifyMVar_ (definitionModuleIndexCacheVar sessionContext) \(DefinitionModuleIndexCache moduleIndexes) ->
    evaluate
      (DefinitionModuleIndexCache (Map.insert homeModule cachedIndex moduleIndexes))
  where
    cachedIndex =
      case maybeModuleIndex of
        Just moduleIndex -> CachedDefinitionModuleIndexAvailable moduleIndex
        Nothing -> CachedDefinitionModuleIndexUnavailable

invalidateDefinitionModuleIndexCacheForModuleInContext :: SessionContext -> GHC.Module -> IO ()
invalidateDefinitionModuleIndexCacheForModuleInContext sessionContext homeModule =
  modifyMVar_ (definitionModuleIndexCacheVar sessionContext) \(DefinitionModuleIndexCache moduleIndexes) ->
    evaluate (DefinitionModuleIndexCache (Map.delete homeModule moduleIndexes))

invalidateDefinitionModuleIndexCache :: (MonadLore m) => m ()
invalidateDefinitionModuleIndexCache = do
  cacheVar <- asks definitionModuleIndexCacheVar
  modifyMVar cacheVar $ \_ -> pure (emptyDefinitionModuleIndexCache, ())
