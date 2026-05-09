module Lore.Internal.Definition.ModuleIndex
  ( lookupModulesForOccurrenceKeys,
    buildParsedOccurrenceModuleIndex,
    prepareCandidateModuleIndexes,
    getDefinitionModuleIndex,
    buildCachedDefinitionModuleIndex,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Analysis (buildDefinitionModuleIndex)
import Lore.Internal.Definition.Cache (CacheLookup (..), cacheDefinitionModuleIndex, getParsedModuleFacts, lookupDefinitionModuleIndexCache)
import Lore.Internal.Definition.Types (DefinitionModuleIndex, MinimalCoreModuleFacts, MinimalTypedModuleFacts, OccKey (..), ParsedModuleCache (..), ParsedModuleFacts (..), ParsedOccurrenceModuleIndex (..), TypedModuleCache (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (readMVar)

lookupModulesForOccurrenceKeys :: Set.Set OccKey -> Map.Map OccKey (Set.Set GHC.Module) -> [GHC.Module]
lookupModulesForOccurrenceKeys targetOccKeys occurrenceIndex =
  Set.toList $
    foldl'
      (\modules occKey -> modules <> Map.findWithDefault Set.empty occKey occurrenceIndex)
      Set.empty
      (Set.toList targetOccKeys)

buildParsedOccurrenceModuleIndex ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  m ParsedOccurrenceModuleIndex
buildParsedOccurrenceModuleIndex modSummaries = do
  parsedModuleCacheVar <- asks referenceParsedModuleCache
  parsedModuleCache <- liftIO (readMVar parsedModuleCacheVar)
  Log.debug $ "Building reference occurrence index for " <> show (Map.size modSummaries) <> " modules."
  let occurrenceIndex =
        foldl' (buildModuleIndex parsedModuleCache) Map.empty (Map.keys modSummaries)
  Log.debug $ "Finished building reference occurrence index. Indexed " <> show (Map.size occurrenceIndex) <> " unique occurrence names."
  pure (ParsedOccurrenceModuleIndex occurrenceIndex)
  where
    buildModuleIndex parsedModuleCache occurrenceIndex homeModule =
      case Map.lookup homeModule parsedModuleCache of
        Nothing -> occurrenceIndex
        Just parsedModule ->
          foldl'
            (\index occName -> Map.insertWith (<>) occName (Set.singleton homeModule) index)
            occurrenceIndex
            (Set.toList (moduleOccurrenceNames parsedModule))

prepareCandidateModuleIndexes ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  [GHC.Module] ->
  m [DefinitionModuleIndex]
prepareCandidateModuleIndexes modSummaries homeModules =
  catMaybes <$> traverse (getDefinitionModuleIndex modSummaries) homeModules

getDefinitionModuleIndex ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  GHC.Module ->
  m (Maybe DefinitionModuleIndex)
getDefinitionModuleIndex modSummaries homeModule = do
  cachedModuleIndex <- lookupDefinitionModuleIndexCache homeModule
  case cachedModuleIndex of
    CacheHit moduleIndex ->
      pure moduleIndex
    CacheMiss ->
      buildCachedDefinitionModuleIndex modSummaries homeModule

buildCachedDefinitionModuleIndex ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  GHC.Module ->
  m (Maybe DefinitionModuleIndex)
buildCachedDefinitionModuleIndex modSummaries homeModule
  | Map.notMember homeModule modSummaries =
      pure Nothing
  | otherwise = do
      maybeArtifacts <- loadCachedDefinitionModuleArtifacts
      case maybeArtifacts of
        Just CachedDefinitionModuleArtifacts {cachedParsedModuleFacts, cachedMinimalTypedFacts, cachedMinimalCoreFacts} -> do
          let moduleIndex =
                buildDefinitionModuleIndex homeModule cachedParsedModuleFacts cachedMinimalTypedFacts cachedMinimalCoreFacts
          cacheDefinitionModuleIndex homeModule (Just moduleIndex)
          pure (Just moduleIndex)
        Nothing -> do
          Log.debug $ "Cached definition artifacts missing for " <> GHC.moduleNameString (GHC.moduleName homeModule)
          cacheDefinitionModuleIndex homeModule Nothing
          pure Nothing
  where
    loadCachedDefinitionModuleArtifacts = do
      typedModuleCacheVar <- asks referenceTypedModuleCache
      maybeParsedFacts <- getParsedModuleFacts homeModule
      maybeMinimalTypedFacts <- lookupMinimalTypedFacts typedModuleCacheVar
      maybeCoreFacts <- lookupMinimalCoreFacts
      pure do
        cachedParsedModuleFacts <- maybeParsedFacts
        cachedMinimalTypedFacts <- maybeMinimalTypedFacts
        pure CachedDefinitionModuleArtifacts {cachedParsedModuleFacts, cachedMinimalTypedFacts, cachedMinimalCoreFacts = maybeCoreFacts}

    lookupMinimalCoreFacts = do
      coreFactsCacheVar <- asks referenceMinimalCoreModuleFactsCache
      coreFactsByModule <- liftIO (readMVar coreFactsCacheVar)
      pure (Map.lookup homeModule coreFactsByModule)

    lookupMinimalTypedFacts typedModuleCacheVar = do
      typedModuleCache <- liftIO (readMVar typedModuleCacheVar)
      case Map.lookup homeModule typedModuleCache of
        Just (TypedModuleMinimalFacts minimalFacts) -> pure (Just minimalFacts)
        Nothing -> pure Nothing

data CachedDefinitionModuleArtifacts = CachedDefinitionModuleArtifacts
  { cachedParsedModuleFacts :: ParsedModuleFacts,
    cachedMinimalTypedFacts :: MinimalTypedModuleFacts,
    cachedMinimalCoreFacts :: Maybe MinimalCoreModuleFacts
  }

moduleOccurrenceNames :: ParsedModuleCache -> Set.Set OccKey
moduleOccurrenceNames = \case
  ParsedModuleFactsCache parsedFacts ->
    parsedFacts.parsedOccKeys
