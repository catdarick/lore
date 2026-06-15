module Lore.Internal.Definition.Cache.ParsedOccurrenceModuleIndex
  ( ParsedOccurrenceModuleIndexCache (..),
    emptyParsedOccurrenceModuleIndexCache,
    getCachedParsedOccurrenceModuleIndex,
    prepareParsedOccurrenceModuleIndex,
    invalidateParsedOccurrenceModuleIndexCache,
    lookupModulesForOccurrenceKeys,
  )
where

import Control.Monad.Reader (asks)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.Types
  ( ModuleCache (..),
    ParsedOccurrenceModuleIndexCache (..),
  )
import Lore.Internal.Definition.Types
  ( OccKey,
    ParsedModuleFacts (..),
    ParsedOccurrenceModuleIndex (..),
  )
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, readMVar)

emptyParsedOccurrenceModuleIndexCache :: ParsedOccurrenceModuleIndexCache
emptyParsedOccurrenceModuleIndexCache =
  ParsedOccurrenceModuleIndexCache Nothing

getCachedParsedOccurrenceModuleIndex ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  m ParsedOccurrenceModuleIndex
getCachedParsedOccurrenceModuleIndex modSummaries = do
  cacheVar <- asks parsedOccurrenceModuleIndexCacheVar
  modifyMVar cacheVar $ \cacheState ->
    case cacheState.cachedParsedOccurrenceModuleIndex of
      Just parsedOccurrenceModuleIndex ->
        pure (cacheState, parsedOccurrenceModuleIndex)
      Nothing -> do
        parsedOccurrenceModuleIndex <- prepareParsedOccurrenceModuleIndex modSummaries
        pure (ParsedOccurrenceModuleIndexCache (Just parsedOccurrenceModuleIndex), parsedOccurrenceModuleIndex)

prepareParsedOccurrenceModuleIndex ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  m ParsedOccurrenceModuleIndex
prepareParsedOccurrenceModuleIndex modSummaries = do
  parsedFactsCacheVar <- asks parsedModuleFactsCacheVar
  ModuleCache parsedFactsByModule <- readMVar parsedFactsCacheVar
  Log.debug $ "Building reference occurrence index for " <> show (Map.size modSummaries) <> " modules."
  let occurrenceIndex =
        List.foldl' (insertModuleOccurrences parsedFactsByModule) Map.empty (Map.keys modSummaries)
  Log.debug $ "Finished building reference occurrence index. Indexed " <> show (Map.size occurrenceIndex) <> " unique occurrence names."
  pure (ParsedOccurrenceModuleIndex occurrenceIndex)
  where
    insertModuleOccurrences parsedFactsByModule occurrenceIndex homeModule =
      case Map.lookup homeModule parsedFactsByModule of
        Nothing -> occurrenceIndex
        Just parsedFacts ->
          List.foldl'
            (\index occKey -> Map.insertWith (<>) occKey (Set.singleton homeModule) index)
            occurrenceIndex
            (Set.toList parsedFacts.parsedOccKeys)

invalidateParsedOccurrenceModuleIndexCache :: (MonadLore m) => m ()
invalidateParsedOccurrenceModuleIndexCache = do
  cacheVar <- asks parsedOccurrenceModuleIndexCacheVar
  modifyMVar cacheVar $ \_ -> pure (emptyParsedOccurrenceModuleIndexCache, ())

lookupModulesForOccurrenceKeys :: Set.Set OccKey -> Map.Map OccKey (Set.Set GHC.Module) -> [GHC.Module]
lookupModulesForOccurrenceKeys targetOccKeys occurrenceIndex =
  Set.toList $
    List.foldl'
      (\modules occKey -> modules <> Map.findWithDefault Set.empty occKey occurrenceIndex)
      Set.empty
      (Set.toList targetOccKeys)
