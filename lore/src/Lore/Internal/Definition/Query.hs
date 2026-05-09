module Lore.Internal.Definition.Query
  ( resolveDefinitionSourceWithSummaries,
    resolveDefinitionDependencyNames,
    resolveDefinitionClosureSourcesWithSummaries,
    getMinifiedImportsForDefinitionWithSummaries,
    resolveReferenceMatchesForNamesWithSummaries,
    matchingReferenceMatches,
    forceReferenceMatchesForRendering,
  )
where

import Control.DeepSeq (deepseq)
import qualified Control.Exception as Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache (getParsedOccurrenceModuleIndex)
import qualified Lore.Internal.Definition.Index as DefinitionIndex
import Lore.Internal.Definition.ModuleIndex (buildParsedOccurrenceModuleIndex, getDefinitionModuleIndex, lookupModulesForOccurrenceKeys, prepareCandidateModuleIndexes)
import Lore.Internal.Definition.Timing (withTimedSection)
import Lore.Internal.Definition.Types (DefinitionDependencies (..), DefinitionId, DefinitionModuleIndex, DefinitionSource (..), NamedDefinitionSource (..), ParsedOccurrenceModuleIndex (..), ReferenceMatch (..), RequiredImport, nameOccKey)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (pooledForConcurrently)

resolveDefinitionSourceWithSummaries ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  GHC.Name ->
  m (Maybe DefinitionSource)
resolveDefinitionSourceWithSummaries modSummaries inputName =
  case GHC.nameModule_maybe inputName of
    Nothing ->
      pure Nothing
    Just definingModule -> do
      maybeModuleIndex <- getDefinitionModuleIndex modSummaries definingModule
      pure $
        maybeModuleIndex >>= DefinitionIndex.lookupDefinitionSourceByName inputName

resolveDefinitionDependencyNames ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  DefinitionSource ->
  m [GHC.Name]
resolveDefinitionDependencyNames modSummaries source = do
  maybeModuleIndex <- getDefinitionModuleIndex modSummaries source.definitionSourceModule
  pure $
    maybe
      []
      ( \moduleIndex ->
          let dependencies =
                DefinitionIndex.lookupDefinitionDependenciesOrEmpty source.definitionSourceId moduleIndex
           in Set.toList (dependencies.dependencyDirectReferenceNames <> dependencies.dependencyUsedInstanceNames)
      )
      maybeModuleIndex

resolveDefinitionClosureSourcesWithSummaries ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  Int ->
  GHC.Name ->
  m [NamedDefinitionSource]
resolveDefinitionClosureSourcesWithSummaries modSummaries maxDepth inputName = do
  let depth = max 0 maxDepth
  (_, sourceBuilder) <- go depth inputName Set.empty
  pure (sourceBuilder [])
  where
    -- Closure output is dependency-first: nested dependencies come before dependents,
    -- and the queried root comes last when depth allows recursion.
    go ::
      (MonadLore m) =>
      Int ->
      GHC.Name ->
      Set.Set DefinitionId ->
      m (Set.Set DefinitionId, [NamedDefinitionSource] -> [NamedDefinitionSource])
    go depth name seen = do
      maybeSource <- resolveDefinitionSourceWithSummaries modSummaries name
      case maybeSource of
        Nothing ->
          pure (seen, id)
        Just source ->
          let definitionId = source.definitionSourceId
           in if Set.member definitionId seen
                then pure (seen, id)
                else
                  if depth == 0
                    then pure (Set.insert definitionId seen, (namedSource name source :))
                    else do
                      let seen' = Set.insert definitionId seen
                      dependencyNames <- resolveDefinitionDependencyNames modSummaries source
                      (seenAfterDependencies, dependencySourceBuilder) <-
                        collectDependencies (go (depth - 1)) seen' dependencyNames
                      pure
                        ( seenAfterDependencies,
                          dependencySourceBuilder . (namedSource name source :)
                        )

    namedSource name source =
      NamedDefinitionSource
        { definitionName = name,
          definitionSource = source
        }

    collectDependencies ::
      (MonadLore m) =>
      (GHC.Name -> Set.Set DefinitionId -> m (Set.Set DefinitionId, [NamedDefinitionSource] -> [NamedDefinitionSource])) ->
      Set.Set DefinitionId ->
      [GHC.Name] ->
      m (Set.Set DefinitionId, [NamedDefinitionSource] -> [NamedDefinitionSource])
    collectDependencies _ seen [] =
      pure (seen, id)
    collectDependencies resolve seen (dependencyName : remainingNames) = do
      (seenAfterDependency, dependencySourceBuilder) <- resolve dependencyName seen
      (seenAfterRemaining, remainingSourceBuilder) <- collectDependencies resolve seenAfterDependency remainingNames
      pure
        ( seenAfterRemaining,
          dependencySourceBuilder . remainingSourceBuilder
        )

getMinifiedImportsForDefinitionWithSummaries ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  DefinitionSource ->
  m [RequiredImport]
getMinifiedImportsForDefinitionWithSummaries modSummaries source = do
  maybeModuleIndex <- getDefinitionModuleIndex modSummaries source.definitionSourceModule
  pure $
    maybe
      []
      (DefinitionIndex.lookupDefinitionRequiredImportsOrEmpty source.definitionSourceId)
      maybeModuleIndex

resolveReferenceMatchesForNamesWithSummaries ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  [GHC.Name] ->
  m [ReferenceMatch]
resolveReferenceMatchesForNamesWithSummaries modSummaries targetNames = do
  let targetSet = Set.fromList targetNames
      targetOccKeys = Set.fromList (map nameOccKey targetNames)
  ParsedOccurrenceModuleIndex occurrenceIndex <-
    withTimedSection "findReferences:getParsedOccurrenceModuleIndex" $
      getParsedOccurrenceModuleIndex (buildParsedOccurrenceModuleIndex modSummaries)
  let candidateModules = lookupModulesForOccurrenceKeys targetOccKeys occurrenceIndex
  Log.debug $ "Resolved " <> show (Set.size targetOccKeys) <> " target occurrence names to " <> show (length candidateModules) <> " candidate modules."
  preparedModules <-
    withTimedSection "findReferences:prepareCandidateModules" $
      prepareCandidateModuleIndexes modSummaries candidateModules
  resolvedMatches <-
    withTimedSection "findReferences:analyzePreparedModules" $
      concat <$> pooledForConcurrently preparedModules (liftIO . Exception.evaluate . matchingReferenceMatches targetSet)
  withTimedSection "findReferences:forceMatches" $
    liftIO (Exception.evaluate (forceReferenceMatchesForRendering resolvedMatches))

matchingReferenceMatches :: Set.Set GHC.Name -> DefinitionModuleIndex -> [ReferenceMatch]
matchingReferenceMatches targetSet moduleIndex =
  DefinitionIndex.lookupReferenceMatchesForNames targetSet moduleIndex

forceReferenceMatchesForRendering :: [ReferenceMatch] -> [ReferenceMatch]
forceReferenceMatchesForRendering matches =
  forceMatches matches `seq` matches
  where
    forceMatches [] = ()
    forceMatches (referenceMatch : restMatches) =
      referenceMatch.referenceMatchDefinition.definitionSourceModule `deepseq`
        referenceMatch.referenceMatchDefinition.definitionSourceSpans `deepseq`
          referenceMatch.referenceMatchOccurrences `deepseq`
            forceMatches restMatches
