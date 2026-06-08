module Lore.Internal.Definition.Query
  ( resolveDefinitionSourceWithSummaries,
    resolveDefinitionClosureSources,
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
import Lore.Internal.Definition.Cache.DefinitionModuleIndex (getCachedDefinitionModuleIndex, getCachedDefinitionModuleIndexes)
import Lore.Internal.Definition.Cache.ParsedOccurrenceModuleIndex (getCachedParsedOccurrenceModuleIndex, lookupModulesForOccurrenceKeys)
import qualified Lore.Internal.Definition.Index as DefinitionIndex
import Lore.Internal.Definition.ProjectIndex
  ( DefinitionTarget (..),
    ProjectDefinitionIndex,
    loadProjectDefinitionIndex,
    lookupDefinitionSource,
    lookupDefinitionTarget,
  )
import Lore.Internal.Definition.Reachability (reachableNamedTargets)
import Lore.Internal.Definition.Timing (withTimedSection)
import Lore.Internal.Definition.Types (DefinitionModuleIndex, DefinitionSource (..), NamedDefinitionSource (..), ParsedOccurrenceModuleIndex (..), ReferenceMatch (..), definitionSourceModule, nameOccKey)
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
      maybeModuleIndex <- getCachedDefinitionModuleIndex modSummaries definingModule
      pure $
        maybeModuleIndex >>= DefinitionIndex.lookupDefinitionSourceByName inputName

resolveDefinitionClosureSources ::
  (MonadLore m) =>
  Int ->
  GHC.Name ->
  m [NamedDefinitionSource]
resolveDefinitionClosureSources maxDepth inputName = do
  projectIndex <- loadProjectDefinitionIndex
  pure $
    case lookupDefinitionTarget projectIndex inputName of
      Nothing ->
        []
      Just root ->
        mapMaybeTargetSource projectIndex $
          dependencyFirstTargets $
            reachableNamedTargets maxDepth projectIndex [root]
  where
    dependencyFirstTargets =
      reverse

    mapMaybeTargetSource :: ProjectDefinitionIndex -> [DefinitionTarget] -> [NamedDefinitionSource]
    mapMaybeTargetSource projectIndex =
      foldr collectSource []
      where
        collectSource :: DefinitionTarget -> [NamedDefinitionSource] -> [NamedDefinitionSource]
        collectSource target sources =
          case lookupDefinitionSource projectIndex target.definitionTargetId of
            Nothing ->
              sources
            Just source ->
              namedSource target source : sources

    namedSource target source =
      NamedDefinitionSource
        { definitionName = target.definitionTargetName,
          definitionSource = source
        }

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
      getCachedParsedOccurrenceModuleIndex modSummaries
  let candidateModules = lookupModulesForOccurrenceKeys targetOccKeys occurrenceIndex
  Log.debug $ "Resolved " <> show (Set.size targetOccKeys) <> " target occurrence names to " <> show (length candidateModules) <> " candidate modules."
  preparedModules <-
    withTimedSection "findReferences:prepareCandidateModules" $
      getCachedDefinitionModuleIndexes modSummaries candidateModules
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
      definitionSourceModule referenceMatch.referenceMatchDefinition `deepseq`
        referenceMatch.referenceMatchDefinition.definitionSourceSpans `deepseq`
          referenceMatch.referenceMatchOccurrences `deepseq`
            forceMatches restMatches
