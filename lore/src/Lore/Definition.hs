module Lore.Definition
  ( -- Source-first definition API.
    resolveDefinitionSourceNamed,
    resolveDefinitionClosureSourcesNamed,
    resolveReferenceMatchesForNames,
    matchingReferenceMatches,
    dedupeReferenceHits,
    lookupModulesForOccurrenceKeys,
    mergeDefinitionSlices,
    DefinitionId (..),
    DefinitionSource (..),
    -- Rendering DTO used by existing renderers.
    DefinitionSlice (..),
    NamedDefinitionSource (..),
    DeclarationSpans (..),
    ReferenceHit (..),
    ReferenceMatch (..),
  )
where

import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.ParsedOccurrenceModuleIndex (lookupModulesForOccurrenceKeys)
import qualified Lore.Internal.Definition.Index as DefinitionIndex
import qualified Lore.Internal.Definition.Query as DefinitionQuery
import Lore.Internal.Definition.Timing (withTimedSection)
import Lore.Internal.Definition.Types (DeclarationSpans (..), DefinitionId (..), DefinitionModuleIndex, DefinitionSlice (..), DefinitionSource (..), NamedDefinitionSource (..), ReferenceHit (..), ReferenceMatch (..))
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

resolveDefinitionSourceNamed :: (MonadLore m) => GHC.Name -> m (Maybe DefinitionSource)
resolveDefinitionSourceNamed inputName = do
  ModSummaries modSummaries <- getCachedModSummaries
  DefinitionQuery.resolveDefinitionSourceWithSummaries modSummaries inputName

resolveReferenceMatchesForNames :: (MonadLore m) => [GHC.Name] -> m [ReferenceMatch]
resolveReferenceMatchesForNames targetNames = do
  ModSummaries modSummaries <-
    withTimedSection "findReferences:getModSummaries" getCachedModSummaries
  forcedMatches <-
    DefinitionQuery.resolveReferenceMatchesForNamesWithSummaries modSummaries targetNames
  Log.debug $
    "Finished resolving reference matches. Found "
      <> show (length forcedMatches)
      <> " matches in total"
  pure forcedMatches

matchingReferenceMatches :: Set.Set GHC.Name -> DefinitionModuleIndex -> [ReferenceMatch]
matchingReferenceMatches =
  DefinitionQuery.matchingReferenceMatches

dedupeReferenceHits :: [ReferenceHit] -> [ReferenceHit]
dedupeReferenceHits =
  DefinitionIndex.dedupeReferenceHits

resolveDefinitionClosureSourcesNamed :: (MonadLore m) => Int -> GHC.Name -> m [NamedDefinitionSource]
resolveDefinitionClosureSourcesNamed maxDepth inputName = do
  ModSummaries modSummaries <- getCachedModSummaries
  DefinitionQuery.resolveDefinitionClosureSourcesWithSummaries modSummaries maxDepth inputName

mergeDefinitionSlices :: [DefinitionSlice] -> Maybe DefinitionSlice
mergeDefinitionSlices [] = Nothing
mergeDefinitionSlices (slice : slices)
  | all ((== slice.definitionModule) . definitionModule) slices =
      Just
        DefinitionSlice
          { definitionModule = slice.definitionModule,
            declarationSpans =
              dedupeDeclarationSpans . sortDeclarationSpans $
                concatMap declarationSpans allSlices
          }
  | otherwise =
      Nothing
  where
    allSlices = slice : slices

sortDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
sortDeclarationSpans =
  List.sortOn (GHC.srcSpanToRealSrcSpan . declarationSpan)

dedupeDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
dedupeDeclarationSpans =
  nubOrdOn (\declarationSpans -> (show declarationSpans.declarationSpan, fmap show declarationSpans.signatureSpan))
