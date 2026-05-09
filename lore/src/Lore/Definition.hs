module Lore.Definition
  ( -- Source-first definition API.
    resolveDefinitionSourceNamed,
    resolveDefinitionClosureSourcesNamed,
    getMinifiedImportsForDefinition,
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
    RequiredImport (..),
    ImportQualifiedStyle (..),
    RequiredImportItem (..),
  )
where

import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.ParsedOccurrenceModuleIndex (lookupModulesForOccurrenceKeys)
import qualified Lore.Internal.Definition.Index as DefinitionIndex
import qualified Lore.Internal.Definition.Query as DefinitionQuery
import Lore.Internal.Definition.RequiredImports (normalizeImportItems)
import Lore.Internal.Definition.Timing (withTimedSection)
import Lore.Internal.Definition.Types (DeclarationSpans (..), DefinitionId (..), DefinitionModuleIndex, DefinitionSlice (..), DefinitionSource (..), ImportQualifiedStyle (..), NamedDefinitionSource (..), ReferenceHit (..), ReferenceMatch (..), RequiredImport (..), RequiredImportItem (..))
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

resolveDefinitionSourceNamed :: (MonadLore m) => GHC.Name -> m (Maybe DefinitionSource)
resolveDefinitionSourceNamed inputName = do
  ModSummaries modSummaries <- getCachedModSummaries
  DefinitionQuery.resolveDefinitionSourceWithSummaries modSummaries inputName

getMinifiedImportsForDefinition :: (MonadLore m) => DefinitionSource -> m [RequiredImport]
getMinifiedImportsForDefinition source = do
  ModSummaries modSummaries <- getCachedModSummaries
  DefinitionQuery.getMinifiedImportsForDefinitionWithSummaries modSummaries source

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
                concatMap declarationSpans allSlices,
            requiredImports =
              mergeImports $
                concatMap requiredImports allSlices
          }
  | otherwise =
      Nothing
  where
    allSlices = slice : slices

mergeImports :: [RequiredImport] -> [RequiredImport]
mergeImports =
  IntMap.elems . foldl insertImport IntMap.empty
  where
    insertImport acc import' =
      IntMap.insertWith mergeImport import'.importKey import' acc

    mergeImport new old =
      old
        { importOriginallyExplicit = old.importOriginallyExplicit || new.importOriginallyExplicit,
          importItems = normalizeImportItems (old.importItems <> new.importItems)
        }

sortDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
sortDeclarationSpans =
  List.sortOn (GHC.srcSpanToRealSrcSpan . declarationSpan)

dedupeDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
dedupeDeclarationSpans =
  nubOrdOn (\declarationSpans -> (show declarationSpans.declarationSpan, fmap show declarationSpans.signatureSpan))
