module Lore.Internal.Definition.Index
  ( emptyDefinitionDependencies,
    lookupDefinitionSourceByName,
    lookupDefinitionSourceById,
    lookupDefinitionDependenciesMaybe,
    lookupDefinitionDependenciesOrEmpty,
    lookupDefinitionRequiredImportsMaybe,
    lookupDefinitionRequiredImportsOrEmpty,
    lookupReferenceHitsForName,
    lookupReferenceHitsForNames,
    lookupReferenceMatchesForNames,
    groupReferenceHitsByDefinition,
    dedupeReferenceHits,
  )
where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Definition.Types
  ( DefinitionDependencies (..),
    DefinitionId,
    DefinitionModuleIndex (..),
    DefinitionSource,
    ReferenceHit (..),
    ReferenceMatch (..),
    RequiredImport,
    nameOccKey,
    srcSpanKey,
  )

emptyDefinitionDependencies :: DefinitionDependencies
emptyDefinitionDependencies =
  DefinitionDependencies
    { dependencyDirectReferenceNames = Set.empty,
      dependencyUsedInstanceNames = Set.empty
    }

lookupDefinitionSourceByName ::
  GHC.Name ->
  DefinitionModuleIndex ->
  Maybe DefinitionSource
lookupDefinitionSourceByName name moduleIndex = do
  definitionId <- Map.lookup name moduleIndex.definitionIdByName
  lookupDefinitionSourceById definitionId moduleIndex

lookupDefinitionSourceById ::
  DefinitionId ->
  DefinitionModuleIndex ->
  Maybe DefinitionSource
lookupDefinitionSourceById definitionId moduleIndex =
  Map.lookup definitionId moduleIndex.definitionsById

lookupDefinitionDependenciesMaybe ::
  DefinitionId ->
  DefinitionModuleIndex ->
  Maybe DefinitionDependencies
lookupDefinitionDependenciesMaybe definitionId moduleIndex =
  Map.lookup definitionId moduleIndex.dependenciesById

lookupDefinitionDependenciesOrEmpty ::
  DefinitionId ->
  DefinitionModuleIndex ->
  DefinitionDependencies
lookupDefinitionDependenciesOrEmpty definitionId moduleIndex =
  Map.findWithDefault emptyDefinitionDependencies definitionId moduleIndex.dependenciesById

lookupDefinitionRequiredImportsMaybe ::
  DefinitionId ->
  DefinitionModuleIndex ->
  Maybe [RequiredImport]
lookupDefinitionRequiredImportsMaybe definitionId moduleIndex =
  Map.lookup definitionId moduleIndex.requiredImportsById

lookupDefinitionRequiredImportsOrEmpty ::
  DefinitionId ->
  DefinitionModuleIndex ->
  [RequiredImport]
lookupDefinitionRequiredImportsOrEmpty definitionId moduleIndex =
  Map.findWithDefault [] definitionId moduleIndex.requiredImportsById

lookupReferenceHitsForName ::
  GHC.Name ->
  DefinitionModuleIndex ->
  [ReferenceHit]
lookupReferenceHitsForName targetName moduleIndex =
  [ hit
  | hit <- Map.findWithDefault [] (nameOccKey targetName) moduleIndex.referenceHitsByOccKey,
    -- OccKey is only a candidate index. Exact Name equality is required to avoid
    -- mixing same-occurrence names from different modules/classes/constructors.
    hit.referenceHitTargetName == targetName
  ]

lookupReferenceHitsForNames ::
  Set.Set GHC.Name ->
  DefinitionModuleIndex ->
  [ReferenceHit]
lookupReferenceHitsForNames targetNames moduleIndex =
  concatMap (`lookupReferenceHitsForName` moduleIndex) (Set.toList targetNames)

lookupReferenceMatchesForNames ::
  Set.Set GHC.Name ->
  DefinitionModuleIndex ->
  [ReferenceMatch]
lookupReferenceMatchesForNames targetNames moduleIndex =
  groupReferenceHitsByDefinition
    moduleIndex
    (lookupReferenceHitsForNames targetNames moduleIndex)

groupReferenceHitsByDefinition ::
  DefinitionModuleIndex ->
  [ReferenceHit] ->
  [ReferenceMatch]
groupReferenceHitsByDefinition moduleIndex hits =
  [ ReferenceMatch
      { referenceMatchDefinition = source,
        referenceMatchOccurrences = dedupeReferenceHits groupedHits
      }
  | (definitionId, groupedHits) <- Map.toList groupedByDefinitionId,
    Just source <- [lookupDefinitionSourceById definitionId moduleIndex]
  ]
  where
    groupedByDefinitionId =
      Map.fromListWith
        mergeHits
        [ (hit.referenceHitDefinitionId, [hit])
        | hit <- hits
        ]

    mergeHits newHits oldHits =
      oldHits <> newHits

dedupeReferenceHits :: [ReferenceHit] -> [ReferenceHit]
dedupeReferenceHits =
  reverse . snd . foldl' go (Set.empty, [])
  where
    go (seen, hits) hit
      | hitKey `Set.member` seen =
          (seen, hits)
      | otherwise =
          (Set.insert hitKey seen, hit : hits)
      where
        hitKey =
          ( hit.referenceHitTargetName,
            srcSpanKey hit.referenceHitExactSpan
          )
