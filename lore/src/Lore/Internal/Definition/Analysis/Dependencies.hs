module Lore.Internal.Definition.Analysis.Dependencies
  ( buildDependencies,
  )
where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lore.Internal.Definition.Analysis.Common (nameUniqueKey)
import Lore.Internal.Definition.Analysis.Occurrences (isFollowableReference)
import Lore.Internal.Definition.Types

buildDependencies ::
  DefinitionCatalog ->
  Map.Map DefinitionId DefinitionMemberIndex ->
  Map.Map DefinitionId [DefinitionOccurrenceFact] ->
  Maybe MinimalCoreModuleFacts ->
  Map.Map DefinitionId DefinitionDependencies
buildDependencies catalog memberIndexesById occurrencesById maybeCoreFacts =
  Map.mapWithKey mkDependencies catalog.definitionSourcesById
  where
    coreEvidenceDependenciesByBinder =
      maybe Map.empty (.coreEvidenceDependenciesByBinder) maybeCoreFacts

    coreSemanticDependenciesByBinder =
      IntMap.fromListWith
        (<>)
        [ (nameUniqueKey binderName, semanticNames)
        | (binderName, semanticNames) <-
            Map.toList (maybe Map.empty (.coreSemanticDependenciesByBinder) maybeCoreFacts)
        ]

    mkDependencies definitionId source =
      DefinitionDependencies
        { dependencyClosureNamesByReferenceName = closureNamesByReferenceName,
          dependencyReachabilityNames = reachabilityNames
        }
      where
        definitionNames = source.definitionSourceNames
        rootNames =
          memberIndex.rootMemberNames
        memberIndex =
          memberIndexesById Map.! definitionId
        followableOccurrences =
          [ occurrence
          | occurrence <- occurrencesById Map.! definitionId,
            isFollowableReference definitionNames source.definitionSourceSpans occurrence.occurrenceFactName
          ]
        directReferencesByReferenceNameRaw =
          Map.fromListWith
            Set.union
            [ (ownerName, Set.singleton occurrence.occurrenceFactName)
            | occurrence <- followableOccurrences,
              ownerName <- ownerNamesForOccurrence definitionNames occurrence
            ]
        usedInstancesByReferenceNameRaw =
          Map.fromListWith
            Set.union
            [ (binderName, Set.singleton instanceName)
            | binderName <- Set.toList definitionNames,
              instanceName <- Map.findWithDefault [] binderName coreEvidenceDependenciesByBinder
            ]
        directReferencesByReferenceName =
          completeDependencyMap
            rootNames
            directReferencesByReferenceNameRaw
        usedInstancesByReferenceName =
          completeDependencyMap
            rootNames
            usedInstancesByReferenceNameRaw
        closureNamesByReferenceName =
          Map.unionWith
            Set.union
            directReferencesByReferenceName
            usedInstancesByReferenceName
        coreSemanticNames =
          Set.fromList
            [ semanticName
            | definitionName <- Set.toList definitionNames,
              semanticName <- IntMap.findWithDefault [] (nameUniqueKey definitionName) coreSemanticDependenciesByBinder
            ]
        reachabilityNames =
          Set.unions (Map.elems directReferencesByReferenceName)
            <> coreSemanticNames

    ownerNamesForOccurrence definitionNames occurrence =
      Set.toList (Set.intersection definitionNames occurrence.occurrenceFactOwners)

    completeDependencyMap rootNames rawDependenciesByName =
      augmentRootEntries
        rootNames
        (Set.unions (Map.elems rawDependenciesByName))
        rawDependenciesByName

    augmentRootEntries rootNames allDependencies dependenciesByName =
      Set.foldl'
        (\acc rootName -> Map.insertWith Set.union rootName allDependencies acc)
        dependenciesByName
        rootNames
