module Lore.Internal.Definition.Analysis.Dependencies
  ( buildDependencies,
  )
where

import Data.Foldable (foldl')
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lore.Internal.Definition.Analysis.Common (nameUniqueKey)
import Lore.Internal.Definition.Analysis.Occurrences (isFollowableReference)
import Lore.Internal.Definition.Types

buildDependencies ::
  DefinitionBindings ->
  Map.Map DefinitionId DefinitionMemberIndex ->
  Map.Map DefinitionId [DefinitionOccurrenceFact] ->
  Maybe MinimalCoreModuleFacts ->
  Map.Map DefinitionId DefinitionDependencies
buildDependencies bindings memberIndexesById occurrencesById maybeCoreFacts =
  Map.mapWithKey mkDependencies bindings.bindingDefinitionsById
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
      let memberIndex =
            Map.findWithDefault
              (DefinitionMemberIndex source.definitionSourceNames [])
              definitionId
              memberIndexesById
          definitionNames = source.definitionSourceNames
          rootNames =
            memberIndex.rootMemberNames
          followableOccurrences =
            [ occurrence
            | occurrence <- Map.findWithDefault [] definitionId occurrencesById,
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
              definitionNames
              rootNames
              directReferencesByReferenceNameRaw
          usedInstancesByReferenceName =
            completeDependencyMap
              definitionNames
              rootNames
              usedInstancesByReferenceNameRaw
          coreSemanticNames =
            [ semanticName
            | definitionName <- Set.toList definitionNames,
              semanticName <- IntMap.findWithDefault [] (nameUniqueKey definitionName) coreSemanticDependenciesByBinder
            ]
       in DefinitionDependencies
            { dependencyDirectReferenceNames =
                Set.unions (Map.elems directReferencesByReferenceName),
              dependencyUsedInstanceNames =
                Set.unions (Map.elems usedInstancesByReferenceName),
              dependencyCoreSemanticNames = coreSemanticNames,
              dependencyDirectReferenceNamesByReferenceName = directReferencesByReferenceName,
              dependencyUsedInstanceNamesByReferenceName = usedInstancesByReferenceName
            }

    ownerNamesForOccurrence definitionNames occurrence =
      Set.toList (Set.intersection definitionNames occurrence.occurrenceFactOwners)

    completeDependencyMap definitionNames rootNames rawDependenciesByName =
      augmentRootEntries
        rootNames
        (Set.unions (Map.elems rawDependenciesByName))
        (withDefaultEntries definitionNames rawDependenciesByName)

    withDefaultEntries definitionNames dependenciesByName =
      foldl'
        (\acc definitionName -> Map.insertWith (\_ old -> old) definitionName Set.empty acc)
        dependenciesByName
        (Set.toList definitionNames)

    augmentRootEntries rootNames allDependencies dependenciesByName =
      foldl'
        (\acc rootName -> Map.insertWith Set.union rootName allDependencies acc)
        dependenciesByName
        (Set.toList rootNames)
