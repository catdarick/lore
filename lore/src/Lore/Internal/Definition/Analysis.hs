module Lore.Internal.Definition.Analysis
  ( collectParsedOccurrenceNames,
    buildParsedModuleFacts,
    buildMinimalTypedModuleFacts,
    buildDefinitionCatalog,
    buildDefinitionMemberIndexes,
    buildDefinitionOccurrences,
    buildReferenceIndex,
    buildDependencies,
    buildDefinitionModuleIndex,
    buildCoreDependenciesByBinder,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Definition.Analysis.Bindings (buildDefinitionCatalog)
import Lore.Internal.Definition.Analysis.Core (buildCoreDependenciesByBinder)
import Lore.Internal.Definition.Analysis.Dependencies (buildDependencies)
import Lore.Internal.Definition.Analysis.Members (buildDefinitionMemberIndexes)
import Lore.Internal.Definition.Analysis.Occurrences (buildDefinitionOccurrences, buildReferenceIndex)
import Lore.Internal.Definition.Analysis.Parsed (buildParsedModuleFacts, collectParsedOccurrenceNames)
import Lore.Internal.Definition.Analysis.Typed (buildMinimalTypedModuleFacts)
import Lore.Internal.Definition.Types

buildDefinitionModuleIndex ::
  GHC.Module ->
  ParsedModuleFacts ->
  MinimalTypedModuleFacts ->
  Maybe MinimalCoreModuleFacts ->
  DefinitionModuleIndex
buildDefinitionModuleIndex definingModule parsedFacts typedModuleFacts maybeCoreFacts =
  DefinitionModuleIndex
    { definitionCatalog = catalog,
      referenceIndex = buildReferenceIndex occurrencesById,
      dependenciesById =
        buildDependencies
          catalog
          memberIndexesById
          occurrencesById
          maybeCoreFacts,
      instanceHeadTypeDefinitionIdsByInstance =
        collectInstanceHeadTypeDefinitionIdsByInstance
          catalog.definitionIdsByName
          typedModuleFacts
    }
  where
    catalog =
      buildDefinitionCatalog parsedFacts typedModuleFacts

    memberIndexesById =
      buildDefinitionMemberIndexes parsedFacts typedModuleFacts catalog

    occurrencesById =
      buildDefinitionOccurrences definingModule typedModuleFacts catalog memberIndexesById

collectInstanceHeadTypeDefinitionIdsByInstance ::
  Map.Map GHC.Name DefinitionId ->
  MinimalTypedModuleFacts ->
  Map.Map DefinitionId (Set.Set DefinitionId)
collectInstanceHeadTypeDefinitionIdsByInstance definitionIdByName typedFacts =
  Map.fromList
    [ (instanceDefinitionId, headTypeDefinitionIds)
    | (instanceName, headTypeNames) <- Map.toList typedFacts.typedInstanceFacts.typedInstanceHeadTypeNamesByInstance,
      Just instanceDefinitionId <- [Map.lookup instanceName definitionIdByName],
      let headTypeDefinitionIds =
            Set.fromList
              [ definitionId
              | headTypeName <- Set.toList headTypeNames,
                Just definitionId <- [Map.lookup headTypeName definitionIdByName]
              ]
    ]
