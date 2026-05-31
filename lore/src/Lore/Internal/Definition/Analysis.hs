module Lore.Internal.Definition.Analysis
  ( collectParsedOccurrenceNames,
    buildParsedModuleFacts,
    buildMinimalTypedModuleFacts,
    buildDefinitionBindings,
    buildDefinitionMemberIndexes,
    buildDefinitionOccurrences,
    buildReferenceHitsByOccKey,
    buildDependencies,
    buildDefinitionModuleIndex,
    buildEvidenceDependenciesByBinder,
    buildSemanticDependenciesByBinder,
  )
where

import qualified Data.Map.Strict as Map
import qualified GHC
import Lore.Internal.Definition.Analysis.Bindings (buildDefinitionBindings)
import Lore.Internal.Definition.Analysis.Core (buildEvidenceDependenciesByBinder, buildSemanticDependenciesByBinder)
import Lore.Internal.Definition.Analysis.Dependencies (buildDependencies)
import Lore.Internal.Definition.Analysis.Members (buildDefinitionMemberIndexes)
import Lore.Internal.Definition.Analysis.Occurrences (buildDefinitionOccurrences, buildReferenceHitsByOccKey)
import Lore.Internal.Definition.Analysis.Parsed (buildParsedModuleFacts, collectParsedOccurrenceNames)
import Lore.Internal.Definition.Analysis.Typed (buildMinimalTypedModuleFacts)
import Lore.Internal.Definition.Types

data DefinitionAssembly = DefinitionAssembly
  { assemblyBindings :: !DefinitionBindings,
    assemblyMemberIndexes :: !(Map.Map DefinitionId DefinitionMemberIndex),
    assemblyOccurrences :: !(Map.Map DefinitionId [DefinitionOccurrenceFact])
  }

buildDefinitionModuleIndex ::
  GHC.Module ->
  ParsedModuleFacts ->
  MinimalTypedModuleFacts ->
  Maybe MinimalCoreModuleFacts ->
  DefinitionModuleIndex
buildDefinitionModuleIndex definingModule parsedFacts typedModuleFacts maybeCoreFacts =
  DefinitionModuleIndex
    { definitionsById = assembly.assemblyBindings.bindingDefinitionsById,
      definitionIdByName = assembly.assemblyBindings.bindingDefinitionIdByName,
      referenceHitsByOccKey = buildReferenceHitsByOccKey assembly.assemblyOccurrences,
      dependenciesById =
        buildDependencies
          assembly.assemblyBindings
          assembly.assemblyMemberIndexes
          assembly.assemblyOccurrences
          maybeCoreFacts
    }
  where
    assembly =
      buildDefinitionAssembly definingModule parsedFacts typedModuleFacts

buildDefinitionAssembly ::
  GHC.Module ->
  ParsedModuleFacts ->
  MinimalTypedModuleFacts ->
  DefinitionAssembly
buildDefinitionAssembly definingModule parsedFacts typedModuleFacts =
  DefinitionAssembly
    { assemblyBindings = bindings,
      assemblyMemberIndexes = memberIndexesById,
      assemblyOccurrences = occurrencesById
    }
  where
    bindings =
      buildDefinitionBindings definingModule parsedFacts typedModuleFacts

    memberIndexesById =
      buildDefinitionMemberIndexes parsedFacts typedModuleFacts bindings

    occurrencesById =
      buildDefinitionOccurrences definingModule typedModuleFacts bindings memberIndexesById
