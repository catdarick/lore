module Lore.Internal.Definition.Analysis.Bindings
  ( buildDefinitionBindings,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Definition.Types

buildDefinitionBindings ::
  GHC.Module ->
  ParsedModuleFacts ->
  MinimalTypedModuleFacts ->
  DefinitionBindings
buildDefinitionBindings definingModule parsedFacts typedModuleFacts =
  DefinitionBindings
    { bindingDefinitionsById = definitionsById,
      bindingDefinitionIdByName = definitionIdByName
    }
  where
    matchedDefinitions =
      [ (definitionId, definitionName)
      | definitionName <- typedModuleFacts.typedDefinitionNames,
        Just definitionId <- [matchDefinitionId definitionName]
      ]

    matchDefinitionId definitionName =
      fst
        <$> List.find
          (\(_, spans) -> GHC.nameSrcSpan definitionName `GHC.isSubspanOf` spans.declarationSpan)
          (Map.toList parsedFacts.parsedDeclarationsById)

    definitionNamesById =
      Map.fromListWith
        (<>)
        [ (definitionId, Set.singleton definitionName)
        | (definitionId, definitionName) <- matchedDefinitions
        ]

    definitionsById =
      Map.mapWithKey mkDefinitionSource definitionNamesById

    mkDefinitionSource definitionId names =
      let spans = parsedFacts.parsedDeclarationsById Map.! definitionId
       in DefinitionSource
            { definitionSourceId = definitionId,
              definitionSourceModule = definingModule,
              definitionSourceNames = names,
              definitionSourceSpans = spans
            }

    definitionIdByName =
      Map.fromList
        [ (definitionName, definitionId)
        | (definitionId, definitionName) <- matchedDefinitions
        ]
