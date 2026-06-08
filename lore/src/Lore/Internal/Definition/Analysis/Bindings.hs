module Lore.Internal.Definition.Analysis.Bindings
  ( buildDefinitionCatalog,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Definition.Types

buildDefinitionCatalog ::
  ParsedModuleFacts ->
  MinimalTypedModuleFacts ->
  DefinitionCatalog
buildDefinitionCatalog parsedFacts typedModuleFacts =
  DefinitionCatalog
    { definitionSourcesById = definitionsById,
      definitionIdsByName = definitionIdByName
    }
  where
    matchedDefinitions =
      [ (definitionId, definitionName)
      | definitionName <- typedModuleFacts.typedNameFacts.typedDefinitionNames,
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
              definitionSourceNames = names,
              definitionSourceSpans = spans
            }

    definitionIdByName =
      Map.fromList
        [ (definitionName, definitionId)
        | (definitionId, definitionName) <- matchedDefinitions
        ]
