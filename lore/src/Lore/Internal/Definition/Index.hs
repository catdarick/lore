module Lore.Internal.Definition.Index
  ( lookupDefinitionSourceByName,
    lookupDefinitionSourceById,
    lookupReferenceMatchesForNames,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Definition.Types
  ( DefinitionCatalog (..),
    DefinitionId,
    DefinitionModuleIndex (..),
    DefinitionSource,
    ReferenceHit (..),
    ReferenceIndex (..),
    ReferenceMatch (..),
    SpanKey,
  )

lookupDefinitionSourceByName ::
  GHC.Name ->
  DefinitionModuleIndex ->
  Maybe DefinitionSource
lookupDefinitionSourceByName name moduleIndex = do
  definitionId <- Map.lookup name moduleIndex.definitionCatalog.definitionIdsByName
  lookupDefinitionSourceById definitionId moduleIndex

lookupDefinitionSourceById ::
  DefinitionId ->
  DefinitionModuleIndex ->
  Maybe DefinitionSource
lookupDefinitionSourceById definitionId moduleIndex =
  Map.lookup definitionId moduleIndex.definitionCatalog.definitionSourcesById

lookupReferenceMatchesForNames ::
  Set.Set GHC.Name ->
  DefinitionModuleIndex ->
  [ReferenceMatch]
lookupReferenceMatchesForNames targetNames moduleIndex =
  [ ReferenceMatch
      { referenceMatchDefinition = source,
        referenceMatchOccurrences = referenceHits
      }
  | (definitionId, referenceHits) <- Map.toList referenceHitsByDefinition,
    Just source <- [lookupDefinitionSourceById definitionId moduleIndex]
  ]
  where
    referenceHitsByDefinition =
      Map.fromListWith
        (<>)
        [ (definitionId, [referenceHit])
        | targetName <- Set.toList targetNames,
          (definitionId, exactSpans) <- Map.toList (lookupReferenceSpansForName targetName moduleIndex),
          exactSpan <- Map.elems exactSpans,
          let referenceHit =
                ReferenceHit
                  { referenceHitDefinitionId = definitionId,
                    referenceHitTargetName = targetName,
                    referenceHitExactSpan = exactSpan
                  }
        ]

lookupReferenceSpansForName ::
  GHC.Name ->
  DefinitionModuleIndex ->
  Map.Map DefinitionId (Map.Map SpanKey GHC.SrcSpan)
lookupReferenceSpansForName targetName moduleIndex =
  Map.findWithDefault Map.empty targetName moduleIndex.referenceIndex.referencesByName
