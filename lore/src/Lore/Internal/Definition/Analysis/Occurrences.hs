module Lore.Internal.Definition.Analysis.Occurrences
  ( buildDefinitionOccurrences,
    buildReferenceIndex,
    isFollowableReference,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Analysis.Members (chooseOccurrenceOwners)
import Lore.Internal.Definition.Types

buildDefinitionOccurrences ::
  GHC.Module ->
  MinimalTypedModuleFacts ->
  DefinitionCatalog ->
  Map.Map DefinitionId DefinitionMemberIndex ->
  Map.Map DefinitionId [DefinitionOccurrenceFact]
buildDefinitionOccurrences definingModule typedModuleFacts catalog memberIndexesById =
  Map.map mkOccurrences catalog.definitionSourcesById
  where
    mkOccurrences source =
      let memberIndex =
            memberIndexesById Map.! source.definitionSourceId
       in collectDefinitionOccurrenceFacts
            definingModule
            source.definitionSourceSpans
            memberIndex
            typedModuleFacts.typedDefinitionFacts.typedOccurrences

buildReferenceIndex ::
  Map.Map DefinitionId [DefinitionOccurrenceFact] ->
  ReferenceIndex
buildReferenceIndex occurrencesById =
  ReferenceIndex $
    Map.fromListWith
      (Map.unionWith Map.union)
      [ ( occurrence.occurrenceFactName,
          Map.singleton
            definitionId
            (Map.singleton (srcSpanKey occurrence.occurrenceFactSpan) occurrence.occurrenceFactSpan)
        )
      | (definitionId, occurrences) <- Map.toList occurrencesById,
        occurrence <- occurrences
      ]

collectDefinitionOccurrenceFacts ::
  GHC.Module ->
  DeclarationSpans ->
  DefinitionMemberIndex ->
  [MinimalTypedOccurrence] ->
  [DefinitionOccurrenceFact]
collectDefinitionOccurrenceFacts definingModule spans memberIndex typedOccurrences =
  dedupeOccurrences $
    mapMaybe toReferencedOccurrence filteredOccurrences
  where
    targetSpans =
      spans.declarationSpan
        : maybeToList spans.signatureSpan

    filteredOccurrences =
      [ occurrence
      | occurrence <- typedOccurrences,
        spanWithin targetSpans occurrence.typedOccurrenceSpan
      ]

    toReferencedOccurrence occurrence = do
      let occurrenceName = occurrence.typedOccurrenceName
      guardReference definingModule spans occurrenceName $
        DefinitionOccurrenceFact
          { occurrenceFactName = occurrenceName,
            occurrenceFactSpan = occurrence.typedOccurrenceSpan,
            occurrenceFactOwners =
              chooseOccurrenceOwners
                memberIndex
                occurrence.typedOccurrenceParent
                occurrence.typedOccurrenceSpan
          }

isFollowableReference :: Set.Set GHC.Name -> DeclarationSpans -> GHC.Name -> Bool
isFollowableReference definitionNames spans name =
  Set.notMember name definitionNames
    && case GHC.nameModule_maybe name of
      Nothing -> False
      Just definingModule ->
        not (definesName spans.declarationSpan definingModule name)

guardReference ::
  GHC.Module ->
  DeclarationSpans ->
  GHC.Name ->
  DefinitionOccurrenceFact ->
  Maybe DefinitionOccurrenceFact
guardReference definingModule spans occurrenceName occurrence
  | definesName spans.declarationSpan definingModule occurrenceName = Nothing
  | otherwise = Just occurrence

definesName :: GHC.SrcSpan -> GHC.Module -> GHC.Name -> Bool
definesName declarationSpan definingModule name =
  GHC.nameModule_maybe name == Just definingModule
    && GHC.nameSrcSpan name `GHC.isSubspanOf` declarationSpan

dedupeOccurrences :: [DefinitionOccurrenceFact] -> [DefinitionOccurrenceFact]
dedupeOccurrences =
  dedupeOn \occurrence ->
    ( occurrence.occurrenceFactName,
      srcSpanKey occurrence.occurrenceFactSpan,
      occurrence.occurrenceFactOwners
    )

dedupeOn :: (Ord key) => (value -> key) -> [value] -> [value]
dedupeOn keyOf =
  reverse . snd . List.foldl' step (Set.empty, [])
  where
    step (seen, values) value
      | key `Set.member` seen = (seen, values)
      | otherwise = (Set.insert key seen, value : values)
      where
        key = keyOf value

spanWithin :: [GHC.SrcSpan] -> GHC.SrcSpan -> Bool
spanWithin targetSpans span' =
  any (span' `GHC.isSubspanOf`) targetSpans
