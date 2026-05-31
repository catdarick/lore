module Lore.Internal.Definition.Analysis.Occurrences
  ( buildDefinitionOccurrences,
    buildReferenceHitsByOccKey,
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
  DefinitionBindings ->
  Map.Map DefinitionId DefinitionMemberIndex ->
  Map.Map DefinitionId [DefinitionOccurrenceFact]
buildDefinitionOccurrences definingModule typedModuleFacts bindings memberIndexesById =
  Map.map mkOccurrences bindings.bindingDefinitionsById
  where
    mkOccurrences source =
      let memberIndex =
            memberIndexesById Map.! source.definitionSourceId
       in collectDefinitionOccurrenceFacts
            definingModule
            source.definitionSourceSpans
            memberIndex
            typedModuleFacts.typedOccurrences

buildReferenceHitsByOccKey ::
  Map.Map DefinitionId [DefinitionOccurrenceFact] ->
  Map.Map OccKey [ReferenceHit]
buildReferenceHitsByOccKey occurrencesById =
  Map.fromListWith
    (<>)
    [ (nameOccKey referenceHit.referenceHitTargetName, [referenceHit])
    | (definitionId, occurrences) <- Map.toList occurrencesById,
      occurrence <- occurrences,
      let referenceHit =
            ReferenceHit
              { referenceHitDefinitionId = definitionId,
                referenceHitTargetName = occurrence.occurrenceFactName,
                referenceHitExactSpan = occurrence.occurrenceFactSpan
              }
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
                occurrence.typedOccurrenceSpan,
            occurrenceFactParent = occurrence.typedOccurrenceParent
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
  List.nubBy sameOccurrence
  where
    sameOccurrence left right =
      left.occurrenceFactName == right.occurrenceFactName
        && left.occurrenceFactSpan == right.occurrenceFactSpan
        && left.occurrenceFactOwners == right.occurrenceFactOwners
        && left.occurrenceFactParent == right.occurrenceFactParent

spanWithin :: [GHC.SrcSpan] -> GHC.SrcSpan -> Bool
spanWithin targetSpans span' =
  any (span' `GHC.isSubspanOf`) targetSpans
