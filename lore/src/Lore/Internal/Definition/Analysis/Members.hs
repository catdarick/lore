module Lore.Internal.Definition.Analysis.Members
  ( buildDefinitionMemberIndexes,
    chooseOccurrenceOwners,
    resolveDefinitionMemberIndex,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHC
import Lore.Internal.Definition.Types
import Lore.Internal.List (minimumMaybe)

buildDefinitionMemberIndexes ::
  ParsedModuleFacts ->
  MinimalTypedModuleFacts ->
  DefinitionCatalog ->
  Map.Map DefinitionId DefinitionMemberIndex
buildDefinitionMemberIndexes parsedFacts typedModuleFacts catalog =
  Map.map
    ( \source ->
        resolveDefinitionMemberIndex
          source
          parsedFacts.parsedDefinitionMembersById
          typedModuleFacts.typedNameFacts.typedDefinitionOccAliases
    )
    catalog.definitionSourcesById

resolveDefinitionMemberIndex ::
  DefinitionSource ->
  Map.Map DefinitionId [ParsedDefinitionMember] ->
  Map.Map GHC.Name (Set.Set Text) ->
  DefinitionMemberIndex
resolveDefinitionMemberIndex source parsedMembersById definitionOccAliases =
  DefinitionMemberIndex
    { rootMemberNames = rootNames,
      scopedMembers = scopedMembers
    }
  where
    parsedMembers =
      Map.findWithDefault [] source.definitionSourceId parsedMembersById

    scopedMembers =
      dedupeDefinitionMembersByNameSpan $ concatMap resolveParsedMember parsedMembers

    scopedMemberNames =
      Set.fromList (map memberName scopedMembers)

    rootCandidates =
      source.definitionSourceNames `Set.difference` scopedMemberNames

    rootNames
      | Set.null rootCandidates = source.definitionSourceNames
      | otherwise = rootCandidates

    namesByOccKey =
      Map.fromListWith
        (<>)
        [ (occKey, [definitionName])
        | definitionName <- Set.toList source.definitionSourceNames,
          occKey <- definitionNameOccKeys definitionName
        ]

    definitionNameOccKeys definitionName =
      nameOccKey definitionName
        : [ OccKey alias
          | alias <- Set.toList (Map.findWithDefault Set.empty definitionName definitionOccAliases)
          ]

    resolveParsedMember parsedMember =
      [ DefinitionMember definitionName parsedMember.parsedMemberSpan
      | definitionName <- memberNamesForParsedMember parsedMember
      ]

    memberNamesForParsedMember parsedMember =
      dedupeExactNames $
        explicitlyNamedMembers parsedMember
          <> sourceSpannedAliasMembers parsedMember

    explicitlyNamedMembers parsedMember =
      case Map.findWithDefault [] parsedMember.parsedMemberOccKey namesByOccKey of
        [] ->
          []
        [definitionName] ->
          [definitionName]
        candidateNames ->
          let namesWithinSpan = namesWithinMemberSpan candidateNames parsedMember.parsedMemberSpan
           in if null namesWithinSpan then candidateNames else namesWithinSpan

    sourceSpannedAliasMembers parsedMember =
      [ definitionName
      | definitionName <- Set.toList source.definitionSourceNames,
        Set.null (Set.fromList (definitionNameOccKeys definitionName) `Set.intersection` parsedMemberOccKeys),
        GHC.nameSrcSpan definitionName `GHC.isSubspanOf` parsedMember.parsedMemberSpan
      ]

    parsedMemberOccKeys =
      Set.fromList (map parsedMemberOccKey parsedMembers)

    namesWithinMemberSpan candidateNames memberSpan =
      [ candidateName
      | candidateName <- candidateNames,
        GHC.nameSrcSpan candidateName `GHC.isSubspanOf` memberSpan
      ]

chooseOccurrenceOwners ::
  DefinitionMemberIndex ->
  Maybe GHC.Name ->
  GHC.SrcSpan ->
  Set.Set GHC.Name
chooseOccurrenceOwners memberIndex maybeParent occurrenceSpan
  | not (Set.null narrowestOwners) =
      if not (Set.null narrowedParentOwners)
        then narrowedParentOwners
        else narrowestOwners
  | not (Set.null rootParentOwners) =
      rootParentOwners
  | otherwise =
      memberIndex.rootMemberNames
  where
    allDeclarationNames =
      memberIndex.rootMemberNames
        <> Set.fromList (map memberName memberIndex.scopedMembers)

    parentOwners =
      Set.fromList
        [ parentName
        | parentName <- maybeToList maybeParent,
          parentName `Set.member` allDeclarationNames
        ]

    rootParentOwners =
      Set.intersection parentOwners memberIndex.rootMemberNames

    containingMembers =
      [ member
      | member <- memberIndex.scopedMembers,
        occurrenceSpan `GHC.isSubspanOf` member.memberSpan
      ]

    narrowestSpanSize =
      minimumMaybe (map (memberSpanSize . memberSpan) containingMembers)

    narrowestOwners =
      case narrowestSpanSize of
        Nothing ->
          Set.empty
        Just minSize ->
          Set.fromList
            [ member.memberName
            | member <- containingMembers,
              memberSpanSize member.memberSpan == minSize
            ]

    narrowedParentOwners =
      Set.intersection parentOwners narrowestOwners

memberSpanSize :: GHC.SrcSpan -> Int
memberSpanSize = \case
  GHC.RealSrcSpan realSpan _ ->
    let lineSpan = GHC.srcSpanEndLine realSpan - GHC.srcSpanStartLine realSpan
        colSpan =
          if lineSpan == 0
            then GHC.srcSpanEndCol realSpan - GHC.srcSpanStartCol realSpan
            else GHC.srcSpanEndCol realSpan
     in lineSpan * 10_000 + colSpan
  GHC.UnhelpfulSpan {} ->
    maxBound

dedupeDefinitionMembersByNameSpan :: [DefinitionMember] -> [DefinitionMember]
dedupeDefinitionMembersByNameSpan =
  List.nubBy sameMember
  where
    sameMember left right =
      left.memberName == right.memberName
        && left.memberSpan == right.memberSpan
