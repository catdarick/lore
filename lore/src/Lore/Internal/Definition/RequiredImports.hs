module Lore.Internal.Definition.RequiredImports
  ( minimalImportToImportCandidate,
    buildImportCandidates,
    indexImportCandidates,
    buildRequiredImportsById,
    buildMinifiedImports,
    normalizeImportItems,
    dedupeImportItemNamesByRenderedOcc,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Types

minimalImportToImportCandidate :: MinimalTypedImport -> ImportCandidate
minimalImportToImportCandidate minimalImport =
  ImportCandidate
    { importCandidateId = minimalImport.typedImportId,
      importCandidateBaseImport =
        RequiredImport
          { importKey = unImportId minimalImport.typedImportId,
            importModule = minimalImport.typedImportModule,
            importPackageQualifier = minimalImport.typedImportPackageQualifier,
            importSource = minimalImport.typedImportSource,
            importQualifiedStyle = minimalImport.typedImportQualifiedStyle,
            importAlias = minimalImport.typedImportAlias,
            importOriginallyExplicit = minimalImport.typedImportOriginallyExplicit,
            importItems = []
          }
    }

buildImportCandidates :: [MinimalTypedImport] -> [ImportCandidate]
buildImportCandidates =
  map minimalImportToImportCandidate

indexImportCandidates :: [ImportCandidate] -> Map.Map ImportId ImportCandidate
indexImportCandidates importCandidates =
  Map.fromList
    [ (importCandidate.importCandidateId, importCandidate)
    | importCandidate <- importCandidates
    ]

buildRequiredImportsById ::
  [ImportCandidate] ->
  Map.Map DefinitionId [DefinitionOccurrenceFact] ->
  Map.Map DefinitionId [RequiredImport]
buildRequiredImportsById importCandidates occurrencesById =
  Map.map (buildMinifiedImports importCandidates) occurrencesById

normalizeImportItems :: [RequiredImportItem] -> [RequiredImportItem]
normalizeImportItems items =
  standaloneItems <> parentItems
  where
    standaloneNames =
      dedupeImportItemNamesByRenderedOcc
        [ name
        | ImportName name <- items
        ]

    childNamesByRenderedParent =
      List.foldl'
        insertParentChildren
        Map.empty
        [ (parentName, childNames)
        | ImportParent parentName childNames <- items
        ]

    standaloneItems =
      map ImportName $
        filter ((`Map.notMember` childNamesByRenderedParent) . renderName) standaloneNames

    parentItems =
      [ ImportParent parentName (dedupeImportItemNamesByRenderedOcc childNames)
      | (_, (parentName, childNames)) <- List.sortOn fst (Map.toList childNamesByRenderedParent)
      ]

    renderName =
      GHC.occNameString . GHC.nameOccName

    insertParentChildren parentsByRenderedName (parentName, childNames) =
      Map.insertWith
        mergeParentChildren
        (renderName parentName)
        (parentName, childNames)
        parentsByRenderedName

    mergeParentChildren (_newParentName, newChildNames) (oldParentName, oldChildNames) =
      (oldParentName, oldChildNames <> newChildNames)

buildMinifiedImports ::
  [ImportCandidate] ->
  [DefinitionOccurrenceFact] ->
  [RequiredImport]
buildMinifiedImports importCandidates occurrences =
  mapMaybe buildRequiredImport $
    Map.toAscList assignedOccurrences
  where
    chosenImports =
      chooseMinimalImports importedOccurrences

    importedOccurrences =
      filter (not . null . occurrenceFactImportCandidates) occurrences

    assignedOccurrences =
      Map.fromListWith
        (<>)
        [ (candidateId, [ref])
        | ref <- importedOccurrences,
          Just candidateId <- [List.find (`Set.member` chosenImports) ref.occurrenceFactImportCandidates]
        ]

    chooseMinimalImports =
      go Set.empty
      where
        go chosen [] = chosen
        go chosen remaining =
          let counts =
                Map.fromListWith
                  (+)
                  [(candidateId, 1 :: Int) | ref <- remaining, candidateId <- ref.occurrenceFactImportCandidates]
              bestImport =
                fst $
                  List.minimumBy compareImportCandidate $
                    Map.toList counts
              chosen' = Set.insert bestImport chosen
           in go chosen' (filter (not . coveredBy chosen') remaining)

        compareImportCandidate (leftId, leftCount) (rightId, rightCount) =
          compare rightCount leftCount
            <> compare leftId rightId

        coveredBy chosen ref =
          any (`Set.member` chosen) ref.occurrenceFactImportCandidates

    buildRequiredImport (importId, refs) = do
      importCandidate <- List.find ((== importId) . importCandidateId) importCandidates
      pure
        importCandidate.importCandidateBaseImport
          { importItems = normalizeImportItems (concatMap occurrenceItems refs)
          }

    occurrenceItems DefinitionOccurrenceFact {occurrenceFactName, occurrenceFactParent} =
      case occurrenceFactParent of
        Just parentName
          | parentName /= occurrenceFactName ->
              [ImportParent parentName [occurrenceFactName]]
        _ ->
          [ImportName occurrenceFactName]

dedupeImportItemNamesByRenderedOcc :: [GHC.Name] -> [GHC.Name]
dedupeImportItemNamesByRenderedOcc =
  reverse . snd . foldl go (Set.empty, [])
  where
    go (seen, names) name
      | renderedOccName `Set.member` seen =
          (seen, names)
      | otherwise =
          (Set.insert renderedOccName seen, name : names)
      where
        renderedOccName =
          GHC.occNameString (GHC.nameOccName name)
