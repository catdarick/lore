{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.ImportNormalize
  ( applyImportOperations,
    normalizeImports,
  )
where

import Data.List (find, foldl', groupBy, partition, sortOn)
import Data.Text (Text)
import Lore.Internal.AutoRefactor.ImportDecl
  ( ImportId,
    ImportItem (..),
    ImportList (..),
    NormalizedImport (..),
    QualifiedImportStyle (..),
  )
import Lore.Internal.AutoRefactor.ImportOps (ImportOperation (..))

applyImportOperations :: [NormalizedImport] -> [ImportOperation] -> ([NormalizedImport], [String])
applyImportOperations imports operations =
  normalizeImports $
    foldl' applyOne (imports, []) operations
  where
    applyOne (currentImports, logs) operation =
      let (updatedImports, newLogs) = applyOperation currentImports operation
       in (updatedImports, logs <> newLogs)

applyOperation :: [NormalizedImport] -> ImportOperation -> ([NormalizedImport], [String])
applyOperation imports = \case
  AddUnqualifiedItem moduleName itemText ->
    ensureUnqualifiedItem moduleName itemText imports
  EnsureQualifiedImport moduleName qualifier ->
    ensureQualifiedImport moduleName qualifier imports
  RemoveImportItem importId itemText ->
    removeImportItem importId itemText imports
  RemoveWholeImport importId ->
    removeWholeImport importId imports

ensureUnqualifiedItem :: Text -> Text -> [NormalizedImport] -> ([NormalizedImport], [String])
ensureUnqualifiedItem moduleName itemText imports =
  case find isCompatible imports of
    Just normalizedImport ->
      case normalizedImport.normalizedImportList of
        OpenImport ->
          (imports, [])
        ExplicitImport items ->
          let updatedItems = appendUniqueImportItems items [ImportItem itemText Nothing]
           in if updatedItems == items
                then (imports, [])
                else
                  ( replaceImport normalizedImport normalizedImport {normalizedImportList = ExplicitImport updatedItems} imports,
                    ["Auto-refact: extended existing import list for " <> show moduleName]
                  )
        HidingImport {} ->
          insertNewImport (newExplicitImport moduleName itemText) imports
    Nothing ->
      insertNewImport (newExplicitImport moduleName itemText) imports
  where
    isCompatible normalizedImport =
      normalizedImport.normalizedImportModuleName == moduleName
        && normalizedImport.normalizedImportQualifiedStyle == ImportUnqualified
        && not (isHidingImport normalizedImport)

ensureQualifiedImport :: Text -> Text -> [NormalizedImport] -> ([NormalizedImport], [String])
ensureQualifiedImport moduleName qualifier imports =
  case find isCompatible imports of
    Just normalizedImport ->
      case normalizedImport.normalizedImportList of
        OpenImport ->
          (imports, [])
        ExplicitImport _ ->
          ( replaceImport normalizedImport normalizedImport {normalizedImportList = OpenImport} imports,
            ["Auto-refact: opened qualified import for " <> show moduleName]
          )
        HidingImport {} ->
          insertNewImport (newQualifiedImport moduleName qualifier) imports
    Nothing ->
      insertNewImport (newQualifiedImport moduleName qualifier) imports
  where
    isCompatible normalizedImport =
      normalizedImport.normalizedImportModuleName == moduleName
        && normalizedImport.normalizedImportQualifiedStyle /= ImportUnqualified
        && normalizedImport.normalizedImportAlias == Just qualifier
        && not (isHidingImport normalizedImport)

removeImportItem :: ImportId -> Text -> [NormalizedImport] -> ([NormalizedImport], [String])
removeImportItem importId itemText imports =
  case find ((== Just importId) . normalizedImportId) imports of
    Nothing ->
      (imports, [])
    Just normalizedImport ->
      case normalizedImport.normalizedImportList of
        ExplicitImport items ->
          let filteredItems =
                filter ((/= itemText) . importItemText) items
           in if filteredItems == items
                then (imports, [])
                else case filteredItems of
                  [] ->
                    ( filter ((/= Just importId) . normalizedImportId) imports,
                      [ "Auto-refact: removed redundant import "
                          <> show normalizedImport.normalizedImportModuleName
                      ]
                    )
                  _ ->
                    ( replaceImport normalizedImport normalizedImport {normalizedImportList = ExplicitImport filteredItems} imports,
                      [ "Auto-refact: removed redundant binding "
                          <> show itemText
                          <> " from "
                          <> show normalizedImport.normalizedImportModuleName
                      ]
                    )
        _ ->
          (imports, [])

removeWholeImport :: ImportId -> [NormalizedImport] -> ([NormalizedImport], [String])
removeWholeImport importId imports =
  case find ((== Just importId) . normalizedImportId) imports of
    Nothing ->
      (imports, [])
    Just normalizedImport ->
      ( filter ((/= Just importId) . normalizedImportId) imports,
        ["Auto-refact: removed redundant import " <> show normalizedImport.normalizedImportModuleName]
      )

normalizeImports :: ([NormalizedImport], [String]) -> ([NormalizedImport], [String])
normalizeImports (imports, logs) =
  let (hidingImports, mergeableImports) = partition isHidingImport imports
      (mergedImports, mergeLogs) = mergeImports mergeableImports
   in (sortOn normalizedImportOrder (mergedImports <> hidingImports), logs <> mergeLogs)

mergeImports :: [NormalizedImport] -> ([NormalizedImport], [String])
mergeImports imports =
  foldl' mergeGroup ([], []) groupedImports
  where
    groupedImports =
      groupBy sameMergeKey $
        sortOn mergeSortKey imports

    mergeSortKey normalizedImport =
      ( mergeKey normalizedImport,
        normalizedImport.normalizedImportOrder
      )

    sameMergeKey left right =
      mergeKey left == mergeKey right

mergeGroup :: ([NormalizedImport], [String]) -> [NormalizedImport] -> ([NormalizedImport], [String])
mergeGroup (accImports, accLogs) = \case
  [] ->
    (accImports, accLogs)
  [normalizedImport] ->
    (normalizedImport : accImports, accLogs)
  groupedImports ->
    let representative = minimumByOrder groupedImports
        mergedImport =
          case find ((== OpenImport) . normalizedImportList) groupedImports of
            Just openImport ->
              representative {normalizedImportList = openImport.normalizedImportList}
            Nothing ->
              representative
                { normalizedImportList =
                    ExplicitImport $
                      foldl'
                        appendUniqueImportItems
                        []
                        [ items
                        | ExplicitImport items <- map (.normalizedImportList) groupedImports
                        ]
                }
        mergeLog =
          "Auto-refact: merged duplicate imports for " <> show representative.normalizedImportModuleName
     in (mergedImport : accImports, mergeLog : accLogs)

mergeKey ::
  NormalizedImport ->
  ( Text,
    QualifiedImportStyle,
    Maybe Text,
    Bool,
    Bool,
    Maybe Text
  )
mergeKey normalizedImport =
  ( normalizedImport.normalizedImportModuleName,
    normalizedImport.normalizedImportQualifiedStyle,
    normalizedImport.normalizedImportAlias,
    normalizedImport.normalizedImportSource,
    normalizedImport.normalizedImportSafe,
    normalizedImport.normalizedImportPackageQualifier
  )

minimumByOrder :: [NormalizedImport] -> NormalizedImport
minimumByOrder =
  foldl1 pickEarlier
  where
    pickEarlier left right
      | left.normalizedImportOrder <= right.normalizedImportOrder = left
      | otherwise = right

replaceImport :: NormalizedImport -> NormalizedImport -> [NormalizedImport] -> [NormalizedImport]
replaceImport original updated =
  map \normalizedImport ->
    if normalizedImport.normalizedImportId == original.normalizedImportId
      then updated
      else normalizedImport

insertNewImport :: NormalizedImport -> [NormalizedImport] -> ([NormalizedImport], [String])
insertNewImport newImport imports =
  (imports <> [newImport], ["Auto-refact: inserted import " <> show newImport.normalizedImportModuleName])

newExplicitImport :: Text -> Text -> NormalizedImport
newExplicitImport moduleName itemText =
  NormalizedImport
    { normalizedImportId = Nothing,
      normalizedImportOrder = 1_000_000,
      normalizedImportSpan = Nothing,
      normalizedImportModuleName = moduleName,
      normalizedImportQualifiedStyle = ImportUnqualified,
      normalizedImportAlias = Nothing,
      normalizedImportSource = False,
      normalizedImportSafe = False,
      normalizedImportPackageQualifier = Nothing,
      normalizedImportList = ExplicitImport [ImportItem itemText Nothing]
    }

newQualifiedImport :: Text -> Text -> NormalizedImport
newQualifiedImport moduleName qualifier =
  NormalizedImport
    { normalizedImportId = Nothing,
      normalizedImportOrder = 1_000_000,
      normalizedImportSpan = Nothing,
      normalizedImportModuleName = moduleName,
      normalizedImportQualifiedStyle = ImportQualifiedPrefix,
      normalizedImportAlias = Just qualifier,
      normalizedImportSource = False,
      normalizedImportSafe = False,
      normalizedImportPackageQualifier = Nothing,
      normalizedImportList = OpenImport
    }

appendUniqueImportItems :: [ImportItem] -> [ImportItem] -> [ImportItem]
appendUniqueImportItems existing additions =
  foldl' appendItem existing additions
  where
    appendItem acc item
      | any ((== item.importItemText) . importItemText) acc = acc
      | otherwise = acc <> [item]

isHidingImport :: NormalizedImport -> Bool
isHidingImport normalizedImport =
  case normalizedImport.normalizedImportList of
    HidingImport {} -> True
    _ -> False
