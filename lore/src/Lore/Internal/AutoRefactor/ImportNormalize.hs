{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.ImportNormalize
  ( applyImportOperations,
    removeImportItem,
    removeTargetFromImportItem,
    parseParentImportItem,
    normalizedFlatBindingText,
  )
where

import Data.List (find, foldl')
import Data.Text (Text)
import Lore.Internal.AutoRefactor.ImportDecl (ImportId, ImportList (..), NormalizedImport (..))
import Lore.Internal.AutoRefactor.ImportItemRemoval (normalizedFlatBindingText, parseParentImportItem, removeTargetFromImportItem)
import qualified Lore.Internal.AutoRefactor.ImportItemRemoval as ImportItemRemoval
import Lore.Internal.AutoRefactor.ImportOps (ImportOperation (..))

applyImportOperations :: [NormalizedImport] -> [ImportOperation] -> ([NormalizedImport], [String])
applyImportOperations imports =
  foldl' applyOne (imports, [])
  where
    applyOne (currentImports, logs) operation =
      let (updatedImports, newLogs) = applyOperation currentImports operation
       in (updatedImports, logs <> newLogs)

applyOperation :: [NormalizedImport] -> ImportOperation -> ([NormalizedImport], [String])
applyOperation imports = \case
  RemoveImportItem importId itemText ->
    removeImportItem importId itemText imports
  RemoveWholeImport importId ->
    removeWholeImport importId imports

removeImportItem :: ImportId -> Text -> [NormalizedImport] -> ([NormalizedImport], [String])
removeImportItem importId itemText imports =
  case find ((== Just importId) . normalizedImportId) imports of
    Nothing ->
      (imports, [])
    Just normalizedImport ->
      case normalizedImport.normalizedImportList of
        ExplicitImport items ->
          let updatedItems =
                foldr
                  (\item acc -> maybe acc (: acc) (ImportItemRemoval.removeTargetFromImportItem itemText item))
                  []
                  items
           in if updatedItems == items
                then (imports, [])
                else case updatedItems of
                  [] ->
                    ( filter ((/= Just importId) . normalizedImportId) imports,
                      [ "Auto-refact: removed redundant import "
                          <> show normalizedImport.normalizedImportModuleName
                      ]
                    )
                  _ ->
                    ( replaceImport normalizedImport normalizedImport {normalizedImportList = ExplicitImport updatedItems} imports,
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

replaceImport :: NormalizedImport -> NormalizedImport -> [NormalizedImport] -> [NormalizedImport]
replaceImport original updated =
  map \normalizedImport ->
    if normalizedImport.normalizedImportId == original.normalizedImportId
      then updated
      else normalizedImport
