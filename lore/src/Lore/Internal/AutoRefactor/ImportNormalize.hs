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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.AutoRefactor.ImportDecl (ImportId, ImportList (..), NormalizedImport (..))
import Lore.Internal.AutoRefactor.ImportItemRemoval (applyRemovalTargets, normalizedFlatBindingText, parseParentImportItem, removeTargetFromImportItem)
import Lore.Internal.AutoRefactor.ImportOps (ImportOperation (..), ImportRemovalTarget (..), mkFlatRemovalTarget, unNormalizedImportItem)

applyImportOperations :: [NormalizedImport] -> [ImportOperation] -> ([NormalizedImport], [String])
applyImportOperations imports =
  foldl' applyOne (imports, [])
  where
    applyOne (currentImports, logs) operation =
      let (updatedImports, newLogs) = applyOperation currentImports operation
       in (updatedImports, logs <> newLogs)

applyOperation :: [NormalizedImport] -> ImportOperation -> ([NormalizedImport], [String])
applyOperation imports = \case
  RemoveImportItems importId targets ->
    removeImportItemTargets importId targets imports
  RemoveWholeImport importId ->
    removeWholeImport importId imports

removeImportItem :: ImportId -> Text -> [NormalizedImport] -> ([NormalizedImport], [String])
removeImportItem importId itemText =
  removeImportItemTargets
    importId
    (mkFlatRemovalTarget itemText :| [])

removeImportItemTargets :: ImportId -> NonEmpty ImportRemovalTarget -> [NormalizedImport] -> ([NormalizedImport], [String])
removeImportItemTargets importId targets imports =
  case find ((== Just importId) . normalizedImportId) imports of
    Nothing ->
      (imports, [])
    Just normalizedImport ->
      case normalizedImport.normalizedImportList of
        ExplicitImport items ->
          let updatedItems =
                foldr
                  (\item acc -> maybe acc (: acc) (applyRemovalTargets targets item))
                  []
                  items
           in if updatedItems == items
                then (imports, [])
                else case updatedItems of
                  [] ->
                    ( filter ((/= Just importId) . normalizedImportId) imports,
                      [ "Auto-refactor: removed redundant import "
                          <> show normalizedImport.normalizedImportModuleName
                      ]
                    )
                  _ ->
                    let targetTextPreview =
                          show (map renderRemovalTarget (NE.toList targets))
                     in ( replaceImport normalizedImport normalizedImport {normalizedImportList = ExplicitImport updatedItems} imports,
                          [ "Auto-refactor: removed redundant bindings "
                              <> targetTextPreview
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
        ["Auto-refactor: removed redundant import " <> show normalizedImport.normalizedImportModuleName]
      )

replaceImport :: NormalizedImport -> NormalizedImport -> [NormalizedImport] -> [NormalizedImport]
replaceImport original updated =
  map \normalizedImport ->
    if normalizedImport.normalizedImportId == original.normalizedImportId
      then updated
      else normalizedImport

renderRemovalTarget :: ImportRemovalTarget -> Text
renderRemovalTarget = \case
  RemoveFlatBinding binding ->
    unNormalizedImportItem binding
  RemoveWholeImportItem item ->
    unNormalizedImportItem item
  RemoveParentChild parent binding ->
    T.concat [unNormalizedImportItem parent, "(", unNormalizedImportItem binding, ")"]
