{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.ImportOps
  ( ImportOperation (..),
  )
where

import Data.Text (Text)
import Lore.Internal.AutoRefactor.ImportDecl (ImportId)

data ImportOperation
  = AddUnqualifiedItem Text Text
  | AddUnqualifiedItemToExistingImport Text Text
  | EnsureQualifiedImport Text Text
  | RemoveImportItem ImportId Text
  | RemoveWholeImport ImportId
  deriving (Eq, Show)
