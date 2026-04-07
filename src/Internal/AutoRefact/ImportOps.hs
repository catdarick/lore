{-# LANGUAGE OverloadedStrings #-}

module Internal.AutoRefact.ImportOps
  ( ImportOperation (..),
  )
where

import Data.Text (Text)
import Internal.AutoRefact.ImportDecl (ImportId)

data ImportOperation
  = AddUnqualifiedItem Text Text
  | EnsureQualifiedImport Text Text
  | RemoveImportItem ImportId Text
  | RemoveWholeImport ImportId
  deriving (Eq, Show)
