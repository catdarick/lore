{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.ImportOps
  ( ImportOperation (..),
  )
where

import Data.Text (Text)
import Lore.Internal.AutoRefactor.ImportDecl (ImportId)

data ImportOperation
  = RemoveImportItem ImportId Text
  | RemoveWholeImport ImportId
  deriving (Eq, Show)
