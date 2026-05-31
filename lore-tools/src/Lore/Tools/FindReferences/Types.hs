module Lore.Tools.FindReferences.Types
  ( FindReferencesVerbosity (..),
  )
where

data FindReferencesVerbosity
  = Low
  | Medium
  | High
  deriving stock (Eq, Show)
