module Lore.Internal.Definition.Types
  ( DefinitionSlice (..),
    DeclarationSpans (..),
    RequiredImport (..),
    RequiredImportItem (..),
    DefinitionAnalysis (..),
    ReferenceModuleSearch (..),
    ReferenceModuleAnalysis (..),
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHC

data DefinitionSlice = DefinitionSlice
  { definitionModule :: GHC.Module,
    declarationSpans :: [DeclarationSpans],
    requiredImports :: [RequiredImport]
  }
  deriving stock (Eq)

data DeclarationSpans = DeclarationSpans
  { declarationSpan :: GHC.SrcSpan,
    signatureSpan :: Maybe GHC.SrcSpan
  }
  deriving stock (Eq, Show)

data RequiredImport = RequiredImport
  { importKey :: Int,
    importModule :: GHC.ModuleName,
    importPackageQualifier :: Maybe String,
    importSource :: Bool,
    importQualifiedStyle :: GHC.ImportDeclQualifiedStyle,
    importAlias :: Maybe GHC.ModuleName,
    importOriginallyExplicit :: Bool,
    importItems :: [RequiredImportItem]
  }
  deriving stock (Eq)

data RequiredImportItem
  = ImportName GHC.Name
  | ImportParent GHC.Name [GHC.Name]
  deriving stock (Eq)

data DefinitionAnalysis = DefinitionAnalysis
  { analysisSlice :: DefinitionSlice,
    analysisReferences :: [GHC.Name],
    analysisUsedInstances :: [GHC.Name]
  }

newtype ReferenceModuleSearch = ReferenceModuleSearch
  { referenceModuleOccurrenceNames :: Set.Set Text
  }

newtype ReferenceModuleAnalysis = ReferenceModuleAnalysis
  { referenceModuleDefinitions :: Map.Map GHC.Name (Maybe DefinitionAnalysis)
  }
