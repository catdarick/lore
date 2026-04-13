{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Lore.Internal.Definition.Types
  ( DefinitionSlice (..),
    DeclarationSpans (..),
    ImportQualifiedStyle (..),
    RequiredImport (..),
    RequiredImportItem (..),
    ReferenceMatch (..),
    DefinitionAnalysis (..),
    ReferenceOccurrenceIndex (..),
    ReferenceModuleAnalysis (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHC
import GHC.Generics (Generic)

data DefinitionSlice = DefinitionSlice
  { definitionModule :: !GHC.Module,
    declarationSpans :: ![DeclarationSpans],
    requiredImports :: ![RequiredImport]
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DeclarationSpans = DeclarationSpans
  { declarationSpan :: !GHC.SrcSpan,
    signatureSpan :: !(Maybe GHC.SrcSpan)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ImportQualifiedStyle
  = QualifiedPre
  | QualifiedPost
  | NotQualified
  deriving stock (Eq, Data, Generic)
  deriving anyclass (NFData)

data RequiredImport = RequiredImport
  { importKey :: !Int,
    importModule :: !GHC.ModuleName,
    importPackageQualifier :: !(Maybe String),
    importSource :: !Bool,
    importQualifiedStyle :: !ImportQualifiedStyle,
    importAlias :: !(Maybe GHC.ModuleName),
    importOriginallyExplicit :: !Bool,
    importItems :: ![RequiredImportItem]
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data RequiredImportItem
  = ImportName GHC.Name
  | ImportParent GHC.Name [GHC.Name]
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data ReferenceMatch = ReferenceMatch
  { referenceSlice :: !DefinitionSlice,
    matchedReferenceSpans :: ![GHC.SrcSpan]
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DefinitionAnalysis = DefinitionAnalysis
  { analysisSlice :: !DefinitionSlice,
    analysisReferences :: ![GHC.Name],
    analysisUsedInstances :: ![GHC.Name],
    analysisReferenceSpans :: !(Map.Map GHC.Name [GHC.SrcSpan])
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

newtype ReferenceOccurrenceIndex = ReferenceOccurrenceIndex
  { referenceOccurrenceModules :: Map.Map Text (Set.Set GHC.Module)
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

newtype ReferenceModuleAnalysis = ReferenceModuleAnalysis
  { referenceModuleDefinitions :: Map.Map GHC.Name (Maybe DefinitionAnalysis)
  }
  deriving stock (Generic)
  deriving anyclass (NFData)
