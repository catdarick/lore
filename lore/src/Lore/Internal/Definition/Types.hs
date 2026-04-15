{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Lore.Internal.Definition.Types
  ( DefinitionSlice (..),
    DeclarationSpans (..),
    MinimalTypedImport (..),
    MinimalTypedOccurrence (..),
    MinimalTypedModuleFacts (..),
    ProcessedTypedDefinitionFacts (..),
    TypedModuleCache (..),
    MinimalCoreModuleFacts (..),
    ParsedModuleCache (..),
    ParsedModuleSummary (..),
    ParsedDefinitionMatch (..),
    ParsedOccurrenceSyntax (..),
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

data ParsedOccurrenceSyntax = ParsedOccurrenceSyntax
  { parsedSyntaxQualifier :: !(Maybe GHC.ModuleName),
    parsedSyntaxUsageSpans :: ![GHC.SrcSpan],
    parsedSyntaxSectionSpans :: ![GHC.SrcSpan]
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data ParsedDefinitionMatch = ParsedDefinitionMatch
  { parsedDefinitionSpans :: !DeclarationSpans,
    parsedOccurrenceSyntaxes :: ![(GHC.SrcSpan, ParsedOccurrenceSyntax)]
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data ParsedModuleSummary = ParsedModuleSummary
  { parsedModuleOccurrenceNames :: !(Set.Set Text),
    parsedModuleDefinitions :: ![ParsedDefinitionMatch]
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data ParsedModuleCache
  = ParsedModuleRaw !GHC.ParsedSource
  | ParsedModuleProcessed !ParsedModuleSummary
  deriving stock (Generic)

data ImportQualifiedStyle
  = QualifiedPre
  | QualifiedPost
  | NotQualified
  deriving stock (Eq, Show, Data, Generic)
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

data MinimalTypedImport = MinimalTypedImport
  { typedImportId :: !Int,
    typedImportModule :: !GHC.ModuleName,
    typedImportPackageQualifier :: !(Maybe String),
    typedImportSource :: !Bool,
    typedImportQualifiedStyle :: !ImportQualifiedStyle,
    typedImportAlias :: !(Maybe GHC.ModuleName),
    typedImportOriginallyExplicit :: !Bool
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data MinimalTypedOccurrence = MinimalTypedOccurrence
  { typedOccurrenceName :: !GHC.Name,
    typedOccurrenceSpan :: !GHC.SrcSpan,
    typedOccurrenceParent :: !(Maybe GHC.Name),
    typedOccurrenceCandidates :: ![Int]
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data MinimalTypedModuleFacts = MinimalTypedModuleFacts
  { typedDefinitionNames :: ![GHC.Name],
    typedSourceImports :: ![MinimalTypedImport],
    typedOccurrences :: ![MinimalTypedOccurrence]
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data ProcessedTypedDefinitionFacts = ProcessedTypedDefinitionFacts
  { processedRequiredImports :: ![RequiredImport],
    processedReferences :: ![GHC.Name],
    processedReferenceSpans :: !(Map.Map GHC.Name [GHC.SrcSpan]),
    processedReferenceUsageSpans :: !(Map.Map GHC.Name [GHC.SrcSpan]),
    processedReferenceSectionSpans :: !(Map.Map GHC.Name [GHC.SrcSpan])
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data TypedModuleCache
  = TypedModuleMinimalFacts !MinimalTypedModuleFacts
  | TypedModuleProcessedData !(Map.Map GHC.Name ProcessedTypedDefinitionFacts)
  deriving stock (Generic)
  deriving anyclass (NFData)

data MinimalCoreModuleFacts = MinimalCoreModuleFacts
  { coreUsedInstancesByBinder :: !(Map.Map GHC.Name [GHC.Name])
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data ReferenceMatch = ReferenceMatch
  { referenceSlice :: !DefinitionSlice,
    matchedReferenceSpans :: ![GHC.SrcSpan],
    matchedReferenceUsageSpans :: ![GHC.SrcSpan],
    matchedReferenceSectionSpans :: ![GHC.SrcSpan]
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DefinitionAnalysis = DefinitionAnalysis
  { analysisSlice :: !DefinitionSlice,
    analysisReferences :: ![GHC.Name],
    analysisUsedInstances :: ![GHC.Name],
    analysisReferenceSpans :: !(Map.Map GHC.Name [GHC.SrcSpan]),
    analysisReferenceUsageSpans :: !(Map.Map GHC.Name [GHC.SrcSpan]),
    analysisReferenceSectionSpans :: !(Map.Map GHC.Name [GHC.SrcSpan])
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
