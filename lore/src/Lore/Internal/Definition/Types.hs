{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Lore.Internal.Definition.Types
  ( ImportId (..),
    SpanKey (..),
    OccKey (..),
    srcSpanKey,
    realSrcSpanKey,
    nameOccKey,
    occNameKey,
    rdrNameOccKey,
    DefinitionId (..),
    definitionIdFromSpans,
    dedupeExactNames,
    dedupeNamesByOccName,
    DefinitionSlice (..),
    NamedDefinitionSource (..),
    DeclarationSpans (..),
    MinimalTypedImport (..),
    MinimalTypedOccurrence (..),
    MinimalTypedModuleFacts (..),
    TypedModuleCache (..),
    MinimalCoreModuleFacts (..),
    ParsedModuleCache (..),
    ParsedModuleFacts (..),
    ParsedOccurrenceSyntax (..),
    SourceRegionCandidate (..),
    DefinitionSourceTree (..),
    SourceRegion (..),
    SourceRegionKind (..),
    ImportQualifiedStyle (..),
    RequiredImport (..),
    RequiredImportItem (..),
    ImportCandidate (..),
    DefinitionSource (..),
    DefinitionDependencies (..),
    DefinitionBindings (..),
    ReferenceHit (..),
    DefinitionOccurrenceFact (..),
    ReferenceMatch (..),
    ParsedOccurrenceModuleIndex (..),
    DefinitionModuleIndex (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC

newtype ImportId = ImportId
  { unImportId :: Int
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

newtype SpanKey = SpanKey
  { unSpanKey :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

newtype OccKey = OccKey
  { unOccKey :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- SpanKey is a session-local lookup key, not a durable persisted identity.
-- Prefer structural fields if this ever needs to cross process boundaries.
srcSpanKey :: GHC.SrcSpan -> SpanKey
srcSpanKey =
  SpanKey . T.pack . show

realSrcSpanKey :: GHC.RealSrcSpan -> SpanKey
realSrcSpanKey =
  SpanKey . T.pack . show

nameOccKey :: GHC.Name -> OccKey
nameOccKey =
  occNameKey . GHC.nameOccName

occNameKey :: GHC.OccName -> OccKey
occNameKey =
  OccKey . T.pack . GHC.occNameString

rdrNameOccKey :: GHC.RdrName -> OccKey
rdrNameOccKey =
  occNameKey . GHC.rdrNameOcc

data DefinitionId = DefinitionId
  { definitionIdModule :: !GHC.Module,
    definitionIdSpanKey :: !SpanKey
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

definitionIdFromSpans :: GHC.Module -> DeclarationSpans -> DefinitionId
definitionIdFromSpans module_ spans =
  -- DefinitionId identifies the source declaration, not an individual binder.
  -- Multiple names bound by the same declaration share one DefinitionId.
  DefinitionId
    { definitionIdModule = module_,
      definitionIdSpanKey = srcSpanKey spans.declarationSpan
    }

dedupeExactNames :: [GHC.Name] -> [GHC.Name]
dedupeExactNames =
  go Set.empty
  where
    go _ [] = []
    go seen (name : names)
      | Set.member name seen = go seen names
      | otherwise = name : go (Set.insert name seen) names

dedupeNamesByOccName :: [GHC.Name] -> [GHC.Name]
dedupeNamesByOccName =
  Map.elems . Map.fromList . map (\name -> (nameOccKey name, name))

data DefinitionSlice = DefinitionSlice
  { definitionModule :: !GHC.Module,
    declarationSpans :: ![DeclarationSpans],
    requiredImports :: ![RequiredImport]
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data NamedDefinitionSource = NamedDefinitionSource
  { definitionName :: !GHC.Name,
    definitionSource :: !DefinitionSource
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
  { parsedSyntaxQualifier :: !(Maybe GHC.ModuleName)
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data ParsedModuleFacts = ParsedModuleFacts
  { parsedOccKeys :: !(Set.Set OccKey),
    parsedDeclarationsById :: !(Map.Map DefinitionId DeclarationSpans),
    parsedOccurrenceSyntaxBySpan :: !(Map.Map SpanKey ParsedOccurrenceSyntax),
    parsedRegionCandidates :: ![SourceRegionCandidate]
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

newtype ParsedModuleCache = ParsedModuleFactsCache
  { unParsedModuleFactsCache :: ParsedModuleFacts
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

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
  { typedImportId :: !ImportId,
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
    typedOccurrenceCandidates :: ![ImportId]
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

newtype TypedModuleCache
  = TypedModuleMinimalFacts MinimalTypedModuleFacts
  deriving stock (Generic)
  deriving anyclass (NFData)

data MinimalCoreModuleFacts = MinimalCoreModuleFacts
  { coreUsedInstancesByBinder :: !(Map.Map GHC.Name [GHC.Name])
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data DefinitionSourceTree = DefinitionSourceTree
  { sourceTreeDefinition :: !DefinitionSource,
    sourceTreeRoot :: !SourceRegion
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data SourceRegion = SourceRegion
  { sourceRegionKind :: !SourceRegionKind,
    sourceRegionSpan :: !GHC.SrcSpan,
    sourceRegionChildren :: ![SourceRegion]
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data SourceRegionKind
  = DefinitionRegion
  | MatchRegion
  | GuardRegion
  | StatementRegion
  | BindingRegion
  | ApplicationRegion
  | RecordRegion
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data SourceRegionCandidate = SourceRegionCandidate
  { candidateRegionKind :: !SourceRegionKind,
    candidateRegionSpan :: !GHC.SrcSpan
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data ImportCandidate = ImportCandidate
  { importCandidateId :: !ImportId,
    importCandidateBaseImport :: !RequiredImport
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DefinitionSource = DefinitionSource
  { definitionSourceId :: !DefinitionId,
    definitionSourceModule :: !GHC.Module,
    definitionSourceNames :: !(Set.Set GHC.Name),
    definitionSourceSpans :: !DeclarationSpans
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DefinitionDependencies = DefinitionDependencies
  { dependencyDirectReferenceNames :: !(Set.Set GHC.Name),
    dependencyUsedInstanceNames :: !(Set.Set GHC.Name)
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DefinitionBindings = DefinitionBindings
  { bindingDefinitionsById :: !(Map.Map DefinitionId DefinitionSource),
    bindingDefinitionIdByName :: !(Map.Map GHC.Name DefinitionId)
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data ReferenceHit = ReferenceHit
  { referenceHitDefinitionId :: !DefinitionId,
    referenceHitTargetName :: !GHC.Name,
    referenceHitExactSpan :: !GHC.SrcSpan
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DefinitionOccurrenceFact = DefinitionOccurrenceFact
  { occurrenceFactName :: !GHC.Name,
    occurrenceFactSpan :: !GHC.SrcSpan,
    occurrenceFactParent :: !(Maybe GHC.Name),
    occurrenceFactImportCandidates :: ![ImportId]
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data ReferenceMatch = ReferenceMatch
  { referenceMatchDefinition :: !DefinitionSource,
    referenceMatchOccurrences :: ![ReferenceHit]
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

newtype ParsedOccurrenceModuleIndex = ParsedOccurrenceModuleIndex
  { parsedOccurrenceModules :: Map.Map OccKey (Set.Set GHC.Module)
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data DefinitionModuleIndex = DefinitionModuleIndex
  { definitionsById :: !(Map.Map DefinitionId DefinitionSource),
    definitionIdByName :: !(Map.Map GHC.Name DefinitionId),
    referenceHitsByOccKey :: !(Map.Map OccKey [ReferenceHit]),
    dependenciesById :: !(Map.Map DefinitionId DefinitionDependencies),
    requiredImportsById :: Map.Map DefinitionId [RequiredImport]
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)
