{-# LANGUAGE DeriveAnyClass #-}

module Lore.Internal.Definition.Types
  ( SpanKey (..),
    OccKey (..),
    srcSpanKey,
    nameOccKey,
    occNameKey,
    rdrNameOccKey,
    DefinitionId (..),
    definitionIdFromSpans,
    dedupeExactNames,
    DefinitionSlice (..),
    NamedDefinitionSource (..),
    DeclarationSpans (..),
    MinimalTypedOccurrence (..),
    MinimalTypedModuleFacts (..),
    TypedNameFacts (..),
    TypedDefinitionFacts (..),
    TypedInstanceFacts (..),
    typedInstanceNames,
    MinimalCoreModuleFacts (..),
    ParsedModuleFacts (..),
    ParsedDefinitionMember (..),
    DefinitionMember (..),
    DefinitionMemberIndex (..),
    SourceRegionCandidate (..),
    DefinitionSourceTree (..),
    SourceRegion (..),
    SourceRegionKind (..),
    DefinitionSource (..),
    definitionSourceModule,
    DefinitionDependencies (..),
    DefinitionCatalog (..),
    ReferenceHit (..),
    ReferenceIndex (..),
    DefinitionOccurrenceFact (..),
    ReferenceMatch (..),
    ParsedOccurrenceModuleIndex (..),
    DefinitionModuleIndex (..),
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import Lore.Internal.Ghc.ValueTypeHead (ValueTypeHeadNames)

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

data DefinitionSlice = DefinitionSlice
  { definitionModule :: !GHC.Module,
    declarationSpans :: ![DeclarationSpans]
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

data ParsedDefinitionMember = ParsedDefinitionMember
  { parsedMemberOccKey :: !OccKey,
    parsedMemberSpan :: !GHC.SrcSpan
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DefinitionMember = DefinitionMember
  { memberName :: !GHC.Name,
    memberSpan :: !GHC.SrcSpan
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DefinitionMemberIndex = DefinitionMemberIndex
  { rootMemberNames :: !(Set.Set GHC.Name),
    scopedMembers :: ![DefinitionMember]
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data ParsedModuleFacts = ParsedModuleFacts
  { parsedOccKeys :: !(Set.Set OccKey),
    parsedDeclarationsById :: !(Map.Map DefinitionId DeclarationSpans),
    parsedDefinitionMembersById :: !(Map.Map DefinitionId [ParsedDefinitionMember]),
    parsedRegionCandidates :: ![SourceRegionCandidate]
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data MinimalTypedOccurrence = MinimalTypedOccurrence
  { typedOccurrenceName :: !GHC.Name,
    typedOccurrenceSpan :: !GHC.SrcSpan,
    typedOccurrenceParent :: !(Maybe GHC.Name)
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data TypedNameFacts = TypedNameFacts
  { typedDefinitionNames :: ![GHC.Name],
    typedDefinitionOccAliases :: !(Map.Map GHC.Name (Set.Set Text)),
    typedExportedNames :: ![GHC.Name],
    typedExportedOccAliases :: !(Map.Map GHC.Name (Set.Set Text))
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data TypedDefinitionFacts = TypedDefinitionFacts
  { -- | Exact typed occurrences used by definition reference and dependency
    -- producers.
    typedOccurrences :: ![MinimalTypedOccurrence],
    typedValueTypeHeadNamesByName :: !(Map.Map GHC.Name ValueTypeHeadNames)
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data TypedInstanceFacts = TypedInstanceFacts
  { -- | For each local instance binder, names of type constructors that appear
    -- in instance-head argument types.
    -- Dead-code policy: instance definitions are alive iff any of these head
    -- type definitions is alive.
    typedInstanceHeadTypeNamesByInstance :: !(Map.Map GHC.Name (Set.Set GHC.Name))
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

data MinimalTypedModuleFacts = MinimalTypedModuleFacts
  { typedNameFacts :: !TypedNameFacts,
    typedDefinitionFacts :: !TypedDefinitionFacts,
    typedInstanceFacts :: !TypedInstanceFacts
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

typedInstanceNames :: MinimalTypedModuleFacts -> Set.Set GHC.Name
typedInstanceNames =
  Map.keysSet . typedInstanceHeadTypeNamesByInstance . typedInstanceFacts

data MinimalCoreModuleFacts = MinimalCoreModuleFacts
  { -- Used by definition-closure/query dependency expansion.
    -- Keep this scoped to direct evidence (dfun) usage to preserve depth semantics.
    coreEvidenceDependenciesByBinder :: !(Map.Map GHC.Name [GHC.Name]),
    -- Used by dead-code reachability graph construction.
    -- May include transitive semantic dependencies across top-level binders.
    coreSemanticDependenciesByBinder :: !(Map.Map GHC.Name [GHC.Name])
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

data DefinitionSource = DefinitionSource
  { definitionSourceId :: !DefinitionId,
    definitionSourceNames :: !(Set.Set GHC.Name),
    definitionSourceSpans :: !DeclarationSpans
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

definitionSourceModule :: DefinitionSource -> GHC.Module
definitionSourceModule =
  definitionIdModule . definitionSourceId

data DefinitionDependencies = DefinitionDependencies
  { -- | Member-sensitive closure dependencies keyed by the queried binder,
    -- constructor, field, or method name.
    dependencyClosureNamesByReferenceName :: !(Map.Map GHC.Name (Set.Set GHC.Name)),
    -- | Definition-level reachability dependencies for project-wide dead-code
    -- analysis. This intentionally excludes direct evidence dependencies so
    -- graph reachability keeps the existing root-level semantics.
    dependencyReachabilityNames :: !(Set.Set GHC.Name)
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DefinitionCatalog = DefinitionCatalog
  { definitionSourcesById :: !(Map.Map DefinitionId DefinitionSource),
    definitionIdsByName :: !(Map.Map GHC.Name DefinitionId)
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

newtype ReferenceIndex = ReferenceIndex
  { referencesByName :: Map.Map GHC.Name (Map.Map DefinitionId (Map.Map SpanKey GHC.SrcSpan))
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DefinitionOccurrenceFact = DefinitionOccurrenceFact
  { occurrenceFactName :: !GHC.Name,
    occurrenceFactSpan :: !GHC.SrcSpan,
    occurrenceFactOwners :: !(Set.Set GHC.Name)
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
  { -- | Canonical source catalog for this module index.
    definitionCatalog :: !DefinitionCatalog,
    -- | Exact-name reference index for this typed module.
    referenceIndex :: !ReferenceIndex,
    -- | Definition dependencies keyed by ids from 'definitionCatalog'.
    dependenciesById :: !(Map.Map DefinitionId DefinitionDependencies),
    -- | Instance-head type dependencies keyed by local instance definition ids.
    instanceHeadTypeDefinitionIdsByInstance :: !(Map.Map DefinitionId (Set.Set DefinitionId))
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)
