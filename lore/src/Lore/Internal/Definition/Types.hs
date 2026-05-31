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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC

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

data MinimalTypedModuleFacts = MinimalTypedModuleFacts
  { typedDefinitionNames :: ![GHC.Name],
    typedInstanceNames :: ![GHC.Name],
    -- | For each local instance binder, names of type constructors that appear
    -- in instance-head argument types.
    -- Dead-code policy: instance definitions are alive iff any of these head
    -- type definitions is alive.
    typedInstanceHeadTypeNamesByInstance :: !(Map.Map GHC.Name (Set.Set GHC.Name)),
    typedDefinitionOccAliases :: !(Map.Map GHC.Name (Set.Set Text)),
    typedExportedNames :: ![GHC.Name],
    typedExportedOccAliases :: !(Map.Map GHC.Name (Set.Set Text)),
    typedOccurrences :: ![MinimalTypedOccurrence]
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

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
    definitionSourceModule :: !GHC.Module,
    definitionSourceNames :: !(Set.Set GHC.Name),
    definitionSourceSpans :: !DeclarationSpans
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data DefinitionDependencies = DefinitionDependencies
  { -- | Compatibility aggregate derived from scoped map values.
    -- Query-time recursion should use scoped maps directly.
    dependencyDirectReferenceNames :: !(Set.Set GHC.Name),
    -- | Compatibility aggregate derived from scoped map values.
    -- Includes directly used instance dictionaries needed by definition-closure
    -- expansion.
    -- Query-time recursion should use scoped maps directly.
    dependencyUsedInstanceNames :: !(Set.Set GHC.Name),
    -- | Core-derived reachability dependencies for project-wide dead-code
    -- analysis. These may be transitive across local top-level binders and are
    -- intentionally separate from the direct evidence dependencies used by
    -- definition-closure queries.
    dependencyCoreSemanticNames :: ![GHC.Name],
    dependencyDirectReferenceNamesByReferenceName :: !(Map.Map GHC.Name (Set.Set GHC.Name)),
    dependencyUsedInstanceNamesByReferenceName :: !(Map.Map GHC.Name (Set.Set GHC.Name))
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
    occurrenceFactOwners :: !(Set.Set GHC.Name),
    occurrenceFactParent :: !(Maybe GHC.Name)
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
  { -- | Canonical source set for this module index.
    definitionsById :: !(Map.Map DefinitionId DefinitionSource),
    -- | Maps every known top-level definition name to an id in 'definitionsById'.
    definitionIdByName :: !(Map.Map GHC.Name DefinitionId),
    -- | Candidate reference index grouped by occurrence key.
    -- Exact 'GHC.Name' filtering is still required at query time.
    referenceHitsByOccKey :: !(Map.Map OccKey [ReferenceHit]),
    -- | Definition dependencies keyed by ids from 'definitionsById'.
    dependenciesById :: !(Map.Map DefinitionId DefinitionDependencies)
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)
