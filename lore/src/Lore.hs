module Lore
  ( SessionConfig (..),
    defaultSessionConfig,
    ParallelWorkersCount (..),
    LoreMonadT,
    MonadLore,
    runLore,
    LoadTargetsResult (..),
    LoadTargetsOptions (..),
    defaultLoadTargetsOptions,
    loadTargets,
    lookupLastLoadTargetsResult,
    NormalizedOccName,
    NormalizedModuleName,
    NormalizedName (occName, moduleName),
    parseAndNormalizeName,
    normalizeModuleName,
    mkNormalizedModuleName,
    Symbol (..),
    SymbolVisibility (..),
    ExportedSymbolNode (..),
    SymbolCategory (..),
    classifySymbolCategory,
    SymbolInfo (..),
    Instances (..),
    PathToRoot (..),
    findMatchingSymbols,
    findMatchingSymbolsRoots,
    resolveModule,
    listSymbolsExportedByModule,
    filterExportedSymbolNodesByTypeHint,
    lookupSymbolInfo,
    listIntersectingInstances,
    listAssociatedInstances,
    resolvePathToRoot,
    mergePathsToRootOn,
    -- Source-first definition API.
    resolveDefinitionSourceNamed,
    resolveDefinitionClosureSourcesNamed,
    resolveReferenceMatchesForNames,
    getMinifiedImportsForDefinition,
    mergeDefinitionSlices,
    DefinitionId (..),
    DefinitionSource (..),
    -- Rendering DTO used by existing renderers.
    DefinitionSlice (..),
    ReferenceHit (..),
    NamedDefinitionSource (..),
    DeclarationSpans (..),
    ReferenceMatch (..),
    RequiredImport (..),
    ImportQualifiedStyle (..),
    RequiredImportItem (..),
    Diagnostic (..),
    DiagnosticClass (..),
    DiagnosticSpan (..),
    Span (..),
    DiagnosticCodeInfo (..),
    interpreterContextIsReady,
    loadInterpreterContext,
    executeStatement,
    getTypeOfExpression,
    LoggerHandle (..),
    LogLevel (..),
    LogMessage (..),
    prettyLoggerHandle,
    noLogHandle,
  )
where

import Lore.Definition
import Lore.Diagnostics
import Lore.Interpreter
import Lore.Logger
import Lore.Lookup
import Lore.Module
import Lore.Monad (LoreMonadT, MonadLore)
import Lore.Session
import Lore.Targets
