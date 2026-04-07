module Lore
  ( SessionConfig (..),
    defaultSessionConfig,
    ParallelWorkersCount (..),
    LoreMonadT,
    MonadLore,
    runLore,
    LoadTargetsOptions (..),
    defaultLoadTargetsOptions,
    loadTargets,
    ExportedSymbol (..),
    SymbolInfo (..),
    Instances (..),
    findSymbols,
    lookupSymbolInfo,
    lookupRootSymbolInfo,
    resolveInstances,
    resolveInstanceDefinitions,
    resolveDefinitionSlice,
    resolveDefinitionClosure,
    mergeDefinitionSlices,
    renderImport,
    DefinitionSlice (..),
    DeclarationSpans (..),
    RequiredImport,
    Diagnostic (..),
    DiagnosticClass (..),
    DiagnosticSpan (..),
    Span (..),
    DiagnosticCodeInfo (..),
    LoggerHandle (..),
    LogLevel (..),
    LogMessage (..),
    prettyLoggerHandle,
    noLogHandle,
  )
where

import Lore.Definition
import Lore.Diagnostics
import Lore.Logger
import Lore.Lookup
import Lore.Monad (LoreMonadT, MonadLore)
import Lore.Session
import Lore.Targets
