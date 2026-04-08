module Lore
  ( SessionConfig (..),
    defaultSessionConfig,
    PreludeImportRule (..),
    ParallelWorkersCount (..),
    LoreMonadT,
    MonadLore,
    runLore,
    LoadTargetsResult (..),
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
    loadInterpreterContext,
    interpretExpression,
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
import Lore.Monad (LoreMonadT, MonadLore)
import Lore.Session
import Lore.Targets
