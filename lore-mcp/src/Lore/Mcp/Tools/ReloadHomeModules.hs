module Lore.Mcp.Tools.ReloadHomeModules where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.List (foldl', nub)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Lore
  ( Diagnostic (..),
    DiagnosticSpan (..),
    LoadHomeModulesOptions (..),
    LoadHomeModulesResult (..),
    MonadLore,
    Span (..),
    loadHomeModules,
  )
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.LoreDoc (LoreDoc, bulletList, heading2, heading3, paragraph)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared (Paginated (..), paginateItems)
import Lore.Mcp.Tools.Shared.Diagnostics (diagnosticMessageBody, diagnosticSeverityTitle)

data ReloadHomeModulesArgs (fieldType :: FieldType) = ReloadHomeModulesArgs
  { skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial diagnostics to skip. Use it only if you need more context to fix the initial errors.",
                      Example 5
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (ReloadHomeModulesArgs 'ValueType)

instance ToSchema (ReloadHomeModulesArgs 'MetadataType)

reloadHomeModulesTool :: (MonadLore m) => SomeTool m
reloadHomeModulesTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "reloadHomeModules",
        description = Just "Reloads all home modules, checks for errors, and applies safe auto-fixes when possible. This reload resets interpreter state (interactive bindings are cleared). Run this before tools that need up-to-date module information.",
        handler = reloadHomeModulesHandler
      }

reloadHomeModulesHandler :: (MonadLore m) => ReloadHomeModulesArgs 'ValueType -> m LoreDoc
reloadHomeModulesHandler ReloadHomeModulesArgs {skip} = do
  loadResult <- loadHomeModules LoadHomeModulesOptions {enableAutoRefactor = True}
  renderReloadHomeModulesResultWithSkip (max 0 (maybe 0 id skip)) loadResult

renderReloadHomeModulesResult :: (MonadLore m) => LoadHomeModulesResult -> m LoreDoc
renderReloadHomeModulesResult =
  renderReloadHomeModulesResultWithSkip 0

renderReloadHomeModulesResultWithSkip :: (MonadLore m) => Int -> LoadHomeModulesResult -> m LoreDoc
renderReloadHomeModulesResultWithSkip skip loadResult@LoadHomeModulesResult {loadHomeModulesDiagnostics} =
  case paginatedDiagnostics of
    [] ->
      pure (paragraph statusLine <> autoFixedSummaryDoc loadResult)
    _ -> do
      let visibleGroups = groupDiagnostics paginatedDiagnostics
      diagnosticsDoc <- mconcat <$> mapM diagnosticGroupDoc visibleGroups
      pure $
        paragraph statusLine
          <> autoFixedSummaryDoc loadResult
          <> diagnosticsDoc
          <> nextPageHintDoc loadHomeModulesDiagnostics diagnosticPage
  where
    diagnosticPage = paginateDiagnostics skip loadHomeModulesDiagnostics
    paginatedDiagnostics = maybe [] (.paginatedItems) diagnosticPage
    statusLine
      | loadResult.loadHomeModulesFailed > 0 =
          "Failed to load "
            <> T.pack (show loadResult.loadHomeModulesFailed)
            <> " of "
            <> T.pack (show loadResult.loadHomeModulesTotal)
            <> " modules."
      | loadResult.loadHomeModulesAutofixed > 0 =
          "Successfully loaded all "
            <> T.pack (show loadResult.loadHomeModulesTotal)
            <> " modules after auto-fixing "
            <> T.pack (show loadResult.loadHomeModulesAutofixed)
            <> ". No errors left."
      | otherwise =
          "Successfully loaded all "
            <> T.pack (show loadResult.loadHomeModulesTotal)
            <> " modules. No errors found."

autoFixedSummaryDoc :: LoadHomeModulesResult -> LoreDoc
autoFixedSummaryDoc loadResult
  | null loadResult.loadHomeModulesAutofixSummaryByFile =
      mempty
  | otherwise =
      heading2 "Safe fixes applied"
        <> bulletList (map renderAutofixedFileDoc loadResult.loadHomeModulesAutofixSummaryByFile)

renderAutofixedFileDoc :: (FilePath, [String]) -> LoreDoc
renderAutofixedFileDoc (filePath, summaries) =
  paragraph $
    T.pack filePath
      <> ": "
      <> T.intercalate "; " (map T.pack (nub summaries))

nextPageHintDoc :: [Diagnostic] -> Maybe (Paginated Diagnostic) -> LoreDoc
nextPageHintDoc _ Nothing =
  mempty
nextPageHintDoc allDiagnostics (Just page)
  | remainingDiagnostics <= 0 =
      mempty
  | otherwise =
      paragraph $
        "... and "
          <> T.pack (show remainingDiagnostics)
          <> " more diagnostics in "
          <> T.pack (show remainingModuleCount)
          <> " modules.\nIf you don't have enough context to fix the listed errors, set skip to "
          <> T.pack (show nextSkip)
          <> " to get the next page."
  where
    nextSkip = page.paginatedSkippedItems + page.paginatedConsumedItems
    remainingDiagnostics =
      max 0 (page.paginatedTotalItems - nextSkip)
    remainingModuleCount =
      length (groupDiagnostics (drop nextSkip allDiagnostics))

paginateDiagnostics :: Int -> [Diagnostic] -> Maybe (Paginated Diagnostic)
paginateDiagnostics skip diagnostics =
  paginateItems skip maxRenderedDiagnostics diagnostics
  where
    maxRenderedDiagnostics = 5

data DiagnosticGroupKey
  = DiagnosticFileGroup FilePath
  | DiagnosticOtherGroup Text
  deriving (Eq, Show)

type DiagnosticGroup = (DiagnosticGroupKey, [Diagnostic])

groupDiagnostics :: [Diagnostic] -> [DiagnosticGroup]
groupDiagnostics =
  foldl' insertDiagnosticGroup []
  where
    insertDiagnosticGroup [] diagnostic =
      [(diagnosticGroupKey diagnostic, [diagnostic])]
    insertDiagnosticGroup ((groupKey, groupedDiagnostics) : rest) diagnostic
      | groupKey == diagnosticKey =
          (groupKey, groupedDiagnostics <> [diagnostic]) : rest
      | otherwise =
          (groupKey, groupedDiagnostics) : insertDiagnosticGroup rest diagnostic
      where
        diagnosticKey = diagnosticGroupKey diagnostic

diagnosticGroupKey :: Diagnostic -> DiagnosticGroupKey
diagnosticGroupKey Diagnostic {diagnosticSpan} =
  case diagnosticSpan of
    RealDiagnosticSpan Span {spanFile} ->
      DiagnosticFileGroup spanFile
    UnhelpfulDiagnosticSpan spanText ->
      DiagnosticOtherGroup spanText

diagnosticGroupDoc :: (MonadLore m) => DiagnosticGroup -> m LoreDoc
diagnosticGroupDoc (groupKey, diagnostics) = do
  snippetContext <- loadSnippetContext groupKey
  pure $
    heading2 (diagnosticGroupTitle groupKey)
      <> mconcat (map (diagnosticDoc snippetContext) diagnostics)

diagnosticGroupTitle :: DiagnosticGroupKey -> Text
diagnosticGroupTitle = \case
  DiagnosticFileGroup filePath -> T.pack filePath
  DiagnosticOtherGroup spanText -> spanText

type SnippetContext = Maybe [Text]

diagnosticDoc :: SnippetContext -> Diagnostic -> LoreDoc
diagnosticDoc snippetContext diagnostic =
  heading3 (diagnosticSeverityTitle diagnostic)
    <> paragraph (truncateDiagnosticMessage (diagnosticMessageBody diagnostic))
    <> diagnosticHintsDoc diagnostic.diagnosticHints
    <> diagnosticSnippetDoc snippetContext diagnostic

truncateDiagnosticMessage :: Text -> Text
truncateDiagnosticMessage message
  | T.length message > maxDiagnosticMessageLength =
      T.take maxDiagnosticMessageLength message
  | otherwise =
      message
  where
    maxDiagnosticMessageLength = 700

diagnosticHintsDoc :: [Text] -> LoreDoc
diagnosticHintsDoc [] =
  mempty
diagnosticHintsDoc hints =
  paragraph "Hints:"
    <> bulletList (map paragraph hints)

diagnosticSnippetDoc :: SnippetContext -> Diagnostic -> LoreDoc
diagnosticSnippetDoc Nothing _ =
  mempty
diagnosticSnippetDoc (Just fileLines) Diagnostic {diagnosticSpan = RealDiagnosticSpan span'} =
  case renderSnippet fileLines span' of
    [] -> mempty
    renderedLines -> paragraph (T.intercalate "\n" (map T.pack renderedLines))
diagnosticSnippetDoc (Just _) Diagnostic {diagnosticSpan = UnhelpfulDiagnosticSpan {}} =
  mempty

loadSnippetContext :: (MonadLore m) => DiagnosticGroupKey -> m SnippetContext
loadSnippetContext = \case
  DiagnosticFileGroup filePath -> do
    maybeFileContents <- liftIO (try @IOException (TIO.readFile filePath))
    pure $
      case maybeFileContents of
        Right fileContents -> Just (T.lines fileContents)
        Left _ -> Nothing
  DiagnosticOtherGroup {} ->
    pure Nothing

renderSnippet :: [Text] -> Span -> [String]
renderSnippet fileLines Span {spanStartLine, spanStartCol, spanEndLine, spanEndCol}
  | spanStartLine <= 0 = []
  | spanEndLine < spanStartLine = []
  | otherwise =
      concatMap renderLineWithCaret [snippetStartLine .. snippetEndLine]
  where
    snippetStartLine = max 1 (spanStartLine - 2)
    snippetEndLine = min (length fileLines) spanEndLine

    renderLineWithCaret lineNumber =
      case safeLine fileLines lineNumber of
        Nothing -> []
        Just sourceLine ->
          [renderSnippetLine lineNumber sourceLine]
            <> maybe [] (\caretLine -> [caretLine]) (renderCaretLine lineNumber sourceLine)

    renderSnippetLine lineNumber sourceLine =
      padLeft 4 (show lineNumber) <> " | " <> T.unpack sourceLine

    renderCaretLine lineNumber sourceLine
      | lineNumber < spanStartLine || lineNumber > spanEndLine = Nothing
      | otherwise =
          let sourceLength = T.length sourceLine
              startColumn
                | lineNumber == spanStartLine = max 1 spanStartCol
                | otherwise = 1
              endColumnExclusive
                | lineNumber == spanEndLine = max startColumn spanEndCol
                | otherwise = sourceLength + 1
              caretOffset = max 0 (startColumn - 1)
              caretWidth = max 1 (min (sourceLength + 1) endColumnExclusive - startColumn)
           in Just $
                "     | "
                  <> replicate caretOffset ' '
                  <> replicate caretWidth '^'

safeLine :: [a] -> Int -> Maybe a
safeLine values lineNumber
  | lineNumber <= 0 = Nothing
  | otherwise =
      case drop (lineNumber - 1) values of
        value : _ -> Just value
        [] -> Nothing

padLeft :: Int -> String -> String
padLeft width value =
  replicate (max 0 (width - length value)) ' ' <> value
