module Lore.Tools.ReloadHomeModules
  ( ReloadHomeModulesOptions (..),
    ReloadHomeModulesStatus (..),
    reloadHomeModules,
    reloadHomeModulesStatus,
    renderReloadHomeModulesResult,
    truncateDiagnosticMessage,
  )
where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl', nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore
  ( Diagnostic (..),
    DiagnosticSpan (..),
    HomeModulesLoadSummary (..),
    LoadHomeModulesOptions (..),
    LoadHomeModulesResult (..),
    MonadLore,
    Span (..),
    projectEnvironmentFailureMessage,
    projectEnvironmentFailureRequiresRestart,
  )
import qualified Lore as Core
import Lore.Tools.Render.Diagnostics (diagnosticHintsDoc, diagnosticMessageBody, diagnosticSeverityTitle)
import Lore.Tools.Render.Doc (LoreDoc, bulletList, heading2, heading3, paragraph)
import Lore.Tools.Result
  ( Paginated (..),
    PageRequest (..),
    RenderedResult (..),
    defaultPageRequest,
    paginateItemsWithPageRequest,
  )

data ReloadHomeModulesStatus
  = ReloadHomeModulesStatusSuccess
  | ReloadHomeModulesStatusCompilationFailure
  | ReloadHomeModulesStatusEnvironmentFailure
  | ReloadHomeModulesStatusRestartRequired
  deriving stock (Eq, Show)

newtype ReloadHomeModulesOptions = ReloadHomeModulesOptions
  { reloadHomeModulesDiagnosticsPageRequest :: Maybe PageRequest
  }
  deriving stock (Eq, Show)

reloadHomeModules :: (MonadLore m) => ReloadHomeModulesOptions -> m (RenderedResult LoadHomeModulesResult)
reloadHomeModules options = do
  loadResult <- Core.loadHomeModules LoadHomeModulesOptions {enableAutoRefactor = True}
  loreDoc <- renderReloadHomeModulesResultWithPageRequest options.reloadHomeModulesDiagnosticsPageRequest loadResult
  pure
    RenderedResult
      { renderedResultValue = loadResult,
        renderedResultDocument = loreDoc
      }

reloadHomeModulesStatus :: LoadHomeModulesResult -> ReloadHomeModulesStatus
reloadHomeModulesStatus loadResult =
  case loadResult of
    LoadHomeModulesCompleted summary
      | summary.homeModulesCompilationSucceeded -> ReloadHomeModulesStatusSuccess
      | otherwise -> ReloadHomeModulesStatusCompilationFailure
    LoadHomeModulesPreparationFailed failure
      | projectEnvironmentFailureRequiresRestart failure -> ReloadHomeModulesStatusRestartRequired
      | otherwise -> ReloadHomeModulesStatusEnvironmentFailure

renderReloadHomeModulesResult :: (MonadLore m) => LoadHomeModulesResult -> m LoreDoc
renderReloadHomeModulesResult =
  renderReloadHomeModulesResultWithPageRequest Nothing

renderReloadHomeModulesResultWithPageRequest ::
  (MonadLore m) =>
  Maybe PageRequest ->
  LoadHomeModulesResult ->
  m LoreDoc
renderReloadHomeModulesResultWithPageRequest _ (LoadHomeModulesPreparationFailed failure) =
  pure $ paragraph ("Project environment preparation failed: " <> T.pack (projectEnvironmentFailureMessage failure))
renderReloadHomeModulesResultWithPageRequest maybePageRequest loadResult@(LoadHomeModulesCompleted summary) =
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
          <> nextPageHintDoc summary.homeModulesDiagnostics diagnosticPage
  where
    diagnosticPage = paginateDiagnostics pageRequest summary.homeModulesDiagnostics
    paginatedDiagnostics = maybe [] (.paginatedItems) diagnosticPage
    pageRequest =
      maybe defaultPageRequest id maybePageRequest
    statusLine
      | summary.homeModulesFailed > 0 =
          "Failed to load "
            <> T.pack (show summary.homeModulesFailed)
            <> " of "
            <> T.pack (show summary.homeModulesTotal)
            <> " modules."
      | summary.homeModulesAutofixed > 0 =
          "Successfully loaded all "
            <> T.pack (show summary.homeModulesTotal)
            <> " modules after auto-fixing "
            <> T.pack (show summary.homeModulesAutofixed)
            <> ". No errors left."
      | otherwise =
          "Successfully loaded all "
            <> T.pack (show summary.homeModulesTotal)
            <> " modules. No errors found."

autoFixedSummaryDoc :: LoadHomeModulesResult -> LoreDoc
autoFixedSummaryDoc (LoadHomeModulesPreparationFailed _) = mempty
autoFixedSummaryDoc (LoadHomeModulesCompleted summary)
  | null summary.homeModulesAutofixSummaryByFile =
      mempty
  | otherwise =
      heading2 "Safe fixes applied"
        <> bulletList (map renderAutofixedFileDoc summary.homeModulesAutofixSummaryByFile)

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

paginateDiagnostics :: PageRequest -> [Diagnostic] -> Maybe (Paginated Diagnostic)
paginateDiagnostics request diagnostics =
  paginateItemsWithPageRequest request diagnostics

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
