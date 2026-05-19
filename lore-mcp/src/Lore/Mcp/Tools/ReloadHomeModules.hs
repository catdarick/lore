module Lore.Mcp.Tools.ReloadHomeModules where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl', nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore
  ( Diagnostic (..),
    DiagnosticClass (..),
    DiagnosticSpan (..),
    LoadHomeModulesOptions (..),
    LoadHomeModulesResult (..),
    MonadLore,
    Span (..),
    loadHomeModules,
  )
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithoutArgs (..))
import Lore.Mcp.Tools.Shared.Diagnostics (compactDiagnosticMessage, renderSummaryLine)

reloadHomeModulesTool :: (MonadLore m) => SomeTool m
reloadHomeModulesTool =
  SomeToolWithoutArgs
    ToolWithoutArgs
      { name = "reloadHomeModules",
        description = Just "Reloads all home modules, checks for errors, and applies safe auto-fixes when possible. This reload resets interpreter state (interactive bindings are cleared). Run this before tools that need up-to-date module information.",
        handler = reloadHomeModulesHandler
      }

reloadHomeModulesHandler :: (MonadLore m) => m Text
reloadHomeModulesHandler = do
  loadResult <- loadHomeModules LoadHomeModulesOptions {enableAutoRefactor = True}
  renderReloadHomeModulesResult loadResult

renderReloadHomeModulesResult :: (MonadLore m) => LoadHomeModulesResult -> m Text
renderReloadHomeModulesResult loadResult@LoadHomeModulesResult {loadHomeModulesDiagnostics}
  | null loadHomeModulesDiagnostics =
      pure $
        T.pack $
          unlines
            ([statusLine] <> autoFixedSummarySection loadResult)
  | otherwise =
      do
        let (visibleDiagnostics, hiddenDiagnostics) = splitAt maxRenderedDiagnostics loadHomeModulesDiagnostics
            visibleGroups = groupDiagnostics visibleDiagnostics
        renderedGroups <- mapM renderDiagnosticGroup visibleGroups
        pure $
          T.pack . unlines $
            [ statusLine,
              ""
            ]
              <> autoFixedSummarySection loadResult
              <> ["" | not (null (autoFixedSummarySection loadResult))]
              <> concatMap (<> [""]) renderedGroups
              <> hiddenDiagnosticsSummary hiddenDiagnostics
  where
    maxRenderedDiagnostics = 5
    statusLine
      | loadResult.loadHomeModulesFailed > 0 =
          "Failed to load "
            <> show loadResult.loadHomeModulesFailed
            <> " of "
            <> show loadResult.loadHomeModulesTotal
            <> " modules."
      | loadResult.loadHomeModulesAutofixed > 0 =
          "Successfully loaded all "
            <> show loadResult.loadHomeModulesTotal
            <> " modules after auto-fixing "
            <> show loadResult.loadHomeModulesAutofixed
            <> ". No errors left."
      | otherwise =
          "Successfully loaded all "
            <> show loadResult.loadHomeModulesTotal
            <> " modules. No errors found."

hiddenDiagnosticsSummary :: [Diagnostic] -> [String]
hiddenDiagnosticsSummary [] = []
hiddenDiagnosticsSummary hiddenDiagnostics =
  [ "... and "
      <> show hiddenCount
      <> " more diagnostics in "
      <> show hiddenModuleCount
      <> " modules."
  ]
  where
    hiddenCount = length hiddenDiagnostics
    hiddenModuleCount = length (groupDiagnostics hiddenDiagnostics)

autoFixedSummarySection :: LoadHomeModulesResult -> [String]
autoFixedSummarySection loadResult
  | null loadResult.loadHomeModulesAutofixSummaryByFile = []
  | otherwise =
      [ "Safe fixes applied:"
      ]
        <> concatMap renderAutofixedFileSummary loadResult.loadHomeModulesAutofixSummaryByFile

renderAutofixedFileSummary :: (FilePath, [String]) -> [String]
renderAutofixedFileSummary (filePath, summaries) =
  ["  - " <> filePath]
    <> map ("      * " <>) (nub summaries)

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

renderDiagnosticGroup :: (MonadLore m) => DiagnosticGroup -> m [String]
renderDiagnosticGroup (groupKey, diagnostics) = do
  snippetContext <- loadSnippetContext groupKey
  pure $
    diagnosticGroupHeader groupKey
      : concatMap (\(index, diagnostic) -> renderDiagnosticBlock snippetContext groupKey index diagnostic) (zip [1 :: Int ..] diagnostics)

type SnippetContext = Maybe [Text]

renderDiagnosticBlock :: SnippetContext -> DiagnosticGroupKey -> Int -> Diagnostic -> [String]
renderDiagnosticBlock snippetContext _groupKey index diagnostic@Diagnostic {diagnosticSeverity, diagnosticMessage, diagnosticHints} =
  [ "  " <> show index <> ". " <> summaryLine
  ]
    <> map ("      " <>) detailLines
    <> hintLines diagnosticHints
    <> snippetLines snippetContext diagnostic
  where
    compactLines = compactDiagnosticMessage diagnosticMessage
    summaryLine =
      renderSummaryLine diagnosticSeverity compactLines
    detailLines = tailOrEmpty compactLines

hintLines :: [Text] -> [String]
hintLines [] = []
hintLines hints =
  "      hints:" : map (("        - " <>) . T.unpack) hints

diagnosticGroupHeader :: DiagnosticGroupKey -> String
diagnosticGroupHeader = \case
  DiagnosticFileGroup filePath -> filePath
  DiagnosticOtherGroup spanText -> T.unpack spanText

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

snippetLines :: SnippetContext -> Diagnostic -> [String]
snippetLines Nothing _ = []
snippetLines (Just fileLines) Diagnostic {diagnosticSpan = RealDiagnosticSpan span'} =
  case renderSnippet fileLines span' of
    [] -> []
    renderedLines -> map ("      " <>) renderedLines
snippetLines (Just _) Diagnostic {diagnosticSpan = UnhelpfulDiagnosticSpan {}} = []

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

tailOrEmpty :: [a] -> [a]
tailOrEmpty [] = []
tailOrEmpty (_ : rest) = rest

isErrorLikeDiagnostic :: Diagnostic -> Bool
isErrorLikeDiagnostic Diagnostic {diagnosticClass, diagnosticSeverity} =
  diagnosticClass == DiagFatal
    || maybe False (isErrorSeverity . show) diagnosticSeverity

isErrorSeverity :: String -> Bool
isErrorSeverity renderedSeverity =
  renderedSeverity == "SevError"
    || renderedSeverity == "SevFatal"
