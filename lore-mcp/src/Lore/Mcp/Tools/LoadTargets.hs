module Lore.Mcp.Tools.LoadTargets where

import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace, toLower)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore
  ( Diagnostic (..),
    DiagnosticClass (..),
    DiagnosticSpan (..),
    LoadTargetsOptions (..),
    LoadTargetsResult (..),
    MonadLore,
    Span (..),
    loadTargets,
  )
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithoutArgs (..))

loadTargetsTool :: (MonadLore m) => SomeTool m
loadTargetsTool =
  SomeToolWithoutArgs
    ToolWithoutArgs
      { name = "loadTargets",
        description = Just "Load the targets of the current project, checking for errors and performing safe auto-fixes if possible.",
        handler = loadTargetsHandler
      }

loadTargetsHandler :: (MonadLore m) => m Text
loadTargetsHandler = do
  loadResult <- loadTargets LoadTargetsOptions {enableAutoRefactor = True}
  renderLoadTargetsResult loadResult

renderLoadTargetsResult :: (MonadLore m) => LoadTargetsResult -> m Text
renderLoadTargetsResult loadResult@LoadTargetsResult {loadTargetsDiagnostics}
  | null loadTargetsDiagnostics =
      pure $
        T.pack $
          unlines
            [statusLine]
  | otherwise =
      do
        let (visibleDiagnostics, hiddenDiagnostics) = splitAt maxRenderedDiagnostics loadTargetsDiagnostics
            visibleGroups = groupDiagnostics visibleDiagnostics
        renderedGroups <- mapM renderDiagnosticGroup visibleGroups
        pure $
          T.pack . unlines $
            [ statusLine,
              ""
            ]
              <> concatMap (<> [""]) renderedGroups
              <> hiddenDiagnosticsSummary hiddenDiagnostics
  where
    maxRenderedDiagnostics = 5
    statusLine
      | loadResult.loadTargetsModulesFailed > 0 =
          "Failed to load "
            <> show loadResult.loadTargetsModulesFailed
            <> " of "
            <> show loadResult.loadTargetsModulesTotal
            <> " modules."
      | loadResult.loadTargetsModulesAutofixed > 0 =
          "Successfully loaded all "
            <> show loadResult.loadTargetsModulesTotal
            <> " modules after auto-fixing "
            <> show loadResult.loadTargetsModulesAutofixed
            <> ". No errors left."
      | otherwise =
          "Successfully loaded all "
            <> show loadResult.loadTargetsModulesTotal
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

renderSeverity :: (Show a) => a -> String
renderSeverity severity =
  case show severity of
    "SevFatal" -> "fatal"
    "SevError" -> "error"
    "SevWarning" -> "warning"
    "SevInfo" -> "info"
    other -> map toLower (dropWhile (== ' ') other)

renderSummaryLine :: (Show a) => Maybe a -> [String] -> String
renderSummaryLine diagnosticSeverity compactLines =
  renderSeverityLabel diagnosticSeverity
    <> ": "
    <> case compactLines of
      firstLine : _ -> firstLine
      [] -> "<empty>"

renderSeverityLabel :: (Show a) => Maybe a -> String
renderSeverityLabel =
  maybe "diagnostic" renderSeverity

tailOrEmpty :: [a] -> [a]
tailOrEmpty [] = []
tailOrEmpty (_ : rest) = rest

compactDiagnosticMessage :: Text -> [String]
compactDiagnosticMessage rawMessage =
  go [] normalizedLines
  where
    normalizedLines =
      filter (not . T.null . T.strip) (T.lines rawMessage)

    go acc [] =
      reverse acc
    go acc (line : rest)
      | isContextBoundary cleanedLine =
          reverse acc
      | isContinuationLine line && not (null acc) =
          go (appendToHead (T.unpack cleanedLine) acc) rest
      | T.null cleanedLine =
          go acc rest
      | otherwise =
          go (T.unpack cleanedLine : acc) rest
      where
        cleanedLine = stripBulletPrefix (T.stripStart line)

appendToHead :: String -> [String] -> [String]
appendToHead extra = \case
  current : rest -> (current <> " " <> extra) : rest
  [] -> [extra]

stripBulletPrefix :: Text -> Text
stripBulletPrefix text =
  fromMaybe text $
    T.stripPrefix "* " text
      <|> T.stripPrefix "• " text

isContextBoundary :: Text -> Bool
isContextBoundary text =
  any
    (`T.isPrefixOf` text)
    [ "In the ",
      "In a ",
      "In an "
    ]

isContinuationLine :: Text -> Bool
isContinuationLine line =
  case T.uncons line of
    Just (firstChar, _) ->
      isSpace firstChar && not ("* " `T.isPrefixOf` T.stripStart line)
    Nothing ->
      False

isErrorLikeDiagnostic :: Diagnostic -> Bool
isErrorLikeDiagnostic Diagnostic {diagnosticClass, diagnosticSeverity} =
  diagnosticClass == DiagFatal
    || maybe False (isErrorSeverity . show) diagnosticSeverity

isErrorSeverity :: String -> Bool
isErrorSeverity renderedSeverity =
  renderedSeverity == "SevError"
    || renderedSeverity == "SevFatal"
