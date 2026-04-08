module Lore.Mcp.Tools.LoadTargets where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Lore
  ( Diagnostic (..),
    DiagnosticClass (..),
    DiagnosticCodeInfo (..),
    DiagnosticSpan (..),
    LoadTargetsOptions (..),
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
  diagnostics <- loadTargets LoadTargetsOptions {enableAutoRefactor = True}
  pure (renderLoadTargetsDiagnostics diagnostics)

renderLoadTargetsDiagnostics :: [Diagnostic] -> Text
renderLoadTargetsDiagnostics diagnostics
  | null diagnostics =
      "Targets loaded successfully with no diagnostics."
  | otherwise =
      T.pack . unlines $
        [ summaryLine,
          ""
        ]
          <> concatMap (\(index, diagnostic) -> renderDiagnosticBlock index diagnostic <> [""]) (zip [1 :: Int ..] diagnostics)
  where
    summaryLine =
      if any isErrorLikeDiagnostic diagnostics
        then "Targets loaded with errors:"
        else "Targets loaded with diagnostics:"

renderDiagnosticBlock :: Int -> Diagnostic -> [String]
renderDiagnosticBlock index Diagnostic {diagnosticClass, diagnosticSeverity, diagnosticReason, diagnosticCode, diagnosticSpan, diagnosticMessage} =
  [ show index <> ". " <> locationLine,
    "   " <> headline,
    "   " <> codeLine
  ]
    <> map ("   " <>) (messageLines diagnosticMessage)
  where
    locationLine = renderDiagnosticSpan diagnosticSpan
    headline =
      intercalate
        " | "
        ( [renderDiagnosticClass diagnosticClass]
            <> maybe [] (\severity -> [renderSeverity severity]) diagnosticSeverity
            <> maybe [] (\reason -> [T.unpack reason]) diagnosticReason
        )
    codeLine = maybe "code: none" (("code: " <>) . renderDiagnosticCodeInfo) diagnosticCode

messageLines :: Text -> [String]
messageLines rawMessage =
  case filter (not . null) (map T.unpack (T.lines rawMessage)) of
    [] -> ["message: <empty>"]
    firstLine : otherLines -> ("message: " <> firstLine) : otherLines

renderDiagnosticSpan :: DiagnosticSpan -> String
renderDiagnosticSpan = \case
  RealDiagnosticSpan Span {spanFile, spanStartLine, spanStartCol, spanEndLine, spanEndCol} ->
    spanFile
      <> ":"
      <> show spanStartLine
      <> ":"
      <> show spanStartCol
      <> "-"
      <> show spanEndLine
      <> ":"
      <> show spanEndCol
  UnhelpfulDiagnosticSpan spanText ->
    T.unpack spanText

renderDiagnosticClass :: DiagnosticClass -> String
renderDiagnosticClass = \case
  DiagOutput -> "output"
  DiagFatal -> "fatal"
  DiagInteractive -> "interactive"
  DiagDump -> "dump"
  DiagInfo -> "info"
  DiagCompiler -> "compiler"

renderSeverity :: (Show a) => a -> String
renderSeverity severity =
  case show severity of
    "SevFatal" -> "fatal"
    "SevError" -> "error"
    "SevWarning" -> "warning"
    "SevInfo" -> "info"
    other -> map toLower (dropWhile (== ' ') other)

renderDiagnosticCodeInfo :: DiagnosticCodeInfo -> String
renderDiagnosticCodeInfo DiagnosticCodeInfo {diagnosticCodeNamespace, diagnosticCodeNumber} =
  T.unpack diagnosticCodeNamespace <> "-" <> show diagnosticCodeNumber

isErrorLikeDiagnostic :: Diagnostic -> Bool
isErrorLikeDiagnostic Diagnostic {diagnosticClass, diagnosticSeverity} =
  diagnosticClass == DiagFatal
    || maybe False (isErrorSeverity . show) diagnosticSeverity

isErrorSeverity :: String -> Bool
isErrorSeverity renderedSeverity =
  renderedSeverity == "SevError"
    || renderedSeverity == "SevFatal"
