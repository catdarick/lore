module Lore.Mcp.Tools.Shared
  ( prependPartialLoadWarning,
    renderDiagnosticSummary,
    renderFailureWithPartialLoadWarning,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore (LoadTargetsResult (..))
import Lore.Diagnostics (Diagnostic (..))

prependPartialLoadWarning :: LoadTargetsResult -> Text -> Text -> Text
prependPartialLoadWarning loadResult partialLoadSuffix body
  | loadResult.loadTargetsModulesFailed > 0 =
      renderPartialLoadWarning loadResult partialLoadSuffix
        <> "\n"
        <> body
  | otherwise =
      body

renderFailureWithPartialLoadWarning :: LoadTargetsResult -> Text -> Text -> [Diagnostic] -> Text
renderFailureWithPartialLoadWarning loadResult partialLoadSuffix heading diagnostics =
  T.unlines $
    warningLines
      <> [heading]
      <> map renderDiagnosticSummary diagnostics
  where
    warningLines =
      [ renderPartialLoadWarning loadResult partialLoadSuffix
      | loadResult.loadTargetsModulesFailed > 0
      ]

renderPartialLoadWarning :: LoadTargetsResult -> Text -> Text
renderPartialLoadWarning loadResult partialLoadSuffix =
  "Warning: only "
    <> T.pack (show loadResult.loadTargetsModulesLoaded)
    <> " of "
    <> T.pack (show loadResult.loadTargetsModulesTotal)
    <> " modules loaded successfully. "
    <> partialLoadSuffix

renderDiagnosticSummary :: Diagnostic -> Text
renderDiagnosticSummary Diagnostic {diagnosticMessage} =
  "- " <> firstMessageLine
  where
    firstMessageLine =
      case filter (not . T.null) (map T.strip (T.lines diagnosticMessage)) of
        line : _ -> stripBulletPrefix line
        [] -> "<empty>"

stripBulletPrefix :: Text -> Text
stripBulletPrefix text =
  case T.stripPrefix "* " text of
    Just stripped -> stripped
    Nothing -> text
