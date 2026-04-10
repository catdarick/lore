module Lore.Mcp.Tools.Shared
  ( appendPartialLoadWarning,
    renderDiagnosticSummary,
    renderFailureWithPartialLoadWarning,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore (LoadTargetsResult (..))
import Lore.Diagnostics (Diagnostic (..))
import Lore.Mcp.Tools.Shared.Diagnostics (renderDiagnosticSummary)

appendPartialLoadWarning :: LoadTargetsResult -> Text -> Text -> Text
appendPartialLoadWarning loadResult partialLoadSuffix body
  | loadResult.loadTargetsModulesFailed > 0 =
      body
        <> "\n\n"
        <> renderPartialLoadWarning loadResult partialLoadSuffix
  | otherwise =
      body

renderFailureWithPartialLoadWarning :: LoadTargetsResult -> Text -> Text -> [Diagnostic] -> Text
renderFailureWithPartialLoadWarning loadResult partialLoadSuffix heading diagnostics =
  appendPartialLoadWarning loadResult partialLoadSuffix renderedBody
  where
    renderedBody =
      T.unlines $
        [heading]
          <> case diagnostics of
            [] -> ["- No diagnostics were produced."]
            _ -> map renderDiagnosticSummary diagnostics

renderPartialLoadWarning :: LoadTargetsResult -> Text -> Text
renderPartialLoadWarning loadResult partialLoadSuffix =
  "Warning: only "
    <> T.pack (show loadResult.loadTargetsModulesLoaded)
    <> " of "
    <> T.pack (show loadResult.loadTargetsModulesTotal)
    <> " modules loaded successfully. "
    <> partialLoadSuffix
