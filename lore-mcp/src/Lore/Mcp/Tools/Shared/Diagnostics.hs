module Lore.Mcp.Tools.Shared.Diagnostics
  ( compactDiagnosticMessage,
    renderDiagnosticSummary,
    renderSummaryLine,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lore (Diagnostic (..))

renderDiagnosticSummary :: Diagnostic -> Text
renderDiagnosticSummary Diagnostic {diagnosticMessage} =
  "- " <> summarizedMessage
  where
    summarizedMessage =
      case compactDiagnosticMessage diagnosticMessage of
        firstLine : _ -> T.pack firstLine
        [] -> "<empty>"

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

renderSeverity :: (Show a) => a -> String
renderSeverity severity =
  case show severity of
    "SevFatal" -> "fatal"
    "SevError" -> "error"
    "SevWarning" -> "warning"
    "SevInfo" -> "info"
    other -> map toLower (dropWhile (== ' ') other)

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
