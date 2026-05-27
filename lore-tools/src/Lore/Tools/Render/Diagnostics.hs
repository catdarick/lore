module Lore.Tools.Render.Diagnostics
  ( diagnosticMessageBody,
    diagnosticSeverityTitle,
    diagnosticHintsDoc,
    diagnosticSummaryDoc,
    diagnosticSummaryWithHintsDoc,
    compactDiagnosticMessage,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lore (Diagnostic (..))
import Lore.Tools.Render.Doc (LoreDoc, bulletList, paragraph)

diagnosticSummaryDoc :: [Diagnostic] -> LoreDoc
diagnosticSummaryDoc diagnostics =
  case diagnostics of
    [] ->
      bulletList [paragraph "No diagnostics were produced."]
    _ ->
      bulletList (map (paragraph . diagnosticSummaryText) diagnostics)

diagnosticSummaryWithHintsDoc :: [Diagnostic] -> LoreDoc
diagnosticSummaryWithHintsDoc diagnostics =
  diagnosticSummaryDoc diagnostics
    <> diagnosticHintsDoc (concatMap (.diagnosticHints) diagnostics)

diagnosticHintsDoc :: [Text] -> LoreDoc
diagnosticHintsDoc [] =
  mempty
diagnosticHintsDoc hints =
  paragraph "Hints:"
    <> bulletList (map paragraph hints)

diagnosticSeverityTitle :: Diagnostic -> Text
diagnosticSeverityTitle diagnostic =
  case renderSeverityLabel diagnostic.diagnosticSeverity of
    "fatal" -> "Fatal"
    "error" -> "Error"
    "warning" -> "Warning"
    "info" -> "Info"
    "diagnostic" -> "Diagnostic"
    other ->
      T.toTitle (T.pack other)

diagnosticMessageBody :: Diagnostic -> Text
diagnosticMessageBody diagnostic =
  case compactDiagnosticMessage diagnostic.diagnosticMessage of
    [] ->
      "<empty>"
    lines' ->
      T.intercalate "\n" (map T.pack lines')

diagnosticSummaryText :: Diagnostic -> Text
diagnosticSummaryText Diagnostic {diagnosticMessage} =
  summarizedMessage
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
