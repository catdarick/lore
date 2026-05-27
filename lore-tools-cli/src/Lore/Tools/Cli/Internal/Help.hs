module Lore.Tools.Cli.Internal.Help
  ( findToolByNameOrAlias,
    renderGeneralHelp,
    renderToolHelp,
  )
where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgSpec (..),
    CliArgs (..),
    CliFlagSpec (..),
    CliOptionSpec (..),
    CliPositionalSpec (..),
  )
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    SomeCliTool (..),
  )

findToolByNameOrAlias :: Text -> [SomeCliTool m] -> Maybe (SomeCliTool m)
findToolByNameOrAlias rawQuery tools =
  let query = T.toLower rawQuery
   in findFirst (matches query) tools
  where
    matches query (SomeCliTool tool) =
      query == T.toLower tool.cliToolName
        || any (\alias -> query == T.toLower alias) tool.cliToolAliases

renderGeneralHelp :: [SomeCliTool m] -> Text
renderGeneralHelp tools =
  T.unlines
    ( [ "Available commands:",
        ""
      ]
        <> concatMap renderSummaryLine sortedTools
        <> [ "",
             "Interactive helpers:",
             "  help",
             "  help COMMAND",
             "  ? COMMAND",
             "  :quit"
           ]
    )
  where
    sortedTools = sortOn toolNameText tools

renderSummaryLine :: SomeCliTool m -> [Text]
renderSummaryLine (SomeCliTool tool) =
  [ "  " <> tool.cliToolName <> padTo 28 tool.cliToolName <> tool.cliToolSummary
  ]
    <> aliasLine
  where
    aliasLine =
      case tool.cliToolAliases of
        [] -> []
        aliases ->
          [ "      aliases: " <> T.intercalate ", " aliases
          ]

renderToolHelp :: SomeCliTool m -> Text
renderToolHelp (SomeCliTool tool) =
  T.unlines
    ( [ tool.cliToolName,
        "",
        tool.cliToolDescription,
        "",
        "Usage:",
        "  " <> renderUsage tool,
        ""
      ]
        <> renderArgumentsSection specs
        <> renderOptionsSection specs
        <> renderExamplesSection tool.cliToolExamples
    )
  where
    specs = tool.cliToolArgs.cliArgsSpecs

renderUsage :: CliTool m args -> Text
renderUsage tool =
  tool.cliToolName <> positionalUsage <> optionUsage
  where
    positionalUsage =
      foldMap (\item -> " " <> item) (map renderPositionalUsage positionals)
    optionUsage =
      foldMap (\item -> " [" <> item <> "]") (map renderOptionalUsage optionals)
    specs = tool.cliToolArgs.cliArgsSpecs
    positionals = [p | CliArgPositional p <- specs]
    optionals = [Left o | CliArgOption o <- specs] <> [Right f | CliArgFlag f <- specs]

renderPositionalUsage :: CliPositionalSpec m -> Text
renderPositionalUsage positional =
  if positional.cliPositionalRepeatable
    then positional.cliPositionalMetavar <> "..."
    else positional.cliPositionalMetavar

renderOptionalUsage :: Either (CliOptionSpec m) CliFlagSpec -> Text
renderOptionalUsage = \case
  Left optionSpec ->
    "--" <> optionSpec.cliOptionLong <> " " <> optionSpec.cliOptionMetavar
  Right flagSpec ->
    "--" <> flagSpec.cliFlagLong

renderArgumentsSection :: [CliArgSpec m] -> [Text]
renderArgumentsSection specs =
  case positionals of
    [] -> []
    _ ->
      [ "Arguments:" ]
        <> concatMap renderPositionalLine positionals
        <> [""]
  where
    positionals = [p | CliArgPositional p <- specs]

renderOptionsSection :: [CliArgSpec m] -> [Text]
renderOptionsSection specs =
  case options of
    [] -> []
    _ ->
      [ "Options:" ]
        <> concatMap renderOptionLine options
        <> [""]
  where
    options = [Left o | CliArgOption o <- specs] <> [Right f | CliArgFlag f <- specs]

renderExamplesSection :: [Text] -> [Text]
renderExamplesSection examples =
  case examples of
    [] -> []
    _ ->
      [ "Examples:" ]
        <> map ("  " <>) examples

renderPositionalLine :: CliPositionalSpec m -> [Text]
renderPositionalLine positional =
  [ "  " <> positional.cliPositionalMetavar,
    "    " <> positional.cliPositionalDescription
  ]

renderOptionLine :: Either (CliOptionSpec m) CliFlagSpec -> [Text]
renderOptionLine = \case
  Left optionSpec ->
    [ "  " <> optionLabel,
      "    " <> optionSpec.cliOptionDescription
    ]
    where
      optionLabel =
        "--" <> optionSpec.cliOptionLong <> " " <> optionSpec.cliOptionMetavar
  Right flagSpec ->
    [ "  --" <> flagSpec.cliFlagLong,
      "    " <> flagSpec.cliFlagDescription
    ]

padTo :: Int -> Text -> Text
padTo width value =
  let missing = max 1 (width - T.length value)
   in T.replicate missing " "

toolNameText :: SomeCliTool m -> Text
toolNameText (SomeCliTool tool) = tool.cliToolName

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst predicate = \case
  [] -> Nothing
  x : xs
    | predicate x -> Just x
    | otherwise -> findFirst predicate xs
