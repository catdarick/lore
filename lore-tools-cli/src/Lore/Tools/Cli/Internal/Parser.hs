module Lore.Tools.Cli.Internal.Parser
  ( CliOptions (..),
    CliMode (..),
    OutputFormat (..),
    parserInfo,
    parseCliWords,
    buildInvocationParser,
    outputFormatParser,
    outputFormatReader,
    resultLimitReader,
    depthReader,
    verbosityReader,
    renderResultLimit,
  )
where

import Data.Char (isDigit, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Tools.Cli.Internal.Annotated (CliArgs (..))
import Lore.Tools.Cli.Internal.Tool
  ( CliInvocation (..),
    CliTool (..),
    SomeCliTool (..),
  )
import Lore.Tools.Result (ResultLimit (..))
import Options.Applicative

newtype CliOptions m = CliOptions
  { cliMode :: CliMode m
  }

data CliMode m
  = CliInteractive OutputFormat
  | CliSingle OutputFormat (CliInvocation m)

data OutputFormat
  = FormatMarkdown
  | FormatJson
  deriving stock (Eq, Show)

parserInfo :: [SomeCliTool m] -> ParserInfo (CliOptions m)
parserInfo tools =
  info
    (cliOptionsParser tools <**> helper)
    ( fullDesc
        <> progDesc "CLI frontend for lore-tools"
    )

parseCliWords :: [SomeCliTool m] -> [String] -> Either String (CliInvocation m)
parseCliWords tools argv =
  case execParserPure defaultPrefs infoForInteractive argv of
    Success cliInvocation -> Right cliInvocation
    Failure failure ->
      let (message, _) = renderFailure failure "lore"
       in Left message
    CompletionInvoked _ ->
      Left "Completion is not supported in interactive mode."
  where
    infoForInteractive =
      info
        (buildInvocationParser tools <**> helper)
        mempty

buildInvocationParser :: [SomeCliTool m] -> Parser (CliInvocation m)
buildInvocationParser tools =
  hsubparser (foldMap commandForTool tools)

outputFormatParser :: Parser OutputFormat
outputFormatParser =
  option
    outputFormatReader
    ( long "format"
        <> value FormatMarkdown
        <> showDefaultWith (const "markdown")
        <> metavar "markdown|json"
        <> help "Output format"
    )

outputFormatReader :: ReadM OutputFormat
outputFormatReader =
  eitherReader \raw ->
    case map toLower raw of
      "markdown" -> Right FormatMarkdown
      "json" -> Right FormatJson
      _ -> Left "expected markdown or json"

resultLimitReader :: ReadM ResultLimit
resultLimitReader =
  eitherReader \raw ->
    case map toLower raw of
      "unlimited" -> Right Unlimited
      _
        | all isDigit raw && not (null raw) -> Right (Limit (read raw))
        | otherwise -> Left "expected positive integer or 'unlimited'"

depthReader :: ReadM (Maybe Int)
depthReader =
  eitherReader \raw ->
    case map toLower raw of
      "unlimited" -> Right Nothing
      _
        | all isDigit raw && not (null raw) -> Right (Just (read raw))
        | otherwise -> Left "expected positive integer or 'unlimited'"

verbosityReader :: ReadM Text
verbosityReader =
  eitherReader \raw ->
    case map toLower raw of
      "low" -> Right "low"
      "medium" -> Right "medium"
      "high" -> Right "high"
      _ -> Left "expected low, medium, or high"

renderResultLimit :: ResultLimit -> String
renderResultLimit = \case
  Unlimited -> "unlimited"
  Limit n -> show n

cliOptionsParser :: [SomeCliTool m] -> Parser (CliOptions m)
cliOptionsParser tools =
  CliOptions
    <$> modeParser tools

modeParser :: [SomeCliTool m] -> Parser (CliMode m)
modeParser tools =
  buildMode
    <$> outputFormatParser
    <*> optional (buildInvocationParser tools)
  where
    buildMode outputFormat maybeInvocation =
      case maybeInvocation of
        Nothing -> CliInteractive outputFormat
        Just invocation -> CliSingle outputFormat invocation

commandForTool :: SomeCliTool m -> Mod CommandFields (CliInvocation m)
commandForTool (SomeCliTool tool) =
  command
    (T.unpack tool.cliToolName)
    (info parser infoMod)
    <> foldMap (aliasCommand parser infoMod) tool.cliToolAliases
  where
    parser =
      case tool.cliToolArgs of
        CliArgs {cliArgsParser} ->
          CliInvocation tool <$> cliArgsParser
    infoMod =
      progDesc (T.unpack tool.cliToolSummary)
        <> fullDesc

aliasCommand :: Parser (CliInvocation m) -> InfoMod (CliInvocation m) -> Text -> Mod CommandFields (CliInvocation m)
aliasCommand parser infoMod aliasName =
  internal
    <> command
      (T.unpack aliasName)
      (info parser infoMod)
