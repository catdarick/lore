module Lore.Tools.Cli.Tools.GetDefinition
  ( getDefinitionCliTool,
  )
where

import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as T
import Lore (MonadLore)
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgSpec (CliArgFlag),
    CliArgs (..),
    CliFlagSpec (..),
    CompletionProvider (DynamicCompletion),
    optionalOptionWithReader,
    somePositionalText,
  )
import Lore.Tools.Cli.Internal.Completion (completeSymbols)
import Lore.Tools.Cli.Internal.Parser (depthReader)
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    defaultSessionRequirements,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common
  ( limitArg,
    offsetArg,
    resultLimitToInt,
    staticCompletionValues,
  )
import qualified Lore.Tools.GetDefinition as GetDefinition
import qualified Lore.Tools.Internal.DefinitionSourceRendering as DefinitionSourceRendering
import Lore.Tools.Internal.SymbolResolution (unresolvedSymbolQueriesMessage)
import Lore.Tools.Render.Doc
  ( LoreDoc,
    SourceFile,
    ToLoreDoc (toLoreDoc),
    paragraph,
    sourceFile,
  )
import Lore.Tools.Result
  ( PageRequest (..),
    Paginated (..),
    ResultLimit,
    ToolRun (..),
  )
import Numeric.Natural (Natural)
import Options.Applicative

data GetDefinitionArgs = GetDefinitionArgs
  { getDefinitionSymbolsArg :: [Text],
    getDefinitionExpansionArg :: GetDefinitionExpansionArg,
    getDefinitionOffsetArg :: Int,
    getDefinitionLimitArg :: ResultLimit,
    getDefinitionMaxDepthArg :: Maybe Int
  }

data GetDefinitionExpansionArg
  = ExpansionNone
  | ExpansionDirect
  | ExpansionRecursive

getDefinitionCliTool :: CliTool LoreCliM GetDefinitionArgs
getDefinitionCliTool =
  CliTool
    { cliToolName = "get-definition",
      cliToolAliases = ["def", "definition"],
      cliToolSummary = "Render source definitions for symbols",
      cliToolDescription = "Render source definitions for symbols with optional dependency expansion.",
      cliToolExamples =
        [ "lore-cli get-definition Demo.lookupOrZero",
          "lore-cli get-definition Demo.lookupOrZero --recursive",
          "lore-cli get-definition Demo.lookupOrZero --recursive --max-depth 5"
        ],
      cliToolArgs = getDefinitionArgs,
      cliToolRun = successfulCliToolRun runGetDefinition,
      cliToolSession = const defaultSessionRequirements
    }

getDefinitionArgs :: CliArgs LoreCliM GetDefinitionArgs
getDefinitionArgs =
  GetDefinitionArgs
    <$> somePositionalText "SYMBOL" "Symbol to render" (DynamicCompletion completeSymbols)
    <*> expansionArg
    <*> offsetArg
    <*> limitArg
    <*> maxDepthArg

expansionArg :: CliArgs m GetDefinitionExpansionArg
expansionArg =
  CliArgs
    { cliArgsParser =
        flag' ExpansionDirect (long "direct" <> help "Expand direct dependencies")
          <|> flag' ExpansionRecursive (long "recursive" <> help "Expand recursively")
          <|> pure ExpansionNone,
      cliArgsSpecs =
        [ CliArgFlag
            CliFlagSpec
              { cliFlagLong = "direct",
                cliFlagShort = Nothing,
                cliFlagDescription = "Expand direct dependencies"
              },
          CliArgFlag
            CliFlagSpec
              { cliFlagLong = "recursive",
                cliFlagShort = Nothing,
                cliFlagDescription = "Expand recursively"
              }
        ]
    }

maxDepthArg :: CliArgs m (Maybe Int)
maxDepthArg =
  join
    <$> optionalOptionWithReader
      depthReader
      "max-depth"
      Nothing
      "N|unlimited"
      "Maximum recursive expansion depth"
      (staticCompletionValues ["0", "1", "2", "3", "5", "unlimited"])

runGetDefinition :: GetDefinitionArgs -> LoreCliM LoreDoc
runGetDefinition args = do
  result <-
    GetDefinition.getDefinitionHandlerWithStrategy
      request
      buildWithoutKnowledgeCache
  pure $
    case result of
      ToolRunBlocked blocked ->
        toLoreDoc blocked
      ToolRunReady output ->
        renderGetDefinitionCoreOutput args.getDefinitionSymbolsArg output
  where
    request =
      GetDefinition.GetDefinitionRequest
        { getDefinitionRequestSymbols = args.getDefinitionSymbolsArg,
          getDefinitionRequestPageRequest =
            Just (PageRequest args.getDefinitionOffsetArg args.getDefinitionLimitArg),
          getDefinitionRequestExpansion =
            Just
              (toExpansion args.getDefinitionExpansionArg args.getDefinitionLimitArg args.getDefinitionMaxDepthArg)
        }

toExpansion :: GetDefinitionExpansionArg -> ResultLimit -> Maybe Int -> GetDefinition.DefinitionExpansion
toExpansion expansion limit maybeMaxDepth =
  case expansion of
    ExpansionNone ->
      GetDefinition.NoExpansion
    ExpansionDirect ->
      GetDefinition.ExpandDirect
    ExpansionRecursive ->
      GetDefinition.ExpandRecursive
        GetDefinition.RecursiveExpansionOptions
          { recursiveExpansionMaxDepth = toRecursiveDepth maybeMaxDepth,
            recursiveExpansionMaxDefinitions = limit
          }

toRecursiveDepth :: Maybe Int -> Maybe Natural
toRecursiveDepth = \case
  Nothing ->
    Nothing
  Just depth ->
    Just (fromIntegral (max 0 depth))

buildWithoutKnowledgeCache ::
  (MonadLore m) =>
  GetDefinition.BuildDefinitionsStrategy m
buildWithoutKnowledgeCache pageRequest _directlyRequestedSymbolNames definitionEntries = do
  filteredDefinitionPage <-
    DefinitionSourceRendering.buildPaginatedDefinitionSourceFiles
      pageRequest.pageOffset
      (resultLimitToInt pageRequest.pageLimit)
      definitionEntries
  pure
    GetDefinition.FilteredDefinitions
      { filteredDefinitionPage,
        filteredOmittedDefinitions = GetDefinition.mkOmittedDefinitions []
      }

renderGetDefinitionCoreOutput :: [Text] -> GetDefinition.GetDefinitionCoreOutput -> LoreDoc
renderGetDefinitionCoreOutput symbols = \case
  GetDefinition.GetDefinitionCoreFailedResult failed ->
    renderGetDefinitionFailure failed.getDefinitionCoreFailure
      <> maybe mempty toLoreDoc failed.getDefinitionCoreFailedPartialLoadWarning
  GetDefinition.GetDefinitionCoreReadyResult ready ->
    case ready.getDefinitionCorePage of
      Nothing ->
        maybe mempty toLoreDoc ready.getDefinitionCorePartialLoadWarning
          <> paragraph
            ( if ready.getDefinitionCoreOmitted.omittedDefinitionCount > 0
                then "No changed definitions to render."
                else "No definitions found for " <> renderQuotedTexts symbols <> "."
            )
      Just page ->
        mconcat
          [ mconcat (map sourceFile page.paginatedItems),
            renderGetDefinitionFooter page ready.getDefinitionCoreOmitted,
            maybe mempty toLoreDoc ready.getDefinitionCorePartialLoadWarning
          ]

renderGetDefinitionFailure :: GetDefinition.GetDefinitionCoreFailure -> LoreDoc
renderGetDefinitionFailure = \case
  GetDefinition.GetDefinitionUnresolvedSymbols unresolved ->
    paragraph (unresolvedSymbolQueriesMessage unresolved)
  GetDefinition.GetDefinitionInternalError message ->
    paragraph message

renderGetDefinitionFooter :: Paginated SourceFile -> GetDefinition.OmittedDefinitions -> LoreDoc
renderGetDefinitionFooter page omitted =
  if null footerLines
    then mempty
    else paragraph (T.intercalate "\n" footerLines)
  where
    footerLines =
      paginationLine
        <> omittedLine
    paginationLine
      | remainingItems > 0 =
          [ "And "
              <> T.pack (show remainingItems)
              <> " more definition results (set --offset to "
              <> T.pack (show nextOffset)
              <> ")."
          ]
      | otherwise = []
    omittedLine
      | omitted.omittedDefinitionCount > 0 =
          [ "Omitted "
              <> T.pack (show omitted.omittedDefinitionCount)
              <> " unchanged definitions."
          ]
      | otherwise = []
    remainingItems =
      max 0 (page.paginatedTotalItems - page.paginatedSkippedItems - page.paginatedConsumedItems)
    nextOffset =
      page.paginatedSkippedItems + page.paginatedConsumedItems

renderQuotedTexts :: [Text] -> Text
renderQuotedTexts values =
  "[" <> T.intercalate ", " (map (\symbolText -> "\"" <> symbolText <> "\"") values) <> "]"
