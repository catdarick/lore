module Lore.Tools.Cli.Internal.Completion
  ( completeLoreLine,
    completeLoadedModules,
    completePackages,
    completeSymbols,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lore
  ( PackageData (packageName),
    findMatchingSymbolLookupNamesByPrefix,
    findProjectModuleNamesByPrefix,
  )
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgSpec (..),
    CliArgs (..),
    CliFlagSpec (..),
    CliOptionSpec (..),
    CliPositionalSpec (..),
    CompletionContext (..),
    CompletionItem (..),
    CompletionProvider (..),
  )
import Lore.Tools.Cli.Internal.Help (findToolByNameOrAlias)
import Lore.Tools.Cli.Internal.ShellWords
  ( LineContext (..),
    parseLineContext,
  )
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    SomeCliTool (..),
  )
import qualified Lore.Tools.DiscoverProject as DiscoverProject
import System.Console.Haskeline

completeLoreLine :: (MonadIO m) => [SomeCliTool m] -> CompletionFunc m
completeLoreLine tools (leftReversed, right) = do
  let left = reverse leftReversed
  let context = parseLineContext left
  let replacementPrefix = replacementPrefixFor left context.lineCurrentToken
  case context.lineWordsBeforeCursor of
    [] ->
      completeFromItems replacementPrefix context.lineCurrentToken (toolCompletionItems tools)
    commandWord : argWordsBefore ->
      if isHelpWord commandWord
        then completeFromItems replacementPrefix context.lineCurrentToken (toolCompletionItems tools)
        else
          case findToolByNameOrAlias commandWord tools of
            Nothing ->
              completeFromItems replacementPrefix context.lineCurrentToken (toolCompletionItems tools)
            Just (SomeCliTool tool) -> do
              let CliArgs {cliArgsSpecs} = tool.cliToolArgs
              let completionContext =
                    CompletionContext
                      { completionContextWordsBeforeCursor = argWordsBefore,
                        completionContextCurrentToken = context.lineCurrentToken
                      }
              completeToolArguments (leftReversed, right) replacementPrefix completionContext cliArgsSpecs

completeSymbols :: CompletionContext -> LoreCliM [CompletionItem]
completeSymbols context = do
  let prefix = context.completionContextCurrentToken
  if T.null prefix
    then pure []
    else do
      symbolNames <- findMatchingSymbolLookupNamesByPrefix prefix
      pure
        ( take 50
            [ CompletionItem
                { completionInsert = symbolName,
                  completionDisplay = symbolName,
                  completionHelp = Nothing
                }
            | symbolName <- symbolNames
            ]
        )

completeLoadedModules :: CompletionContext -> LoreCliM [CompletionItem]
completeLoadedModules context = do
  let prefix = context.completionContextCurrentToken
  if T.null prefix
    then pure []
    else do
      moduleNames <- findProjectModuleNamesByPrefix prefix
      pure
        [ CompletionItem
            { completionInsert = moduleName,
              completionDisplay = moduleName,
              completionHelp = Nothing
            }
        | moduleName <- take 50 moduleNames
        ]

completePackages :: CompletionContext -> LoreCliM [CompletionItem]
completePackages _context = do
  output <- DiscoverProject.discoverProject
  pure
    [ CompletionItem
        { completionInsert = T.pack pkg.packageName,
          completionDisplay = T.pack pkg.packageName,
          completionHelp = Nothing
        }
      | pkg <- output.discoverProjectPackages
    ]

completeToolArguments :: (MonadIO m) => (String, String) -> String -> CompletionContext -> [CliArgSpec m] -> m (String, [Completion])
completeToolArguments originalInput replacementPrefix context specs =
  case parserState.pendingOption of
    Just pendingOption ->
      completeFromProvider
        originalInput
        replacementPrefix
        context
        pendingOption.cliOptionCompletion
    Nothing
      | T.null context.completionContextCurrentToken ->
          let optionItems = optionCompletionItems parserState.usedOptionLongs specs
           in if null optionItems
                then completeFromProvider originalInput replacementPrefix context (positionalCompletionProvider parserState.consumedPositionals specs)
                else completeFromItems replacementPrefix context.completionContextCurrentToken optionItems
      | T.isPrefixOf "-" context.completionContextCurrentToken ->
          completeFromItems
            replacementPrefix
            context.completionContextCurrentToken
            (optionCompletionItems parserState.usedOptionLongs specs)
      | otherwise ->
          completeFromProvider
            originalInput
            replacementPrefix
            context
            (positionalCompletionProvider parserState.consumedPositionals specs)
  where
    parserState = parseArgumentWords specs context.completionContextWordsBeforeCursor

data ArgumentParseState m = ArgumentParseState
  { pendingOption :: Maybe (CliOptionSpec m),
    usedOptionLongs :: Set.Set Text,
    consumedPositionals :: Int
  }

parseArgumentWords :: [CliArgSpec m] -> [Text] -> ArgumentParseState m
parseArgumentWords specs wordsBeforeCursor =
  foldl consumeWord initialState wordsBeforeCursor
  where
    initialState =
      ArgumentParseState
        { pendingOption = Nothing,
          usedOptionLongs = Set.empty,
          consumedPositionals = 0
        }

    consumeWord parseState word =
      case parseState.pendingOption of
        Just optionSpec ->
          parseState
            { pendingOption = Nothing,
              usedOptionLongs = Set.insert optionSpec.cliOptionLong parseState.usedOptionLongs
            }
        Nothing ->
          case lookupOption specs word of
            Just (Left optionSpec) ->
              parseState
                { pendingOption = Just optionSpec
                }
            Just (Right flagSpec) ->
              parseState
                { usedOptionLongs = Set.insert flagSpec.cliFlagLong parseState.usedOptionLongs
                }
            Nothing ->
              parseState
                { consumedPositionals = parseState.consumedPositionals + 1
                }

lookupOption :: [CliArgSpec m] -> Text -> Maybe (Either (CliOptionSpec m) CliFlagSpec)
lookupOption specs rawToken =
  let longName = T.stripPrefix "--" rawToken
      shortName = T.stripPrefix "-" rawToken
      matchingOption =
        case longName of
          Just value ->
            firstMatchingOption (\optionSpec -> value == optionSpec.cliOptionLong) specs
          Nothing ->
            case shortName >>= shortChar of
              Just value ->
                firstMatchingOption (\optionSpec -> Just value == optionSpec.cliOptionShort) specs
              Nothing -> Nothing
      matchingFlag =
        case longName of
          Just value ->
            firstMatchingFlag (\flagSpec -> value == flagSpec.cliFlagLong) specs
          Nothing ->
            case shortName >>= shortChar of
              Just value ->
                firstMatchingFlag (\flagSpec -> Just value == flagSpec.cliFlagShort) specs
              Nothing -> Nothing
   in case matchingOption of
        Just optionSpec -> Just (Left optionSpec)
        Nothing -> Right <$> matchingFlag

positionalCompletionProvider :: Int -> [CliArgSpec m] -> CompletionProvider m
positionalCompletionProvider consumedPositionals specs =
  case positionalSpecs of
    [] -> NoCompletion
    _
      | consumedPositionals < length positionalSpecs ->
          (positionalSpecs !! consumedPositionals).cliPositionalCompletion
      | otherwise ->
          case lastMaybe positionalSpecs of
            Just positional
              | positional.cliPositionalRepeatable -> positional.cliPositionalCompletion
            _ -> NoCompletion
  where
    positionalSpecs =
      [ positionalSpec
        | CliArgPositional positionalSpec <- specs
      ]

optionCompletionItems :: Set.Set Text -> [CliArgSpec m] -> [CompletionItem]
optionCompletionItems usedLongOptions specs =
  concatMap fromSpec specs
  where
    fromSpec = \case
      CliArgOption optionSpec
        | optionSpec.cliOptionRepeatable || not (Set.member optionSpec.cliOptionLong usedLongOptions) ->
            [ CompletionItem
                { completionInsert = "--" <> optionSpec.cliOptionLong,
                  completionDisplay = "--" <> optionSpec.cliOptionLong,
                  completionHelp = Just optionSpec.cliOptionDescription
                }
            ]
      CliArgFlag flagSpec
        | not (Set.member flagSpec.cliFlagLong usedLongOptions) ->
            [ CompletionItem
                { completionInsert = "--" <> flagSpec.cliFlagLong,
                  completionDisplay = "--" <> flagSpec.cliFlagLong,
                  completionHelp = Just flagSpec.cliFlagDescription
                }
            ]
      _ -> []

toolCompletionItems :: [SomeCliTool m] -> [CompletionItem]
toolCompletionItems tools =
  uniqueCompletionItems
    (concatMap toolItems tools)
  where
    toolItems (SomeCliTool tool) =
      CompletionItem
        { completionInsert = tool.cliToolName,
          completionDisplay = tool.cliToolName,
          completionHelp = Just tool.cliToolSummary
        }
        : [ CompletionItem
              { completionInsert = alias,
                completionDisplay = alias,
                completionHelp = Just ("alias for " <> tool.cliToolName)
              }
            | alias <- tool.cliToolAliases
          ]

completeFromProvider :: (MonadIO m) => (String, String) -> String -> CompletionContext -> CompletionProvider m -> m (String, [Completion])
completeFromProvider originalInput replacementPrefix context provider =
  case provider of
    NoCompletion ->
      pure (reverse replacementPrefix, [])
    StaticCompletion completionItems ->
      completeFromItems replacementPrefix context.completionContextCurrentToken completionItems
    DynamicCompletion dynamicProvider -> do
      completionItems <- dynamicProvider context
      completeFromItems replacementPrefix context.completionContextCurrentToken completionItems
    FileCompletion ->
      completeFilename originalInput
    DirectoryCompletion -> do
      (replacementToken, fileCompletions) <-
        completeFilename originalInput
      pure
        ( replacementToken,
          filter (not . isFinished) fileCompletions
        )

completeFromItems :: (Monad m) => String -> Text -> [CompletionItem] -> m (String, [Completion])
completeFromItems replacementPrefix currentToken completionItems =
  pure
    ( reverse replacementPrefix,
      map toHaskelineCompletion (matchingItems currentToken completionItems)
    )

matchingItems :: Text -> [CompletionItem] -> [CompletionItem]
matchingItems currentToken items =
  sortOn completionInsert
    ( filter
        (\item -> prefixMatches currentToken item.completionInsert)
        (uniqueCompletionItems items)
    )

toHaskelineCompletion :: CompletionItem -> Completion
toHaskelineCompletion completionItem =
  Completion
    { replacement = T.unpack completionItem.completionInsert,
      display = T.unpack displayValue,
      isFinished = True
    }
  where
    displayValue =
      case completionItem.completionHelp of
        Nothing -> completionItem.completionDisplay
        Just helpText -> completionItem.completionDisplay <> "\t" <> helpText

prefixMatches :: Text -> Text -> Bool
prefixMatches rawNeedle rawHaystack =
  T.toLower rawNeedle `T.isPrefixOf` T.toLower rawHaystack

isHelpWord :: Text -> Bool
isHelpWord rawWord =
  lowerWord == "help" || lowerWord == "?"
  where
    lowerWord = T.toLower rawWord

shortChar :: Text -> Maybe Char
shortChar rawText =
  case T.unpack rawText of
    [value] -> Just value
    _ -> Nothing

firstMatchingOption :: (CliOptionSpec m -> Bool) -> [CliArgSpec m] -> Maybe (CliOptionSpec m)
firstMatchingOption predicate = \case
  [] -> Nothing
  CliArgOption optionSpec : rest
    | predicate optionSpec -> Just optionSpec
    | otherwise -> firstMatchingOption predicate rest
  _ : rest -> firstMatchingOption predicate rest

firstMatchingFlag :: (CliFlagSpec -> Bool) -> [CliArgSpec m] -> Maybe CliFlagSpec
firstMatchingFlag predicate = \case
  [] -> Nothing
  CliArgFlag flagSpec : rest
    | predicate flagSpec -> Just flagSpec
    | otherwise -> firstMatchingFlag predicate rest
  _ : rest -> firstMatchingFlag predicate rest

lastMaybe :: [a] -> Maybe a
lastMaybe = \case
  [] -> Nothing
  [value] -> Just value
  _ : rest -> lastMaybe rest

uniqueCompletionItems :: [CompletionItem] -> [CompletionItem]
uniqueCompletionItems items =
  reverse (snd (foldl step (Set.empty, []) items))
  where
    step (seen, acc) item
      | Set.member item.completionInsert seen =
          (seen, acc)
      | otherwise =
          (Set.insert item.completionInsert seen, item : acc)

replacementPrefixFor :: String -> Text -> String
replacementPrefixFor leftRaw currentToken
  | T.null currentToken = leftRaw
  | otherwise =
      case T.stripSuffix currentToken (T.pack leftRaw) of
        Just prefix -> T.unpack prefix
        Nothing -> leftRaw
