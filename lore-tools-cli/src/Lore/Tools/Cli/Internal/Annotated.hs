module Lore.Tools.Cli.Internal.Annotated
  ( CliArgs (..),
    CliArgSpec (..),
    CliOptionSpec (..),
    CliFlagSpec (..),
    CliPositionalSpec (..),
    CompletionProvider (..),
    CompletionContext (..),
    CompletionItem (..),
    positionalText,
    somePositionalText,
    optionalOptionText,
    manyOptionText,
    manyOptionWithReader,
    optionWithReader,
    optionalOptionWithReader,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

data CliArgs m a = CliArgs
  { cliArgsParser :: Parser a,
    cliArgsSpecs :: [CliArgSpec m]
  }

instance Functor (CliArgs m) where
  fmap f (CliArgs parser specs) =
    CliArgs (fmap f parser) specs

instance Applicative (CliArgs m) where
  pure x =
    CliArgs (pure x) []

  CliArgs parserF specsF <*> CliArgs parserA specsA =
    CliArgs (parserF <*> parserA) (specsF <> specsA)

data CliArgSpec m
  = CliArgOption (CliOptionSpec m)
  | CliArgFlag CliFlagSpec
  | CliArgPositional (CliPositionalSpec m)

data CliOptionSpec m = CliOptionSpec
  { cliOptionLong :: Text,
    cliOptionShort :: Maybe Char,
    cliOptionMetavar :: Text,
    cliOptionDescription :: Text,
    cliOptionRepeatable :: Bool,
    cliOptionCompletion :: CompletionProvider m
  }

data CliFlagSpec = CliFlagSpec
  { cliFlagLong :: Text,
    cliFlagShort :: Maybe Char,
    cliFlagDescription :: Text
  }

data CliPositionalSpec m = CliPositionalSpec
  { cliPositionalMetavar :: Text,
    cliPositionalDescription :: Text,
    cliPositionalRepeatable :: Bool,
    cliPositionalCompletion :: CompletionProvider m
  }

data CompletionProvider m
  = NoCompletion
  | StaticCompletion [CompletionItem]
  | FileCompletion
  | DirectoryCompletion
  | DynamicCompletion (CompletionContext -> m [CompletionItem])

data CompletionContext = CompletionContext
  { completionContextWordsBeforeCursor :: [Text],
    completionContextCurrentToken :: Text
  }

data CompletionItem = CompletionItem
  { completionInsert :: Text,
    completionDisplay :: Text,
    completionHelp :: Maybe Text
  }

positionalText :: Text -> Text -> CompletionProvider m -> CliArgs m Text
positionalText metavarText description completion =
  CliArgs
    { cliArgsParser =
        T.pack <$> strArgument (metavar (T.unpack metavarText) <> help (T.unpack description)),
      cliArgsSpecs =
        [ CliArgPositional
            CliPositionalSpec
              { cliPositionalMetavar = metavarText,
                cliPositionalDescription = description,
                cliPositionalRepeatable = False,
                cliPositionalCompletion = completion
              }
        ]
    }

somePositionalText :: Text -> Text -> CompletionProvider m -> CliArgs m [Text]
somePositionalText metavarText description completion =
  CliArgs
    { cliArgsParser =
        some (T.pack <$> strArgument (metavar (T.unpack metavarText) <> help (T.unpack description))),
      cliArgsSpecs =
        [ CliArgPositional
            CliPositionalSpec
              { cliPositionalMetavar = metavarText,
                cliPositionalDescription = description,
                cliPositionalRepeatable = True,
                cliPositionalCompletion = completion
              }
        ]
    }

optionalOptionText ::
  Text ->
  Maybe Char ->
  Text ->
  Text ->
  CompletionProvider m ->
  CliArgs m (Maybe Text)
optionalOptionText longName shortName metavarText description completion =
  CliArgs
    { cliArgsParser =
        optional
          ( T.pack
              <$> strOption
                ( optionModifier longName shortName
                    <> metavar (T.unpack metavarText)
                    <> help (T.unpack description)
                )
          ),
      cliArgsSpecs =
        [ CliArgOption
            CliOptionSpec
              { cliOptionLong = longName,
                cliOptionShort = shortName,
                cliOptionMetavar = metavarText,
                cliOptionDescription = description,
                cliOptionRepeatable = False,
                cliOptionCompletion = completion
              }
        ]
    }

manyOptionText ::
  Text ->
  Maybe Char ->
  Text ->
  Text ->
  CompletionProvider m ->
  CliArgs m [Text]
manyOptionText longName shortName metavarText description completion =
  CliArgs
    { cliArgsParser =
        many
          ( T.pack
              <$> strOption
                ( optionModifier longName shortName
                    <> metavar (T.unpack metavarText)
                    <> help (T.unpack description)
                )
          ),
      cliArgsSpecs =
        [ CliArgOption
            CliOptionSpec
              { cliOptionLong = longName,
                cliOptionShort = shortName,
                cliOptionMetavar = metavarText,
                cliOptionDescription = description,
                cliOptionRepeatable = True,
                cliOptionCompletion = completion
              }
        ]
    }

manyOptionWithReader ::
  ReadM a ->
  Text ->
  Maybe Char ->
  Text ->
  Text ->
  CompletionProvider m ->
  CliArgs m [a]
manyOptionWithReader reader longName shortName metavarText description completion =
  CliArgs
    { cliArgsParser =
        many
          ( option
              reader
              ( optionModifier longName shortName
                  <> metavar (T.unpack metavarText)
                  <> help (T.unpack description)
              )
          ),
      cliArgsSpecs =
        [ CliArgOption
            CliOptionSpec
              { cliOptionLong = longName,
                cliOptionShort = shortName,
                cliOptionMetavar = metavarText,
                cliOptionDescription = description,
                cliOptionRepeatable = True,
                cliOptionCompletion = completion
              }
        ]
    }

optionWithReader ::
  ReadM a ->
  Text ->
  Maybe Char ->
  Text ->
  Text ->
  Maybe (a -> String) ->
  Maybe a ->
  CompletionProvider m ->
  CliArgs m a
optionWithReader reader longName shortName metavarText description maybeShowDefault maybeDefault completion =
  CliArgs
    { cliArgsParser =
        option
          reader
          ( optionModifier longName shortName
              <> metavar (T.unpack metavarText)
              <> help (T.unpack description)
              <> maybe mempty showDefaultWith maybeShowDefault
              <> maybe mempty value maybeDefault
          ),
      cliArgsSpecs =
        [ CliArgOption
            CliOptionSpec
              { cliOptionLong = longName,
                cliOptionShort = shortName,
                cliOptionMetavar = metavarText,
                cliOptionDescription = description,
                cliOptionRepeatable = False,
                cliOptionCompletion = completion
              }
        ]
    }

optionalOptionWithReader ::
  ReadM a ->
  Text ->
  Maybe Char ->
  Text ->
  Text ->
  CompletionProvider m ->
  CliArgs m (Maybe a)
optionalOptionWithReader reader longName shortName metavarText description completion =
  CliArgs
    { cliArgsParser =
        optional
          ( option
              reader
              ( optionModifier longName shortName
                  <> metavar (T.unpack metavarText)
                  <> help (T.unpack description)
              )
          ),
      cliArgsSpecs =
        [ CliArgOption
            CliOptionSpec
              { cliOptionLong = longName,
                cliOptionShort = shortName,
                cliOptionMetavar = metavarText,
                cliOptionDescription = description,
                cliOptionRepeatable = False,
                cliOptionCompletion = completion
              }
        ]
    }

optionModifier :: Text -> Maybe Char -> Mod OptionFields a
optionModifier longName shortName =
  long (T.unpack longName)
    <> maybe mempty short shortName
