module Lore.Tools.Cli.Internal.ShellWords
  ( QuoteMode (..),
    LineContext (..),
    shellWords,
    parseLineContext,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

data QuoteMode
  = QuoteNone
  | QuoteSingle
  | QuoteDouble
  deriving stock (Eq, Show)

data LineContext = LineContext
  { lineWordsBeforeCursor :: [Text],
    lineCurrentToken :: Text,
    lineEndsWithSpace :: Bool,
    lineQuoteMode :: QuoteMode
  }
  deriving stock (Eq, Show)

shellWords :: String -> [String]
shellWords raw =
  map T.unpack lineContextWords
  where
    lineContext = parseLineContext raw
    lineContextWords =
      lineContext.lineWordsBeforeCursor
        <> (if T.null lineContext.lineCurrentToken then [] else [lineContext.lineCurrentToken])

parseLineContext :: String -> LineContext
parseLineContext raw =
  LineContext
    { lineWordsBeforeCursor = map T.pack finalState.completedArgs,
      lineCurrentToken = T.pack (reverse finalState.currentArg),
      lineEndsWithSpace = finalState.endsWithSpace,
      lineQuoteMode = parserModeToQuoteMode finalState.mode
    }
  where
    finalState = foldl step initialState raw

    initialState =
      ParserState
        { completedArgs = [],
          currentArg = [],
          mode = Outside,
          currentTokenStarted = False,
          endsWithSpace = True
        }

    step parserState c =
      case parserState.mode of
        Outside
          | c == ' ' || c == '\t' || c == '\n' ->
              flushCurrentArg parserState
                { endsWithSpace = True
                }
          | c == '"' ->
              parserState
                { mode = InDoubleQuote,
                  currentTokenStarted = True,
                  endsWithSpace = False
                }
          | c == '\'' ->
              parserState
                { mode = InSingleQuote,
                  currentTokenStarted = True,
                  endsWithSpace = False
                }
          | c == '\\' ->
              parserState
                { mode = Escape Outside,
                  currentTokenStarted = True,
                  endsWithSpace = False
                }
          | otherwise ->
              appendChar parserState c
        InDoubleQuote
          | c == '"' ->
              parserState {mode = Outside, endsWithSpace = False}
          | c == '\\' ->
              parserState {mode = Escape InDoubleQuote, endsWithSpace = False}
          | otherwise ->
              appendChar parserState c
        InSingleQuote
          | c == '\'' ->
              parserState {mode = Outside, endsWithSpace = False}
          | c == '\\' ->
              parserState {mode = Escape InSingleQuote, endsWithSpace = False}
          | otherwise ->
              appendChar parserState c
        Escape returnMode ->
          appendChar parserState {mode = returnMode, endsWithSpace = False} c

    flushCurrentArg parserState@ParserState {completedArgs, currentArg, currentTokenStarted} =
      if null currentArg && not currentTokenStarted
        then parserState
        else
          parserState
            { completedArgs = completedArgs <> [reverse currentArg],
              currentArg = [],
              currentTokenStarted = False
            }

    appendChar parserState@ParserState {currentArg} c =
      parserState
        { currentArg = c : currentArg,
          currentTokenStarted = True,
          endsWithSpace = False
        }

parserModeToQuoteMode :: ParserMode -> QuoteMode
parserModeToQuoteMode = \case
  Outside -> QuoteNone
  InSingleQuote -> QuoteSingle
  InDoubleQuote -> QuoteDouble
  Escape baseMode -> parserModeToQuoteMode baseMode

data ParserMode
  = Outside
  | InDoubleQuote
  | InSingleQuote
  | Escape ParserMode

data ParserState = ParserState
  { completedArgs :: [String],
    currentArg :: String,
    mode :: ParserMode,
    currentTokenStarted :: Bool,
    endsWithSpace :: Bool
  }
