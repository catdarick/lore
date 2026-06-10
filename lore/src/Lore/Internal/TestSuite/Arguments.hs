module Lore.Internal.TestSuite.Arguments
  ( TestArgumentsParseError (..),
    parseTestArguments,
    renderTestArgumentsParseError,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

data TestArgumentsParseError
  = UnterminatedSingleQuote
  | UnterminatedDoubleQuote
  | TrailingEscape
  deriving stock (Eq, Show)

parseTestArguments :: Text -> Either TestArgumentsParseError [String]
parseTestArguments raw =
  finish (T.foldl' step initialParserState raw)
  where
    initialParserState =
      ParserState
        { completedArgs = [],
          currentArg = [],
          currentArgStarted = False,
          mode = Outside
        }

    step parserState c =
      case parserState.mode of
        Outside
          | isArgumentSeparator c ->
              flushCurrentArg parserState
          | c == '"' ->
              parserState {currentArgStarted = True, mode = InDoubleQuote}
          | c == '\'' ->
              parserState {currentArgStarted = True, mode = InSingleQuote}
          | c == '\\' ->
              parserState {currentArgStarted = True, mode = Escape Outside}
          | otherwise ->
              appendChar parserState c
        InDoubleQuote
          | c == '"' ->
              parserState {mode = Outside}
          | c == '\\' ->
              parserState {mode = Escape InDoubleQuote}
          | otherwise ->
              appendChar parserState c
        InSingleQuote
          | c == '\'' ->
              parserState {mode = Outside}
          | c == '\\' ->
              parserState {mode = Escape InSingleQuote}
          | otherwise ->
              appendChar parserState c
        Escape returnMode ->
          appendChar parserState {mode = returnMode} c

    finish parserState =
      case parserState.mode of
        Outside ->
          Right (reverse (emitCurrentArg parserState.completedArgs parserState.currentArgStarted parserState.currentArg))
        InSingleQuote ->
          Left UnterminatedSingleQuote
        InDoubleQuote ->
          Left UnterminatedDoubleQuote
        Escape _ ->
          Left TrailingEscape

    isArgumentSeparator c =
      c == ' ' || c == '\t' || c == '\n'

    flushCurrentArg parserState@ParserState {completedArgs, currentArgStarted, currentArg} =
      parserState
        { completedArgs = emitCurrentArg completedArgs currentArgStarted currentArg,
          currentArg = [],
          currentArgStarted = False
        }

    appendChar parserState@ParserState {currentArg} c =
      parserState {currentArg = c : currentArg, currentArgStarted = True}

    emitCurrentArg args argStarted current =
      if argStarted
        then reverse current : args
        else args

renderTestArgumentsParseError :: TestArgumentsParseError -> Text
renderTestArgumentsParseError = \case
  UnterminatedSingleQuote ->
    "unterminated single-quoted argument"
  UnterminatedDoubleQuote ->
    "unterminated double-quoted argument"
  TrailingEscape ->
    "trailing escape character"

data ParserMode
  = Outside
  | InDoubleQuote
  | InSingleQuote
  | Escape ParserMode

data ParserState = ParserState
  { completedArgs :: [String],
    currentArg :: String,
    currentArgStarted :: Bool,
    mode :: ParserMode
  }
