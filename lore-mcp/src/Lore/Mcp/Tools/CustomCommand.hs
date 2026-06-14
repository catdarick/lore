module Lore.Mcp.Tools.CustomCommand
  ( customCommandTool,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JKM
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Mcp.Config (CustomCommandToolArgConfig (..), CustomCommandToolArgQuoteMode (..), CustomCommandToolConfig (..))
import Lore.Mcp.Internal.Tool (DynamicTool (..), SomeTool (..))
import Lore.Tools.Render.Doc (LoreDoc, paragraph)
import System.Exit (ExitCode (..))
import System.Process (readCreateProcessWithExitCode, shell)

customCommandTool :: (MonadIO m) => CustomCommandToolConfig -> SomeTool m
customCommandTool config =
  SomeDynamicTool
    DynamicTool
      { name = config.name,
        description = config.description,
        inputSchema = customCommandInputSchema config.args,
        handler = customCommandHandler config
      }

customCommandInputSchema :: [CustomCommandToolArgConfig] -> J.Value
customCommandInputSchema args =
  J.object
    [ "type" J..= ("object" :: Text),
      "properties" J..= J.Object (JKM.fromList (map argProperty args)),
      "required" J..= map (.name) args,
      "additionalProperties" J..= False
    ]
  where
    argProperty arg =
      ( JK.fromText arg.name,
        J.object $
          catMaybes
            [ Just ("type" J..= argType arg),
              ("description" J..=) <$> arg.description
            ]
      )

    argType arg =
      if arg.nullable
        then J.toJSON (["string", "null"] :: [Text])
        else J.String "string"

customCommandHandler :: (MonadIO m) => CustomCommandToolConfig -> J.Value -> m LoreDoc
customCommandHandler config rawArgs = do
  argValues <- parseCustomCommandArgs config.args rawArgs
  let commandText = substituteCommandArgs argValues config.command
  (exitCode, stdoutText, stderrText) <-
    liftIO $ readCreateProcessWithExitCode (shell (T.unpack commandText)) ""
  pure (customCommandResultDoc exitCode (T.pack stdoutText) (T.pack stderrText))

parseCustomCommandArgs :: (MonadIO m) => [CustomCommandToolArgConfig] -> J.Value -> m [(CustomCommandToolArgConfig, Maybe Text)]
parseCustomCommandArgs args = \case
  J.Object obj ->
    traverse parseArg args
    where
      parseArg arg =
        case JKM.lookup (JK.fromText arg.name) obj of
          Just (J.String value) ->
            pure (arg, Just value)
          Just J.Null
            | arg.nullable ->
                pure (arg, Nothing)
          Just J.Null ->
            liftIO $ ioError $ userError ("argument " <> T.unpack arg.name <> " must be a string")
          Just _ ->
            liftIO $ ioError $ userError ("argument " <> T.unpack arg.name <> " must be a string")
          Nothing ->
            liftIO $ ioError $ userError ("missing argument " <> T.unpack arg.name)
  _ ->
    liftIO $ ioError $ userError "custom command arguments must be an object"

substituteCommandArgs :: [(CustomCommandToolArgConfig, Maybe Text)] -> Text -> Text
substituteCommandArgs argValues commandText =
  foldl replaceArg commandText argValues
  where
    replaceArg text (arg, maybeArgValue) =
      T.replace ("@{" <> arg.name <> "}") (quoteArgValue arg (prepareArgValue arg maybeArgValue)) text

prepareArgValue :: CustomCommandToolArgConfig -> Maybe Text -> Text
prepareArgValue arg maybeArgValue =
  let value = maybe "" id maybeArgValue
   in if arg.escapeQuotes
        then escapeDoubleQuotes value
        else value

escapeDoubleQuotes :: Text -> Text
escapeDoubleQuotes =
  T.replace "\"" "\\\""

quoteArgValue :: CustomCommandToolArgConfig -> Text -> Text
quoteArgValue arg value =
  case arg.quoteMode of
    CustomCommandToolArgQuoteSingle ->
      shellSingleQuote value
    CustomCommandToolArgQuoteDouble ->
      shellDoubleQuote value
    CustomCommandToolArgQuoteNone ->
      value

shellSingleQuote :: Text -> Text
shellSingleQuote value =
  "'" <> T.replace "'" "'\\''" value <> "'"

shellDoubleQuote :: Text -> Text
shellDoubleQuote value =
  "\"" <> escapeDoubleQuotedShellChars value <> "\""

escapeDoubleQuotedShellChars :: Text -> Text
escapeDoubleQuotedShellChars =
  T.concatMap \case
    '\\' -> "\\\\"
    '"' -> "\\\""
    '$' -> "\\$"
    '`' -> "\\`"
    char -> T.singleton char

customCommandResultDoc :: ExitCode -> Text -> Text -> LoreDoc
customCommandResultDoc exitCode stdoutText stderrText =
  paragraph $ T.intercalate "\n" (filter (not . T.null) [exitStatusText exitCode, stdoutBlock, stderrBlock])
  where
    exitStatusText = \case
      ExitSuccess ->
        "exit: 0"
      ExitFailure code ->
        "exit: " <> T.pack (show code)

    stdoutBlock =
      if T.null stdoutText
        then ""
        else "stdout:\n" <> stdoutText

    stderrBlock =
      if T.null stderrText
        then ""
        else "stderr:\n" <> stderrText
