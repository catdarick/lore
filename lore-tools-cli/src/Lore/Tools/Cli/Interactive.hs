module Lore.Tools.Cli.Interactive
  ( runInteractive,
    interactiveSettings,
  )
where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Lore.Session (SessionContext (..))
import Lore.Tools.Cli.Internal.Completion (completeLoreLine)
import Lore.Tools.Cli.Internal.Help
  ( findToolByNameOrAlias,
    renderGeneralHelp,
    renderToolHelp,
  )
import Lore.Tools.Cli.Internal.Parser (OutputFormat, parseCliWords)
import Lore.Tools.Cli.Internal.ShellWords (shellWords)
import Lore.Tools.Cli.Internal.Tool
  ( CliInvocation,
    CliInvocationResult (..),
    SomeCliTool,
    cliInvocationName,
    runCliInvocation,
  )
import Lore.Tools.Cli.Render
  ( FileWriteMode (..),
    renderOutput,
    writeOutputToFile,
  )
import Lore.Tools.Render.Doc (LoreDoc)
import System.Console.Haskeline
import System.FilePath ((</>))

runInteractive :: (MonadIO m, MonadMask m, MonadReader SessionContext m) => [SomeCliTool m] -> OutputFormat -> m ()
runInteractive tools format = do
  workDir <- asks (.sessionGhcWorkDir)
  runInputT (interactiveSettings tools workDir) (loop TeeDisabled)
  where
    loop teeState = do
      maybeLine <- getInputLine "lore> "
      case maybeLine of
        Nothing -> pure ()
        Just line -> do
          let trimmed = T.strip (T.pack line)
          if T.null trimmed
            then loop teeState
            else do
              decision <- handleLine tools format teeState trimmed
              case decision of
                LoopStop -> pure ()
                LoopContinue nextTeeState -> loop nextTeeState

interactiveSettings :: (MonadIO m) => [SomeCliTool m] -> FilePath -> Settings m
interactiveSettings tools workDir =
  Settings
    { complete = completeLoreLine tools,
      historyFile = Just (workDir </> "lore-cli-history"),
      autoAddHistory = True
    }

handleLine :: (MonadIO m) => [SomeCliTool m] -> OutputFormat -> TeeState -> T.Text -> InputT m LoopDecision
handleLine tools format teeState trimmed
  | ":" `T.isPrefixOf` trimmed =
      handleMetaCommand tools format teeState (T.drop 1 trimmed)
  | otherwise =
      handleRegularInput tools format teeState trimmed

handleRegularInput :: (MonadIO m) => [SomeCliTool m] -> OutputFormat -> TeeState -> T.Text -> InputT m LoopDecision
handleRegularInput tools format teeState trimmed =
  case shellWords (T.unpack trimmed) of
    [] -> pure (LoopContinue teeState)
    wordsToParse ->
      case wordsToParse of
        ["help"] -> do
          outputStrLn (T.unpack (renderGeneralHelp tools))
          pure (LoopContinue teeState)
        ["?"] -> do
          outputStrLn (T.unpack (renderGeneralHelp tools))
          pure (LoopContinue teeState)
        ["help", commandName] -> do
          renderInteractiveToolHelp tools (T.pack commandName)
          pure (LoopContinue teeState)
        ["?", commandName] -> do
          renderInteractiveToolHelp tools (T.pack commandName)
          pure (LoopContinue teeState)
        _ -> do
          executeInvocationWords tools format teeState wordsToParse
          pure (LoopContinue teeState)

handleMetaCommand :: (MonadIO m) => [SomeCliTool m] -> OutputFormat -> TeeState -> T.Text -> InputT m LoopDecision
handleMetaCommand tools format teeState rawMeta =
  case shellWords (T.unpack (T.strip rawMeta)) of
    [] -> pure (LoopContinue teeState)
    ["quit"] -> pure LoopStop
    ["q"] -> pure LoopStop
    ["help"] -> do
      outputStrLn (T.unpack (renderInteractiveHelp tools teeState))
      pure (LoopContinue teeState)
    "help" : [commandName] -> do
      renderInteractiveToolHelp tools (T.pack commandName)
      pure (LoopContinue teeState)
    "write" : writeArgs -> do
      runWriteCommand tools format writeArgs
      pure (LoopContinue teeState)
    "tee" : teeArgs ->
      handleTeeCommand teeState teeArgs
    _ -> do
      outputStrLn ("Unknown meta command: :" <> T.unpack rawMeta)
      pure (LoopContinue teeState)

executeInvocationWords :: (MonadIO m) => [SomeCliTool m] -> OutputFormat -> TeeState -> [String] -> InputT m ()
executeInvocationWords tools format teeState wordsToParse =
  case parseCliWords tools wordsToParse of
    Left parseError ->
      outputStrLn (trimTrailingNewline parseError)
    Right invocation ->
      runInvocationWithOutput format teeState invocation

runWriteCommand :: (MonadIO m) => [SomeCliTool m] -> OutputFormat -> [String] -> InputT m ()
runWriteCommand tools format rawArgs =
  case parseWriteCommand rawArgs of
    Left err ->
      outputStrLn err
    Right writeCommand ->
      case parseCliWords tools writeCommand.writeCommandWords of
        Left parseError ->
          outputStrLn (trimTrailingNewline parseError)
        Right invocation -> do
          (commandName, loreDoc) <- materializeInvocation invocation
          let mode = if writeCommand.writeAppend then WriteAppend else WriteTruncate
          lift (writeOutputToFile mode writeCommand.writePath format commandName loreDoc)
          outputStrLn
            ( if writeCommand.writeAppend
                then "Appended output to " <> writeCommand.writePath
                else "Wrote output to " <> writeCommand.writePath
            )

runInvocationWithOutput :: (MonadIO m) => OutputFormat -> TeeState -> CliInvocation m -> InputT m ()
runInvocationWithOutput format teeState invocation = do
  (commandName, loreDoc) <- materializeInvocation invocation
  lift (renderOutput format commandName loreDoc)
  case teeState of
    TeeDisabled -> pure ()
    TeeEnabled teePath ->
      lift (writeOutputToFile WriteAppend teePath format commandName loreDoc)

materializeInvocation :: (MonadIO m) => CliInvocation m -> InputT m (T.Text, LoreDoc)
materializeInvocation invocation = do
  invocationResult <- lift (runCliInvocation invocation)
  pure (cliInvocationName invocation, invocationResult.cliInvocationResultDoc)

handleTeeCommand :: (MonadIO m) => TeeState -> [String] -> InputT m LoopDecision
handleTeeCommand teeState rawArgs =
  case rawArgs of
    [] -> do
      outputStrLn (T.unpack (renderTeeStatus teeState))
      pure (LoopContinue teeState)
    ["off"] -> do
      outputStrLn "Tee disabled."
      pure (LoopContinue TeeDisabled)
    ["disable"] -> do
      outputStrLn "Tee disabled."
      pure (LoopContinue TeeDisabled)
    [path] -> do
      outputStrLn ("Tee enabled. Appending command outputs to " <> path)
      pure (LoopContinue (TeeEnabled path))
    _ -> do
      outputStrLn "Usage: :tee [off|disable|PATH]"
      pure (LoopContinue teeState)

renderInteractiveHelp :: [SomeCliTool m] -> TeeState -> T.Text
renderInteractiveHelp tools teeState =
  T.unlines
    [ renderGeneralHelp tools,
      "",
      "Output forwarding:",
      "  :write PATH COMMAND [ARGS...]",
      "  :write --append PATH COMMAND [ARGS...]",
      "  :tee PATH",
      "  :tee off",
      "  :tee",
      "",
      "Current tee status: " <> renderTeeStatus teeState
    ]

renderTeeStatus :: TeeState -> T.Text
renderTeeStatus = \case
  TeeDisabled -> "disabled"
  TeeEnabled path -> "enabled (" <> T.pack path <> ")"

data WriteCommand = WriteCommand
  { writeAppend :: Bool,
    writePath :: FilePath,
    writeCommandWords :: [String]
  }

parseWriteCommand :: [String] -> Either String WriteCommand
parseWriteCommand rawArgs =
  case consumeFlags False rawArgs of
    (appendMode, path : commandWords@(_ : _)) ->
      Right
        WriteCommand
          { writeAppend = appendMode,
            writePath = path,
            writeCommandWords = commandWords
          }
    (_, [_path]) ->
      Left "Usage: :write [--append|-a] PATH COMMAND [ARGS...]"
    _ ->
      Left "Usage: :write [--append|-a] PATH COMMAND [ARGS...]"
  where
    consumeFlags appendMode = \case
      "--append" : rest -> consumeFlags True rest
      "-a" : rest -> consumeFlags True rest
      remaining -> (appendMode, remaining)

data TeeState
  = TeeDisabled
  | TeeEnabled FilePath

data LoopDecision
  = LoopContinue TeeState
  | LoopStop

renderInteractiveToolHelp :: (MonadIO m) => [SomeCliTool m] -> T.Text -> InputT m ()
renderInteractiveToolHelp tools commandName =
  case findToolByNameOrAlias commandName tools of
    Nothing ->
      outputStrLn ("Unknown command: " <> T.unpack commandName)
    Just someCliTool ->
      outputStrLn (T.unpack (renderToolHelp someCliTool))

trimTrailingNewline :: String -> String
trimTrailingNewline = reverse . dropWhile (== '\n') . reverse
