module Lore.Internal.Interpreter
  ( interpreterContextIsReady,
    lookupInterpreterContextCache,
    storeInterpreterContextCache,
    invalidateInterpreterContextCache,
    refreshInterpreterContext,
    executeStatementRaw,
    executeStatementRawInDirectory,
    getTypeOfExpressionRaw,
  )
where

import qualified Control.Exception as Exception
import Control.Monad.Catch (Handler (..), catches, finally)
import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Driver.Session as GHC.Session
import qualified GHC.Types.SourceError as GHC.SourceError
import Lore.Diagnostics (Diagnostic (..), DiagnosticClass (..), DiagnosticSpan (..), ghcMessagesToDiagnostics)
import Lore.Internal.Interpreter.Process (captureInterpreterOutput, withInterpreterWorkingDirectory)
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.Cache.Types (InterpreterContextCache (..))
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, readMVar)

data RedirectedExecution = RedirectedExecution
  { redirectedExecResult :: Either Exception.SomeException GHC.ExecResult,
    redirectedOutput :: String
  }

lookupInterpreterContextCache :: (MonadLore m) => m (Maybe [GHC.ModuleName])
lookupInterpreterContextCache = do
  cacheVar <- asks interpreterContextCacheVar
  InterpreterContextCache maybeLoadedModuleNames <- readMVar cacheVar
  pure maybeLoadedModuleNames

storeInterpreterContextCache :: (MonadLore m) => [GHC.ModuleName] -> m ()
storeInterpreterContextCache loadedModuleNames = do
  cacheVar <- asks interpreterContextCacheVar
  modifyMVar cacheVar $ \_ -> pure (InterpreterContextCache (Just loadedModuleNames), ())

invalidateInterpreterContextCache :: (MonadLore m) => m ()
invalidateInterpreterContextCache = do
  cacheVar <- asks interpreterContextCacheVar
  modifyMVar cacheVar $ \_ -> pure (InterpreterContextCache Nothing, ())

interpreterContextIsReady :: (MonadLore m) => m Bool
interpreterContextIsReady =
  maybe False (const True) <$> lookupInterpreterContextCache

refreshInterpreterContext :: (MonadLore m) => m ()
refreshInterpreterContext = do
  maybeCustomPrelude <- asks customPrelude
  ModSummaries modSummaries <- getCachedModSummaries
  loadedModuleNames <- Set.toAscList . Set.fromList <$> mapMMaybe loadedHomeModuleName (Map.elems modSummaries)

  let preludeName = maybe "Prelude" T.unpack maybeCustomPrelude
      preludeIsHomeModule = any (\summary -> GHC.moduleNameString (GHC.moduleName (GHC.ms_mod summary)) == preludeName) (Map.elems modSummaries)
      preludeSuccessfullyLoaded = GHC.mkModuleName preludeName `elem` loadedModuleNames

      -- Only explicitly import the prelude if it's an external module,
      -- or if it's a home module that successfully loaded (though in the latter case it's mostly redundant).
      shouldAddPrelude = not preludeIsHomeModule || preludeSuccessfullyLoaded
      preludeContext = if shouldAddPrelude then [importModule (GHC.mkModuleName preludeName)] else []

  catches
    (GHC.setContext (preludeContext <> map importModule loadedModuleNames))
    [Handler \(_ :: GHC.SourceError.SourceError) -> pure ()]

  storeInterpreterContextCache loadedModuleNames
  where
    importModule =
      GHC.IIDecl . GHC.simpleImportDecl

    loadedHomeModuleName summary = do
      maybeInfo <- GHC.getModuleInfo (GHC.ms_mod summary)
      pure $
        case maybeInfo of
          Just _ -> Just (GHC.moduleName (GHC.ms_mod summary))
          Nothing -> Nothing

executeStatementRaw :: (MonadLore m) => Text -> m (Either [Diagnostic] String)
executeStatementRaw =
  executeCompiledStatement Nothing

executeStatementRawInDirectory :: (MonadLore m) => FilePath -> Text -> m (Either [Diagnostic] String)
executeStatementRawInDirectory directory =
  executeCompiledStatement (Just directory)

getTypeOfExpressionRaw :: (MonadLore m) => Text -> m GHC.Type
getTypeOfExpressionRaw source = do
  GHC.exprType GHC.TM_Inst (T.unpack source)

mapMMaybe :: (Applicative m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMMaybe f =
  fmap foldMaybes . traverse f
  where
    foldMaybes =
      foldr
        (\item acc -> maybe acc (: acc) item)
        []

executeCompiledStatement :: (MonadLore m) => Maybe FilePath -> Text -> m (Either [Diagnostic] String)
executeCompiledStatement maybeDirectory source =
  withInterpreterWarningsAllowed do
    catches
      ( do
          redirectedExecution <- runStatementWithRedirect maybeDirectory (T.unpack source)
          case redirectedExecResult redirectedExecution of
            Left runtimeException ->
              pure (Left [runtimeExceptionDiagnostic (Just redirectedExecution.redirectedOutput) runtimeException])
            Right executionResult ->
              case executionResult of
                GHC.ExecComplete {GHC.execResult = Left runtimeException} ->
                  pure (Left [runtimeExceptionDiagnostic (Just redirectedExecution.redirectedOutput) runtimeException])
                GHC.ExecBreak {} ->
                  pure (Left [unexpectedInterpreterResultDiagnostic "ExecBreak"])
                GHC.ExecComplete {} ->
                  pure (Right redirectedExecution.redirectedOutput)
      )
      [ Handler \sourceError ->
          pure (Left (ghcMessagesToDiagnostics (GHC.SourceError.srcErrorMessages sourceError))),
        Handler (pure . Left . pure . runtimeExceptionDiagnostic Nothing)
      ]

withInterpreterWarningsAllowed :: (MonadLore m) => m a -> m a
withInterpreterWarningsAllowed action = do
  originalDynFlags <- GHC.getSessionDynFlags
  logger <- GHC.getLogger
  (parsedDynFlags, _leftoverArgs, _warnings) <-
    GHC.parseDynamicFlags logger originalDynFlags [GHC.noLoc "-Wwarn"]
  let interpreterDynFlags = GHC.Session.gopt_unset parsedDynFlags GHC.Session.Opt_WarnIsError
  GHC.setSessionDynFlags interpreterDynFlags
  action `finally` GHC.setSessionDynFlags originalDynFlags

runStatementWithRedirect :: (MonadLore m) => Maybe FilePath -> String -> m RedirectedExecution
runStatementWithRedirect maybeDirectory statement = do
  (executionResult, capturedOutput) <-
    captureInterpreterOutput $
      withExecutionDirectory maybeDirectory $
        catches
          (Right <$> GHC.execStmt statement GHC.execOptions)
          [Handler (\runtimeException -> pure (Left runtimeException))]
  pure
    RedirectedExecution
      { redirectedExecResult = executionResult,
        redirectedOutput = trimTrailingNewlines capturedOutput
      }

withExecutionDirectory :: (MonadLore m) => Maybe FilePath -> m a -> m a
withExecutionDirectory maybeDirectory action =
  case maybeDirectory of
    Nothing -> action
    Just directory -> withInterpreterWorkingDirectory directory action

trimTrailingNewlines :: String -> String
trimTrailingNewlines =
  reverse . dropWhile (`elem` ['\n', '\r']) . reverse

runtimeExceptionDiagnostic :: Maybe String -> Exception.SomeException -> Diagnostic
runtimeExceptionDiagnostic maybeCapturedOutput runtimeException =
  Diagnostic
    { diagnosticClass = DiagInteractive,
      diagnosticSeverity = Just GHC.SevError,
      diagnosticReason = Nothing,
      diagnosticWarningFlag = Nothing,
      diagnosticCode = Nothing,
      diagnosticSpan = UnhelpfulDiagnosticSpan "executeStatement",
      diagnosticMessage = T.pack (show runtimeException),
      diagnosticHints = capturedOutputHints
    }
  where
    capturedOutputHints =
      case maybeCapturedOutput of
        Just capturedOutput
          | not (null capturedOutput) ->
              ["Captured output: " <> T.pack capturedOutput]
        _ ->
          []

unexpectedInterpreterResultDiagnostic :: Text -> Diagnostic
unexpectedInterpreterResultDiagnostic expectedType =
  Diagnostic
    { diagnosticClass = DiagInteractive,
      diagnosticSeverity = Just GHC.SevError,
      diagnosticReason = Nothing,
      diagnosticWarningFlag = Nothing,
      diagnosticCode = Nothing,
      diagnosticSpan = UnhelpfulDiagnosticSpan "executeStatement",
      diagnosticMessage = "Internal interpreter error: expected statement execution result of type " <> expectedType <> ".",
      diagnosticHints = []
    }
