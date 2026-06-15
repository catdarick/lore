module Lore.Internal.Interpreter
  ( interpreterContextIsReady,
    lookupInterpreterContextCache,
    storeInterpreterContextCache,
    invalidateInterpreterContextCache,
    refreshInterpreterContext,
    executeStatementRaw,
    getTypeOfExpressionRaw,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.DeepSeq (force)
import qualified Control.Exception as Exception
import Control.Monad.Catch (Handler (..), catches, finally)
import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import qualified GHC.Types.SourceError as GHC.SourceError
import Lore.Diagnostics (Diagnostic (..), DiagnosticClass (..), DiagnosticSpan (..), ghcMessagesToDiagnostics)
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.Cache.Types (InterpreterContextCache (..))
import Lore.Monad (MonadLore)
import System.Directory (removeFile)
import System.IO (BufferMode (NoBuffering), Handle, hClose, hFlush, hSetBuffering, openTempFile, stderr, stdout)
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (modifyMVar, readMVar, withRunInIO)

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
  executeCompiledStatement

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

executeCompiledStatement :: (MonadLore m) => Text -> m (Either [Diagnostic] String)
executeCompiledStatement source =
  withInterpretExecutionContext helperImports do
    catches
      ( do
          redirectedExecution <- runStatementWithRedirect (T.unpack source)
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

runStatementWithRedirect :: (MonadLore m) => String -> m RedirectedExecution
runStatementWithRedirect statement = do
  (executionResult, redirectedOutput) <-
    captureProcessOutput do
      result <-
        catches
          (Right <$> GHC.execStmt statement GHC.execOptions)
          [Handler (\runtimeException -> pure (Left runtimeException))]
      _ <- GHC.execStmt "System.IO.hFlush System.IO.stdout" GHC.execOptions
      _ <- GHC.execStmt "System.IO.hFlush System.IO.stderr" GHC.execOptions
      pure result
  pure
    RedirectedExecution
      { redirectedExecResult = executionResult,
        redirectedOutput
      }

withInterpretExecutionContext :: (MonadLore m) => [GHC.InteractiveImport] -> m a -> m a
withInterpretExecutionContext extraImports action = do
  originalContext <- GHC.getContext
  GHC.setContext (extraImports <> originalContext)
  action `finally` GHC.setContext originalContext

helperImports :: [GHC.InteractiveImport]
helperImports =
  map
    qualifiedImport
    [ "GHC.IO.Handle",
      "System.IO",
      "Data.IORef",
      "System.IO.Unsafe"
    ]

qualifiedImport :: String -> GHC.InteractiveImport
qualifiedImport moduleName =
  GHC.IIDecl $
    (GHC.simpleImportDecl (GHC.mkModuleName moduleName))
      { GHC.ideclQualified = GHC.QualifiedPre
      }

data SavedProcessHandles = SavedProcessHandles
  { savedStdout :: Handle,
    savedStderr :: Handle
  }

-- The internal interpreter runs in this process, so stdout/stderr redirection
-- is process-wide. Serialize captures across every Lore session.
{-# NOINLINE processOutputCaptureLock #-}
processOutputCaptureLock :: MVar ()
processOutputCaptureLock = unsafePerformIO (newMVar ())

captureProcessOutput :: (MonadLore m) => m a -> m (a, String)
captureProcessOutput action =
  withRunInIO $ \runInIO ->
    withMVar processOutputCaptureLock $ \_ ->
      Exception.bracket
        createCaptureFile
        cleanupCaptureFile
        ( \(capturePath, captureHandle) -> do
            hSetBuffering captureHandle NoBuffering
            result <-
              Exception.bracket
                (redirectProcessOutput captureHandle)
                restoreProcessOutput
                (const (runInIO action))
            hClose captureHandle
            capturedOutput <- readTrimmedCaptureFile capturePath
            pure (result, capturedOutput)
        )

createCaptureFile :: IO (FilePath, Handle)
createCaptureFile =
  openTempFile "/tmp" "lore-interpreter-output"

cleanupCaptureFile :: (FilePath, Handle) -> IO ()
cleanupCaptureFile (capturePath, captureHandle) = do
  ignoreIOException (hClose captureHandle)
  ignoreIOException (removeFile capturePath)

redirectProcessOutput :: Handle -> IO SavedProcessHandles
redirectProcessOutput captureHandle =
  Exception.mask_ do
    hFlush stdout
    hFlush stderr
    savedStdout <- hDuplicate stdout
    savedStderr <- hDuplicate stderr `Exception.onException` hClose savedStdout
    let savedHandles = SavedProcessHandles {savedStdout, savedStderr}
    ( do
        hDuplicateTo captureHandle stdout
        hDuplicateTo captureHandle stderr
      )
      `Exception.onException` restoreProcessOutput savedHandles
    pure savedHandles

restoreProcessOutput :: SavedProcessHandles -> IO ()
restoreProcessOutput SavedProcessHandles {savedStdout, savedStderr} =
  Exception.mask_ do
    cleanupErrors <-
      mapM
        tryCleanup
        [ hFlush stdout,
          hFlush stderr,
          hDuplicateTo savedStdout stdout,
          hDuplicateTo savedStderr stderr,
          hClose savedStdout,
          hClose savedStderr
        ]
    case [cleanupError | Just cleanupError <- cleanupErrors] of
      [] -> pure ()
      firstError : _ -> Exception.throwIO firstError

tryCleanup :: IO () -> IO (Maybe Exception.SomeException)
tryCleanup cleanupAction =
  (cleanupAction >> pure Nothing)
    `Exception.catch` (pure . Just)

ignoreIOException :: IO () -> IO ()
ignoreIOException action =
  catchIOError action (const (pure ()))

readTrimmedCaptureFile :: FilePath -> IO String
readTrimmedCaptureFile capturePath = do
  capturedOutput <- readFile capturePath
  Exception.evaluate (force (trimTrailingNewlines capturedOutput))

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
