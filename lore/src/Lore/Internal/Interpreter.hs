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

import Control.DeepSeq (force)
import qualified Control.Exception as Exception
import Control.Monad.Catch (Handler (..), catches, finally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Types.SourceError as GHC.SourceError
import Lore.Diagnostics (Diagnostic (..), DiagnosticClass (..), DiagnosticSpan (..), ghcMessagesToDiagnostics)
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.Cache.Types (InterpreterContextCache (..))
import Lore.Monad (MonadLore)
import System.Directory (removeFile)
import System.IO (hClose, openTempFile)
import System.IO.Error (catchIOError)
import UnliftIO (modifyMVar, readMVar)

data RedirectedExecution = RedirectedExecution
  { redirectedExecResult :: GHC.ExecResult,
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
            GHC.ExecComplete {GHC.execResult = Left runtimeException} ->
              pure (Left [runtimeExceptionDiagnostic runtimeException])
            GHC.ExecBreak {} ->
              pure (Left [unexpectedInterpreterResultDiagnostic "ExecBreak"])
            GHC.ExecComplete {} ->
              Right <$> forceExecutionOutput redirectedExecution.redirectedOutput
      )
      [ Handler \sourceError ->
          pure (Left (ghcMessagesToDiagnostics (GHC.SourceError.srcErrorMessages sourceError))),
        Handler (pure . Left . pure . runtimeExceptionDiagnostic)
      ]

runStatementWithRedirect :: (MonadLore m) => String -> m RedirectedExecution
runStatementWithRedirect statement =
  withTemporaryCaptureFile \capturePath -> do
    _ <- GHC.execStmt renderCaptureRestoreRefBinding GHC.execOptions
    _ <- GHC.execStmt renderStdoutRestoreStatement GHC.execOptions
    _ <- GHC.execStmt (renderStdoutRedirectStatement capturePath) GHC.execOptions
    ( do
        executionResult <- GHC.execStmt statement GHC.execOptions
        _ <- GHC.execStmt "System.IO.hFlush System.IO.stdout" GHC.execOptions
        redirectedOutput <- liftIO (readTrimmedCaptureFile capturePath)
        pure
          RedirectedExecution
            { redirectedExecResult = executionResult,
              redirectedOutput
            }
      )
      `finally` GHC.execStmt renderStdoutRestoreStatement GHC.execOptions

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

withTemporaryCaptureFile :: (MonadLore m) => (FilePath -> m a) -> m a
withTemporaryCaptureFile action = do
  (capturePath, captureHandle) <- liftIO $ openTempFile "/tmp" "lore-interpreter-stdout"
  liftIO $ hClose captureHandle
  action capturePath `finally` liftIO (catchIOError (removeFile capturePath) (const (pure ())))

renderStdoutRedirectStatement :: FilePath -> String
renderStdoutRedirectStatement capturePath =
  T.unpack $
    T.unlines
      [ "do",
        "  __lore_saved_stdout_handle <- GHC.IO.Handle.hDuplicate System.IO.stdout",
        "  Data.IORef.writeIORef __lore_stdout_restore_ref (Just __lore_saved_stdout_handle)",
        "  __lore_stdout_handle <- System.IO.openFile " <> renderedCapturePath <> " System.IO.WriteMode",
        "  System.IO.hSetBuffering __lore_stdout_handle System.IO.NoBuffering",
        "  GHC.IO.Handle.hDuplicateTo __lore_stdout_handle System.IO.stdout",
        "  System.IO.hClose __lore_stdout_handle"
      ]
  where
    renderedCapturePath =
      T.pack (show capturePath)

renderCaptureRestoreRefBinding :: String
renderCaptureRestoreRefBinding =
  T.unpack $
    T.unlines
      [ "let __lore_stdout_restore_ref =",
        "      (System.IO.Unsafe.unsafePerformIO (Data.IORef.newIORef Nothing) :: Data.IORef.IORef (Maybe System.IO.Handle))"
      ]

renderStdoutRestoreStatement :: String
renderStdoutRestoreStatement =
  T.unpack $
    T.unlines
      [ "do",
        "  __lore_saved_stdout_handle <- Data.IORef.readIORef __lore_stdout_restore_ref",
        "  case __lore_saved_stdout_handle of",
        "    Just __lore_handle -> do",
        "      GHC.IO.Handle.hDuplicateTo __lore_handle System.IO.stdout",
        "      System.IO.hClose __lore_handle",
        "      Data.IORef.writeIORef __lore_stdout_restore_ref Nothing",
        "    Nothing -> pure ()"
      ]

readTrimmedCaptureFile :: FilePath -> IO String
readTrimmedCaptureFile capturePath =
  trimTrailingNewlines <$> readFile capturePath

trimTrailingNewlines :: String -> String
trimTrailingNewlines =
  reverse . dropWhile (`elem` ['\n', '\r']) . reverse

forceExecutionOutput :: (MonadLore m) => String -> m String
forceExecutionOutput renderedOutput =
  liftIO do
    Exception.evaluate (force renderedOutput)

runtimeExceptionDiagnostic :: Exception.SomeException -> Diagnostic
runtimeExceptionDiagnostic runtimeException =
  Diagnostic
    { diagnosticClass = DiagInteractive,
      diagnosticSeverity = Just GHC.SevError,
      diagnosticReason = Nothing,
      diagnosticCode = Nothing,
      diagnosticSpan = UnhelpfulDiagnosticSpan "executeStatement",
      diagnosticMessage = T.pack (show runtimeException),
      diagnosticHints = []
    }

unexpectedInterpreterResultDiagnostic :: Text -> Diagnostic
unexpectedInterpreterResultDiagnostic expectedType =
  Diagnostic
    { diagnosticClass = DiagInteractive,
      diagnosticSeverity = Just GHC.SevError,
      diagnosticReason = Nothing,
      diagnosticCode = Nothing,
      diagnosticSpan = UnhelpfulDiagnosticSpan "executeStatement",
      diagnosticMessage = "Internal interpreter error: expected statement execution result of type " <> expectedType <> ".",
      diagnosticHints = []
    }
