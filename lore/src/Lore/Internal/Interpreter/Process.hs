module Lore.Internal.Interpreter.Process
  ( installInterpreterProcessHook,
    captureInterpreterOutput,
    withInterpreterWorkingDirectory,
  )
where

import Control.DeepSeq (force)
import qualified Control.Exception as Exception
import Control.Monad.Catch (finally)
import Control.Monad.Reader (asks)
import qualified Data.List as List
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Driver.Env as GHC.Env
import qualified GHC.Driver.Hooks as GHC.Hooks
import qualified GHC.Driver.Main as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Runtime.Interpreter as GHC.Interpreter
import qualified GHC.Unit.Info as GHC.Unit
import qualified GHC.Unit.State as GHC.Unit
import Lore.Internal.Ghc.AvailInfo (availInfosNameSet)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)
import System.FilePath ((</>))
import System.IO (IOMode (AppendMode, ReadMode), openFile)
import System.Process (CreateProcess (..), StdStream (UseHandle), createProcess)
import UnliftIO (liftIO)

data InterpreterExport = InterpreterExport
  { exportPackage :: String,
    exportModule :: String,
    exportName :: String
  }

-- The external interpreter writes straight to its own stdout/stderr, so it is
-- spawned with those streams bound to a session-owned capture file. Its stdin
-- must never be Lore's stdin, which carries the MCP protocol. Its working
-- directory is the project root, which withInterpreterWorkingDirectory restores.
installInterpreterProcessHook :: SessionContext -> GHC.HscEnv -> GHC.HscEnv
installInterpreterProcessHook sessionContext hscEnv =
  hscEnv
    { GHC.Env.hsc_hooks =
        (GHC.Env.hsc_hooks hscEnv)
          { GHC.Hooks.createIservProcessHook = Just spawnInterpreter
          }
    }
  where
    spawnInterpreter processConfig = do
      inputHandle <- openFile "/dev/null" ReadMode
      outputHandle <- openFile (interpreterOutputPath sessionContext.sessionGhcWorkDir) AppendMode
      (_, _, _, processHandle) <-
        createProcess
          processConfig
            { cwd = Just sessionContext.projectRoot,
              std_in = UseHandle inputHandle,
              std_out = UseHandle outputHandle,
              std_err = UseHandle outputHandle
            }
      pure processHandle

captureInterpreterOutput :: (MonadLore m) => m a -> m (a, String)
captureInterpreterOutput action = do
  outputPath <- asks (interpreterOutputPath . sessionGhcWorkDir)
  liftIO (writeFile outputPath "")
  result <-
    (runInterpreterExport disableBuffering [] >> action)
      `finally` runInterpreterExport flushAll []
  capturedOutput <- liftIO (readFile outputPath >>= Exception.evaluate . force)
  pure (result, capturedOutput)

withInterpreterWorkingDirectory :: (MonadLore m) => FilePath -> m a -> m a
withInterpreterWorkingDirectory directory action = do
  sessionRoot <- asks projectRoot
  runInterpreterExport setCurrentDirectory [directory]
  action `finally` runInterpreterExport setCurrentDirectory [sessionRoot]

interpreterOutputPath :: FilePath -> FilePath
interpreterOutputPath ghcWorkDir =
  ghcWorkDir </> "tmp" </> "interpreter-output"

disableBuffering :: InterpreterExport
disableBuffering =
  InterpreterExport "base" "GHC.GHCi.Helpers" "disableBuffering"

flushAll :: InterpreterExport
flushAll =
  InterpreterExport "base" "GHC.GHCi.Helpers" "flushAll"

setCurrentDirectory :: InterpreterExport
setCurrentDirectory =
  InterpreterExport "directory" "System.Directory" "setCurrentDirectory"

-- Helpers are resolved from module interfaces rather than the interactive
-- scope, because the session exposes only the project's direct dependencies.
runInterpreterExport :: (MonadLore m) => InterpreterExport -> [String] -> m ()
runInterpreterExport export stringArguments = do
  name <- resolveInterpreterExport export
  let call =
        foldl
          (\function argument -> GHC.nlHsApp function (GHC.nlHsLit (GHC.mkHsString argument)))
          (GHC.nlHsVar (GHC.nameRdrName name))
          stringArguments
  compiledCall <- GHC.compileParsedExprRemote call
  interp <- GHC.Env.hscInterp <$> GHC.getSession
  liftIO (GHC.Interpreter.evalIO interp compiledCall)

resolveInterpreterExport :: (MonadLore m) => InterpreterExport -> m GHC.Name
resolveInterpreterExport InterpreterExport {exportPackage, exportModule, exportName} = do
  hscEnv <- GHC.getSession
  let moduleName = GHC.mkModuleName exportModule
      providingUnits =
        [ unitInfo
        | unitInfo <- GHC.Unit.listUnitInfo (GHC.hsc_units hscEnv),
          GHC.Unit.unitPackageNameString unitInfo == exportPackage,
          moduleName `elem` map fst (GHC.Unit.unitExposedModules unitInfo)
        ]
  case providingUnits of
    [] -> unavailable
    _ -> do
      let newestUnit = List.maximumBy (comparing GHC.Unit.unitPackageVersion) providingUnits
      iface <- liftIO (GHC.hscGetModuleInterface hscEnv (GHC.mkModule (GHC.Unit.mkUnit newestUnit) moduleName))
      case [name | name <- Set.toList (availInfosNameSet (GHC.mi_exports iface)), GHC.occNameString (GHC.nameOccName name) == exportName] of
        name : _ -> pure name
        [] -> unavailable
  where
    unavailable =
      liftIO (Exception.throwIO (userError ("Interpreter helper " <> exportPackage <> ":" <> exportModule <> "." <> exportName <> " is unavailable.")))
