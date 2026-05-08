module Lore.Internal.Definition.Callbacks
  ( installDefinitionCallbacks,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Data.IOEnv as GHC.IOEnv
import qualified GHC.Driver.Env as GHC.Env
import qualified GHC.Driver.Hooks as GHC.Hooks
import qualified GHC.Driver.Plugins as GHC.Plugins
import qualified GHC.Hs as GHC.Hs
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC.Tc
import Lore.Internal.Definition.Analysis (buildMinimalTypedModuleFacts, buildParsedModuleFacts, buildUsedInstancesByBinder)
import Lore.Internal.Definition.Types (MinimalCoreModuleFacts (..), MinimalTypedModuleFacts (..), ParsedModuleCache (..), TypedModuleCache (..))
import Lore.Internal.Session (SessionContext (..))
import UnliftIO (modifyMVar_, readMVar)

installDefinitionCallbacks :: SessionContext -> GHC.HscEnv -> GHC.HscEnv
installDefinitionCallbacks sessionContext hscEnv =
  hscEnv
    { GHC.Env.hsc_hooks = hooks,
      GHC.Env.hsc_plugins = plugins
    }
  where
    hooks =
      (GHC.Env.hsc_hooks hscEnv)
        { GHC.Hooks.hscFrontendHook = Nothing
        }

    plugins =
      let pluginWithArgs =
            GHC.Plugins.PluginWithArgs
              { GHC.Plugins.paPlugin = definitionPlugin sessionContext,
                GHC.Plugins.paArguments = []
              }
       in (GHC.Env.hsc_plugins hscEnv)
            { GHC.Plugins.staticPlugins =
                GHC.Plugins.StaticPlugin {GHC.Plugins.spPlugin = pluginWithArgs}
                  : GHC.Plugins.staticPlugins (GHC.Env.hsc_plugins hscEnv)
            }

definitionPlugin :: SessionContext -> GHC.Plugins.Plugin
definitionPlugin sessionContext =
  GHC.Plugins.defaultPlugin
    { GHC.Plugins.parsedResultAction = parsedResultAction sessionContext,
      GHC.Plugins.typeCheckResultAction = typeCheckResultAction sessionContext,
      GHC.Plugins.installCoreToDos = installCoreToDos sessionContext,
      GHC.Plugins.pluginRecompile = GHC.Plugins.purePlugin
    }

parsedResultAction ::
  SessionContext ->
  [GHC.Plugins.CommandLineOption] ->
  GHC.ModSummary ->
  GHC.Plugins.ParsedResult ->
  GHC.Hsc GHC.Plugins.ParsedResult
parsedResultAction sessionContext _ summary parsedResult = do
  let parsedSource =
        GHC.Hs.hpm_module (GHC.Plugins.parsedResultModule parsedResult)
      homeModule = GHC.ms_mod summary
  liftIO do
    parsedFacts <- evaluate $ force $ buildParsedModuleFacts homeModule parsedSource
    modifyMVar_ (referenceParsedModuleCache sessionContext) \cache ->
      let cache' = Map.insert homeModule (ParsedModuleFactsCache parsedFacts) cache
       in evaluate cache'
  pure parsedResult

typeCheckResultAction ::
  SessionContext ->
  [GHC.Plugins.CommandLineOption] ->
  GHC.ModSummary ->
  GHC.Tc.TcGblEnv ->
  GHC.Tc.TcM GHC.Tc.TcGblEnv
typeCheckResultAction sessionContext _ summary tcg = do
  let homeModule = GHC.ms_mod summary
  GHC.IOEnv.liftIO do
    typedFacts <- evaluate $ force $ buildMinimalTypedModuleFacts homeModule tcg
    modifyMVar_ (referenceTypedModuleCache sessionContext) \cache ->
      let cache' = Map.insert homeModule (TypedModuleMinimalFacts typedFacts) cache
       in evaluate cache'
  pure tcg

installCoreToDos ::
  SessionContext ->
  [GHC.Plugins.CommandLineOption] ->
  [GHC.CoreToDo] ->
  GHC.CoreM [GHC.CoreToDo]
installCoreToDos sessionContext _ todos =
  pure (GHC.CoreDoPluginPass "lore-raw-artifacts-tc-core-processed" (rawArtifactsTcCoreProcessedCorePass sessionContext) : todos)

rawArtifactsTcCoreProcessedCorePass :: SessionContext -> GHC.ModGuts -> GHC.CoreM GHC.ModGuts
rawArtifactsTcCoreProcessedCorePass sessionContext modGuts = do
  let homeModule = GHC.mg_module modGuts
  GHC.IOEnv.liftIO do
    typedCache <- readMVar (referenceTypedModuleCache sessionContext)
    let interestingBinders =
          case Map.lookup homeModule typedCache of
            Just (TypedModuleMinimalFacts typedFacts) ->
              Set.fromList typedFacts.typedDefinitionNames
            Nothing ->
              Set.empty
        coreFacts =
          MinimalCoreModuleFacts
            { coreUsedInstancesByBinder =
                buildUsedInstancesByBinder interestingBinders (GHC.mg_binds modGuts)
            }
    modifyMVar_ (referenceMinimalCoreModuleFactsCache sessionContext) \cache ->
      let cache' = Map.insert homeModule coreFacts cache
       in evaluate cache'
    -- Core facts arriving after a previously built definition index would leave
    -- dependencyUsedInstanceNames stale. Drop that module index so it is rebuilt.
    modifyMVar_ (definitionModuleIndexCache sessionContext) \cache ->
      let cache' = Map.delete homeModule cache
       in evaluate cache'
  pure modGuts
