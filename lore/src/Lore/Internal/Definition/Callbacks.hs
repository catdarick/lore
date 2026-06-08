module Lore.Internal.Definition.Callbacks
  ( installDefinitionCallbacks,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Data.IOEnv as GHC.IOEnv
import qualified GHC.Driver.Env as GHC.Env
import qualified GHC.Driver.Hooks as GHC.Hooks
import qualified GHC.Driver.Plugins as GHC.Plugins
import qualified GHC.Hs as GHC.Hs
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC.Tc
import Lore.Internal.Definition.Analysis
  ( buildCoreDependenciesByBinder,
    buildMinimalTypedModuleFacts,
    buildParsedModuleFacts,
  )
import Lore.Internal.Definition.Cache.CoreModuleFacts (storeCoreModuleFactsCacheInContext)
import Lore.Internal.Definition.Cache.DefinitionModuleIndex (invalidateDefinitionModuleIndexCacheForModuleInContext)
import Lore.Internal.Definition.Cache.ParsedModuleFacts (storeParsedModuleFactsCacheInContext)
import Lore.Internal.Definition.Cache.TypedModuleFacts (lookupTypedModuleFactsCacheInContext, storeTypedModuleFactsCacheInContext)
import Lore.Internal.Definition.Types (MinimalCoreModuleFacts (..), MinimalTypedModuleFacts (..), TypedNameFacts (..))
import Lore.Internal.Session (SessionContext (..))

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
  GHC.IOEnv.liftIO do
    parsedFacts <- evaluate $ force $ buildParsedModuleFacts homeModule parsedSource
    storeParsedModuleFactsCacheInContext sessionContext homeModule parsedFacts
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
    storeTypedModuleFactsCacheInContext sessionContext homeModule typedFacts
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
    maybeTypedFacts <- lookupTypedModuleFactsCacheInContext sessionContext homeModule
    let interestingNames =
          case maybeTypedFacts of
            Just typedFacts ->
              Set.fromList typedFacts.typedNameFacts.typedDefinitionNames
            Nothing ->
              Set.empty
        (evidenceDependenciesByBinder, semanticDependenciesByBinder) =
          buildCoreDependenciesByBinder interestingNames interestingNames (GHC.mg_binds modGuts)
        coreFacts =
          MinimalCoreModuleFacts
            { coreEvidenceDependenciesByBinder =
                evidenceDependenciesByBinder,
              coreSemanticDependenciesByBinder =
                semanticDependenciesByBinder
            }
    storeCoreModuleFactsCacheInContext sessionContext homeModule coreFacts
    -- Core facts arriving after a previously built definition index would leave
    -- dependency maps stale. Drop that module index so it is rebuilt.
    invalidateDefinitionModuleIndexCacheForModuleInContext sessionContext homeModule
  pure modGuts
