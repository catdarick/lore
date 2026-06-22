module Lore.Internal.Session.Cache.Registry
  ( SessionCacheVars (..),
    SessionCacheResetAction (..),
    emptyCoreModuleFactsCache,
    emptyDefinitionModuleIndexCache,
    emptyExternalSymbolsEnvironmentKeyCache,
    emptyExternalSymbolsIndexCache,
    emptyGeneratedMainModulesRegistry,
    emptyHomeSymbolsIndexCache,
    emptyInstanceEnvironmentInputsCache,
    emptyInterpreterContextCache,
    emptyLastLoadHomeModulesResultCache,
    emptyModSummariesCache,
    emptyNameToInstancesIndexCache,
    emptyParsedModuleFactsCache,
    emptyParsedOccurrenceModuleIndexCache,
    emptySymbolSearchIndexCache,
    emptyTemporalModulesRegistry,
    emptyTypedModuleFactsCache,
    newSessionCacheVars,
    sessionCacheResetActions,
    setCacheVarStrict,
  )
where

import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.MVar (MVar)
import Lore.Internal.Definition.Cache.Types
  ( CoreModuleFactsCache,
    DefinitionModuleIndexCache (..),
    ModuleCache (..),
    ParsedModuleFactsCache,
    ParsedOccurrenceModuleIndexCache (..),
    TypedModuleFactsCache,
  )
import Lore.Internal.Lookup.Cache.Types
  ( ExternalSymbolsEnvironmentKeyCache (..),
    ExternalSymbolsIndexCache (..),
    HomeSymbolsIndexCache (..),
    InstanceEnvironmentInputsCache (..),
    ModSummariesCache (..),
    NameToInstancesIndexCache (..),
    SymbolSearchIndexCache (..),
  )
import Lore.Internal.Session.Cache.Types
  ( GeneratedMainModulesRegistry (..),
    InterpreterContextCache (..),
    LastLoadHomeModulesResultCache (..),
    TemporalModulesRegistry (..),
  )

data SessionCacheVars = SessionCacheVars
  { homeSymbolsIndexCacheVar :: MVar HomeSymbolsIndexCache,
    externalSymbolsIndexCacheVar :: MVar ExternalSymbolsIndexCache,
    symbolSearchIndexCacheVar :: MVar SymbolSearchIndexCache,
    externalSymbolsEnvironmentKeyCacheVar :: MVar ExternalSymbolsEnvironmentKeyCache,
    modSummariesCacheVar :: MVar ModSummariesCache,
    nameToInstancesIndexCacheVar :: MVar NameToInstancesIndexCache,
    instanceEnvironmentInputsCacheVar :: MVar InstanceEnvironmentInputsCache,
    parsedOccurrenceModuleIndexCacheVar :: MVar ParsedOccurrenceModuleIndexCache,
    definitionModuleIndexCacheVar :: MVar DefinitionModuleIndexCache,
    typedModuleFactsCacheVar :: MVar TypedModuleFactsCache,
    coreModuleFactsCacheVar :: MVar CoreModuleFactsCache,
    parsedModuleFactsCacheVar :: MVar ParsedModuleFactsCache,
    interpreterContextCacheVar :: MVar InterpreterContextCache,
    lastLoadHomeModulesResultCacheVar :: MVar LastLoadHomeModulesResultCache,
    generatedMainModulesRegistryVar :: MVar GeneratedMainModulesRegistry,
    temporalModulesRegistryVar :: MVar TemporalModulesRegistry
  }

newSessionCacheVars :: IO SessionCacheVars
newSessionCacheVars = do
  homeSymbolsIndexCacheVar <- newMVar emptyHomeSymbolsIndexCache
  externalSymbolsIndexCacheVar <- newMVar emptyExternalSymbolsIndexCache
  symbolSearchIndexCacheVar <- newMVar emptySymbolSearchIndexCache
  externalSymbolsEnvironmentKeyCacheVar <- newMVar emptyExternalSymbolsEnvironmentKeyCache
  modSummariesCacheVar <- newMVar emptyModSummariesCache
  nameToInstancesIndexCacheVar <- newMVar emptyNameToInstancesIndexCache
  instanceEnvironmentInputsCacheVar <- newMVar emptyInstanceEnvironmentInputsCache
  parsedOccurrenceModuleIndexCacheVar <- newMVar emptyParsedOccurrenceModuleIndexCache
  definitionModuleIndexCacheVar <- newMVar emptyDefinitionModuleIndexCache
  typedModuleFactsCacheVar <- newMVar emptyTypedModuleFactsCache
  coreModuleFactsCacheVar <- newMVar emptyCoreModuleFactsCache
  parsedModuleFactsCacheVar <- newMVar emptyParsedModuleFactsCache
  interpreterContextCacheVar <- newMVar emptyInterpreterContextCache
  lastLoadHomeModulesResultCacheVar <- newMVar emptyLastLoadHomeModulesResultCache
  generatedMainModulesRegistryVar <- newMVar emptyGeneratedMainModulesRegistry
  temporalModulesRegistryVar <- newMVar emptyTemporalModulesRegistry
  pure SessionCacheVars {..}

emptyHomeSymbolsIndexCache :: HomeSymbolsIndexCache
emptyHomeSymbolsIndexCache =
  HomeSymbolsIndexCache Nothing

emptyExternalSymbolsIndexCache :: ExternalSymbolsIndexCache
emptyExternalSymbolsIndexCache =
  ExternalSymbolsIndexCache Nothing

emptySymbolSearchIndexCache :: SymbolSearchIndexCache
emptySymbolSearchIndexCache =
  SymbolSearchIndexCache Nothing

emptyExternalSymbolsEnvironmentKeyCache :: ExternalSymbolsEnvironmentKeyCache
emptyExternalSymbolsEnvironmentKeyCache =
  ExternalSymbolsEnvironmentKeyCache Set.empty

emptyModSummariesCache :: ModSummariesCache
emptyModSummariesCache =
  ModSummariesCache Nothing

emptyNameToInstancesIndexCache :: NameToInstancesIndexCache
emptyNameToInstancesIndexCache =
  NameToInstancesIndexCache Nothing

emptyInstanceEnvironmentInputsCache :: InstanceEnvironmentInputsCache
emptyInstanceEnvironmentInputsCache =
  InstanceEnvironmentInputsCache Nothing

emptyParsedOccurrenceModuleIndexCache :: ParsedOccurrenceModuleIndexCache
emptyParsedOccurrenceModuleIndexCache =
  ParsedOccurrenceModuleIndexCache Nothing

emptyDefinitionModuleIndexCache :: DefinitionModuleIndexCache
emptyDefinitionModuleIndexCache =
  DefinitionModuleIndexCache Map.empty

emptyTypedModuleFactsCache :: TypedModuleFactsCache
emptyTypedModuleFactsCache =
  ModuleCache Map.empty

emptyCoreModuleFactsCache :: CoreModuleFactsCache
emptyCoreModuleFactsCache =
  ModuleCache Map.empty

emptyParsedModuleFactsCache :: ParsedModuleFactsCache
emptyParsedModuleFactsCache =
  ModuleCache Map.empty

emptyInterpreterContextCache :: InterpreterContextCache
emptyInterpreterContextCache =
  InterpreterContextCache Nothing

emptyLastLoadHomeModulesResultCache :: LastLoadHomeModulesResultCache
emptyLastLoadHomeModulesResultCache =
  LastLoadHomeModulesResultCache Nothing

emptyGeneratedMainModulesRegistry :: GeneratedMainModulesRegistry
emptyGeneratedMainModulesRegistry =
  GeneratedMainModulesRegistry Map.empty

emptyTemporalModulesRegistry :: TemporalModulesRegistry
emptyTemporalModulesRegistry =
  TemporalModulesRegistry Nothing []

data SessionCacheResetAction = SessionCacheResetAction
  { sessionCacheResetActionName :: Text,
    sessionCacheResetActionRun :: SessionCacheVars -> IO ()
  }

sessionCacheResetActions :: [SessionCacheResetAction]
sessionCacheResetActions =
  [ SessionCacheResetAction
      { sessionCacheResetActionName = "homeSymbolsIndexCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.homeSymbolsIndexCacheVar emptyHomeSymbolsIndexCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "externalSymbolsIndexCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.externalSymbolsIndexCacheVar emptyExternalSymbolsIndexCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "symbolSearchIndexCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.symbolSearchIndexCacheVar emptySymbolSearchIndexCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "externalSymbolsEnvironmentKeyCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.externalSymbolsEnvironmentKeyCacheVar emptyExternalSymbolsEnvironmentKeyCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "modSummariesCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.modSummariesCacheVar emptyModSummariesCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "nameToInstancesIndexCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.nameToInstancesIndexCacheVar emptyNameToInstancesIndexCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "instanceEnvironmentInputsCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.instanceEnvironmentInputsCacheVar emptyInstanceEnvironmentInputsCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "parsedOccurrenceModuleIndexCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.parsedOccurrenceModuleIndexCacheVar emptyParsedOccurrenceModuleIndexCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "definitionModuleIndexCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.definitionModuleIndexCacheVar emptyDefinitionModuleIndexCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "typedModuleFactsCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.typedModuleFactsCacheVar emptyTypedModuleFactsCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "coreModuleFactsCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.coreModuleFactsCacheVar emptyCoreModuleFactsCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "parsedModuleFactsCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.parsedModuleFactsCacheVar emptyParsedModuleFactsCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "interpreterContextCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.interpreterContextCacheVar emptyInterpreterContextCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "lastLoadHomeModulesResultCacheVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.lastLoadHomeModulesResultCacheVar emptyLastLoadHomeModulesResultCache
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "generatedMainModulesRegistryVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.generatedMainModulesRegistryVar emptyGeneratedMainModulesRegistry
      },
    SessionCacheResetAction
      { sessionCacheResetActionName = "temporalModulesRegistryVar",
        sessionCacheResetActionRun = \cacheVars ->
          setCacheVarStrict cacheVars.temporalModulesRegistryVar emptyTemporalModulesRegistry
      }
  ]

setCacheVarStrict :: MVar a -> a -> IO ()
setCacheVarStrict cacheVar value = do
  let !forcedValue = value
  modifyMVar_ cacheVar (\_ -> pure forcedValue)
  cachedValue <- readMVar cacheVar
  _ <- evaluate cachedValue
  pure ()
