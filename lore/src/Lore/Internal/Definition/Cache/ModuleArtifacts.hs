module Lore.Internal.Definition.Cache.ModuleArtifacts
  ( DefinitionModuleArtifacts (..),
    lookupDefinitionModuleArtifacts,
    lookupDefinitionModuleArtifactsForModules,
  )
where

import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.CoreModuleFacts (lookupCoreModuleFactsCache)
import Lore.Internal.Definition.Cache.ParsedModuleFacts (lookupParsedModuleFactsCache)
import Lore.Internal.Definition.Cache.TypedModuleFacts (lookupTypedModuleFactsCache)
import Lore.Internal.Definition.Cache.Types
  ( CoreModuleFactsCache (..),
    ParsedModuleFactsCache (..),
    TypedModuleFactsCache (..),
  )
import Lore.Internal.Definition.Types (MinimalCoreModuleFacts, MinimalTypedModuleFacts, ParsedModuleFacts)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)
import UnliftIO (readMVar)

data DefinitionModuleArtifacts = DefinitionModuleArtifacts
  { definitionArtifactParsedFacts :: ParsedModuleFacts,
    definitionArtifactTypedFacts :: MinimalTypedModuleFacts,
    definitionArtifactCoreFacts :: Maybe MinimalCoreModuleFacts
  }

lookupDefinitionModuleArtifacts :: (MonadLore m) => GHC.Module -> m (Maybe DefinitionModuleArtifacts)
lookupDefinitionModuleArtifacts homeModule = do
  maybeParsedFacts <- lookupParsedModuleFactsCache homeModule
  maybeTypedFacts <- lookupTypedModuleFactsCache homeModule
  maybeCoreFacts <- lookupCoreModuleFactsCache homeModule
  pure do
    definitionArtifactParsedFacts <- maybeParsedFacts
    definitionArtifactTypedFacts <- maybeTypedFacts
    pure
      DefinitionModuleArtifacts
        { definitionArtifactParsedFacts,
          definitionArtifactTypedFacts,
          definitionArtifactCoreFacts = maybeCoreFacts
        }

lookupDefinitionModuleArtifactsForModules ::
  (MonadLore m) =>
  [GHC.Module] ->
  m (Map.Map GHC.Module DefinitionModuleArtifacts)
lookupDefinitionModuleArtifactsForModules requestedModules = do
  SessionContext {parsedModuleFactsCacheVar, typedModuleFactsCacheVar, coreModuleFactsCacheVar} <- asks id
  ParsedModuleFactsCache parsedFactsByModule <- readMVar parsedModuleFactsCacheVar
  TypedModuleFactsCache typedFactsByModule <- readMVar typedModuleFactsCacheVar
  CoreModuleFactsCache coreFactsByModule <- readMVar coreModuleFactsCacheVar
  let requestedModuleSet =
        Set.fromList requestedModules
      parsedRequested =
        Map.restrictKeys parsedFactsByModule requestedModuleSet
      typedRequested =
        Map.restrictKeys typedFactsByModule requestedModuleSet
      coreRequested =
        Map.restrictKeys coreFactsByModule requestedModuleSet
  pure $
    Map.intersectionWithKey
      ( \homeModule definitionArtifactParsedFacts definitionArtifactTypedFacts ->
          DefinitionModuleArtifacts
            { definitionArtifactParsedFacts,
              definitionArtifactTypedFacts,
              definitionArtifactCoreFacts =
                Map.lookup homeModule coreRequested
            }
      )
      parsedRequested
      typedRequested
