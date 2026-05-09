module Lore.Internal.Definition.Cache.ModuleArtifacts
  ( DefinitionModuleArtifacts (..),
    lookupDefinitionModuleArtifacts,
  )
where

import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.CoreModuleFacts (lookupCoreModuleFactsCache)
import Lore.Internal.Definition.Cache.ParsedModuleFacts (lookupParsedModuleFactsCache)
import Lore.Internal.Definition.Cache.TypedModuleFacts (lookupTypedModuleFactsCache)
import Lore.Internal.Definition.Types (MinimalCoreModuleFacts, MinimalTypedModuleFacts, ParsedModuleFacts)
import Lore.Monad (MonadLore)

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
