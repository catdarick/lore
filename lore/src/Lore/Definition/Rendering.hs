module Lore.Definition.Rendering
  ( getDefinitionSourceTree,
    chooseBestReferenceContext,
  )
where

import qualified Data.Map.Strict as Map
import Lore.Internal.Definition.Cache.ParsedModuleFacts (lookupParsedModuleFactsCache)
import Lore.Internal.Definition.SourceTree (buildDefinitionSourceTree, chooseBestReferenceContext)
import Lore.Internal.Definition.Types
  ( DefinitionSource (..),
    DefinitionSourceTree (..),
    ParsedModuleFacts (..),
  )
import Lore.Monad (MonadLore)

getDefinitionSourceTree :: (MonadLore m) => DefinitionSource -> m (Maybe DefinitionSourceTree)
getDefinitionSourceTree source = do
  maybeParsedFacts <- lookupParsedModuleFactsCache source.definitionSourceModule
  pure do
    parsedFacts <- maybeParsedFacts
    spans <- Map.lookup source.definitionSourceId parsedFacts.parsedDeclarationsById
    pure
      DefinitionSourceTree
        { sourceTreeDefinition = source,
          sourceTreeRoot =
            buildDefinitionSourceTree
              spans
              parsedFacts.parsedRegionCandidates
        }
