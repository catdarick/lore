module Lore.Internal.Definition.Cache.ParsedModuleFacts
  ( ParsedModuleFactsCache,
    lookupParsedModuleFactsCache,
    storeParsedModuleFactsCacheInContext,
    retainParsedModuleFactsCacheForLoadedModules,
  )
where

import Control.Monad.Reader (asks)
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.ModuleCache (lookupModuleCache, retainModuleCache, storeModuleCache)
import Lore.Internal.Definition.Cache.Types (ParsedModuleFactsCache)
import Lore.Internal.Definition.Types (ParsedModuleFacts)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)

lookupParsedModuleFactsCache :: (MonadLore m) => GHC.Module -> m (Maybe ParsedModuleFacts)
lookupParsedModuleFactsCache homeModule = do
  cacheVar <- asks parsedModuleFactsCacheVar
  lookupModuleCache homeModule cacheVar

storeParsedModuleFactsCacheInContext :: SessionContext -> GHC.Module -> ParsedModuleFacts -> IO ()
storeParsedModuleFactsCacheInContext sessionContext homeModule parsedFacts =
  storeModuleCache homeModule parsedFacts (parsedModuleFactsCacheVar sessionContext)

retainParsedModuleFactsCacheForLoadedModules :: (MonadLore m) => Set.Set GHC.Module -> m ()
retainParsedModuleFactsCacheForLoadedModules loadedModules = do
  cacheVar <- asks parsedModuleFactsCacheVar
  retainModuleCache loadedModules cacheVar
