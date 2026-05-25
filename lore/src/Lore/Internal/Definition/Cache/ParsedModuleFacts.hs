module Lore.Internal.Definition.Cache.ParsedModuleFacts
  ( ParsedModuleFactsCache (..),
    lookupParsedModuleFactsCache,
    storeParsedModuleFactsCacheInContext,
    retainParsedModuleFactsCacheForLoadedModules,
  )
where

import Control.Exception (evaluate)
import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.Types (ParsedModuleFactsCache (..))
import Lore.Internal.Definition.Types (ParsedModuleFacts)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, modifyMVar_, readMVar)

lookupParsedModuleFactsCache :: (MonadLore m) => GHC.Module -> m (Maybe ParsedModuleFacts)
lookupParsedModuleFactsCache homeModule = do
  cacheVar <- asks parsedModuleFactsCacheVar
  ParsedModuleFactsCache parsedFactsByModule <- readMVar cacheVar
  pure (Map.lookup homeModule parsedFactsByModule)

storeParsedModuleFactsCacheInContext :: SessionContext -> GHC.Module -> ParsedModuleFacts -> IO ()
storeParsedModuleFactsCacheInContext sessionContext homeModule parsedFacts =
  modifyMVar_ (parsedModuleFactsCacheVar sessionContext) \(ParsedModuleFactsCache parsedFactsByModule) ->
    evaluate (ParsedModuleFactsCache (Map.insert homeModule parsedFacts parsedFactsByModule))

retainParsedModuleFactsCacheForLoadedModules :: (MonadLore m) => Set.Set GHC.Module -> m ()
retainParsedModuleFactsCacheForLoadedModules loadedModules = do
  cacheVar <- asks parsedModuleFactsCacheVar
  modifyMVar cacheVar $ \(ParsedModuleFactsCache parsedFactsByModule) ->
    pure (ParsedModuleFactsCache (Map.restrictKeys parsedFactsByModule loadedModules), ())
