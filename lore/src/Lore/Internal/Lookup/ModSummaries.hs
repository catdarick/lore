module Lore.Internal.Lookup.ModSummaries
  ( ModSummariesCache (..),
    emptyModSummariesCache,
    getCachedModSummaries,
    getCachedModSummariesByFile,
    prepareModSummaries,
    prepareFreshModSummariesByFile,
    invalidateModSummariesCache,
    lookupModSummary,
    modSummariesToMap,
  )
where

import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import qualified GHC
import Lore.Internal.Lookup.Cache.Types (ModSummariesCache (..))
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import System.FilePath (normalise)
import UnliftIO (modifyMVar)

emptyModSummariesCache :: ModSummariesCache
emptyModSummariesCache =
  ModSummariesCache Nothing

getCachedModSummaries :: (MonadLore m) => m ModSummaries
getCachedModSummaries = do
  cacheVar <- asks modSummariesCacheVar
  modifyMVar cacheVar $ \cacheState ->
    case cacheState.cachedModSummaries of
      Just modSummaries ->
        pure (cacheState, modSummaries)
      Nothing -> do
        modSummaries <- prepareModSummaries
        pure (ModSummariesCache (Just modSummaries), modSummaries)

invalidateModSummariesCache :: (MonadLore m) => m ()
invalidateModSummariesCache = do
  cacheVar <- asks modSummariesCacheVar
  modifyMVar cacheVar $ \_ -> pure (emptyModSummariesCache, ())

prepareModSummaries :: (MonadLore m) => m ModSummaries
prepareModSummaries = do
  Log.debug "Preparing module summaries map..."
  moduleGraph <- GHC.getModuleGraph
  let modSummariesMap = Map.fromList [(GHC.ms_mod ms, ms) | ms <- GHC.mgModSummaries moduleGraph]
  Log.debug $ "Prepared module summaries map with " <> show (Map.size modSummariesMap) <> " entries."
  pure $ ModSummaries modSummariesMap

lookupModSummary :: GHC.Module -> ModSummaries -> Maybe GHC.ModSummary
lookupModSummary homeModule (ModSummaries modSummariesByModule) =
  Map.lookup homeModule modSummariesByModule

modSummariesToMap :: ModSummaries -> Map.Map GHC.Module GHC.ModSummary
modSummariesToMap (ModSummaries modSummariesByModule) =
  modSummariesByModule

getCachedModSummariesByFile :: (MonadLore m) => m (Map.Map FilePath GHC.ModSummary)
getCachedModSummariesByFile = do
  ModSummaries modSummariesByModule <- getCachedModSummaries
  pure $
    Map.fromList
      [ (normalise sourceFile, summary)
      | summary <- Map.elems modSummariesByModule,
        sourceFile <- maybeToList (GHC.ml_hs_file (GHC.ms_location summary))
      ]

prepareFreshModSummariesByFile :: (MonadLore m) => m (Map.Map FilePath GHC.ModSummary)
prepareFreshModSummariesByFile = do
  moduleGraph <- GHC.depanal [] False
  pure $
    Map.fromList
      [ (normalise sourceFile, summary)
      | summary <- GHC.mgModSummaries moduleGraph,
        sourceFile <- maybeToList (GHC.ml_hs_file (GHC.ms_location summary))
      ]
