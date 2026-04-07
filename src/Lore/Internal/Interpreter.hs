module Lore.Internal.Interpreter
  ( interpreterContextIsReady,
    invalidateInterpreterContext,
    refreshInterpreterContext,
    interpretExpressionRaw,
    getTypeOfExpressionRaw,
  )
where

import Control.Monad.Reader (asks)
import Data.Dynamic (fromDynamic)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import Lore.Internal.Lookup.ModSummaries (getModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Session (PreludeImportRule (..), SessionContext (..))
import Lore.Monad (MonadLore)
import UnliftIO (modifyMVar, readMVar)

interpreterContextIsReady :: (MonadLore m) => m Bool
interpreterContextIsReady = do
  cacheVar <- asks interpreterContextCache
  maybe False (const True) <$> readMVar cacheVar

invalidateInterpreterContext :: (MonadLore m) => m ()
invalidateInterpreterContext = do
  cacheVar <- asks interpreterContextCache
  modifyMVar cacheVar $ \_ -> pure (Nothing, ())

refreshInterpreterContext :: (MonadLore m) => m ()
refreshInterpreterContext = do
  preludeRule <- asks interpreterPreludeImportRule
  ModSummaries modSummaries <- getModSummaries
  loadedModuleNames <- Set.toAscList . Set.fromList <$> mapMMaybe loadedHomeModuleName (Map.elems modSummaries)
  GHC.setContext (preludeImports preludeRule <> map importModule loadedModuleNames)
  cacheVar <- asks interpreterContextCache
  modifyMVar cacheVar $ \_ -> pure (Just loadedModuleNames, ())
  where
    importModule =
      GHC.IIDecl . GHC.simpleImportDecl

    preludeImports = \case
      NoPrelude ->
        []
      ImportBasePrelude ->
        [importModule (GHC.mkModuleName "Prelude")]
      ImportCustomPrelude moduleName ->
        [importModule (GHC.mkModuleName (T.unpack moduleName))]

    loadedHomeModuleName summary = do
      maybeInfo <- GHC.getModuleInfo (GHC.ms_mod summary)
      pure $
        case maybeInfo of
          Just _ -> Just (GHC.moduleName (GHC.ms_mod summary))
          Nothing -> Nothing

interpretExpressionRaw :: (MonadLore m) => Text -> m String
interpretExpressionRaw source = do
  compiled <- GHC.dynCompileExpr (renderedExpression source)
  case fromDynamic compiled of
    Just rendered ->
      pure rendered
    Nothing ->
      error "Lore.Interpreter.interpretExpressionRaw: expected a String result from the compiled expression."
  where
    renderedExpression snippet =
      "show ("
        <> T.unpack snippet
        <> ")"

getTypeOfExpressionRaw :: (MonadLore m) => Text -> m GHC.Type
getTypeOfExpressionRaw source = do
  GHC.exprType GHC.TM_Inst (T.unpack source)

mapMMaybe :: (Applicative m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMMaybe f =
  fmap foldMaybes . traverse f
  where
    foldMaybes =
      foldr
        (\item acc -> maybe acc (: acc) item)
        []
