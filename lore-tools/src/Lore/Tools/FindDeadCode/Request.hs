module Lore.Tools.FindDeadCode.Request
  ( FindDeadCodeFailureReason (..),
    FindDeadCodeOptions (..),
    ResolvedFindDeadCodeRequest (..),
    resolveFindDeadCodeRequest,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import Lore
  ( DeadCodeConfig (..),
    LoreConfig (..),
    LoreConfigError,
    MonadLore,
    Symbol (..),
    compileModulePattern,
    loadLoreConfig,
    matchesModulePattern,
    mkNormalizedModuleName,
    renderLoreConfigError,
    resolveDefinitionSourceNamed,
  )
import Lore.Tools.Internal.SymbolResolution
  ( ResolvedSymbolQuery (..),
    SymbolsResolved (resolvedQueries),
    SymbolsUnresolved,
    resolveUniqueSymbolQueries,
    unresolvedSymbolQueriesMessage,
  )
import Lore.Tools.Render.Doc (ToLoreDoc (toLoreDoc), paragraph)
import Lore.Tools.Render.Text (quoteText, renderModuleName)
import Lore.Tools.Result (PageRequest)

data FindDeadCodeOptions = FindDeadCodeOptions
  { findDeadCodeModules :: Maybe [Text],
    findDeadCodePageRequest :: Maybe PageRequest
  }
  deriving stock (Eq, Show, Generic)

data FindDeadCodeFailureReason
  = FindDeadCodeUnresolvedModules [Text]
  | FindDeadCodeInvalidConfig LoreConfigError
  | FindDeadCodeUnresolvedAliveModules [Text]
  | FindDeadCodeUnresolvedSymbols SymbolsUnresolved
  | FindDeadCodeInvalidAliveSymbols [Text]

instance ToLoreDoc FindDeadCodeFailureReason where
  toLoreDoc = \case
    FindDeadCodeUnresolvedModules unresolved ->
      paragraph (T.intercalate "\n" unresolved)
    FindDeadCodeInvalidConfig configError ->
      paragraph (renderLoreConfigError configError)
    FindDeadCodeUnresolvedAliveModules unresolved ->
      paragraph (T.intercalate "\n" unresolved)
    FindDeadCodeUnresolvedSymbols unresolvedSymbols ->
      paragraph (unresolvedSymbolQueriesMessage unresolvedSymbols)
    FindDeadCodeInvalidAliveSymbols invalidSymbols ->
      paragraph (T.intercalate "\n" invalidSymbols)

data ResolvedFindDeadCodeRequest = ResolvedFindDeadCodeRequest
  { resolvedDeadCodeTargetModules :: Maybe (Set.Set GHC.Module),
    resolvedDeadCodeAliveModules :: Set.Set GHC.Module,
    resolvedDeadCodeAliveNames :: Set.Set GHC.Name
  }

resolveFindDeadCodeRequest ::
  (MonadLore m) =>
  FindDeadCodeOptions ->
  m (Either FindDeadCodeFailureReason ResolvedFindDeadCodeRequest)
resolveFindDeadCodeRequest FindDeadCodeOptions {findDeadCodeModules} = do
  eiTargetModules <- resolveOptionalLoadedHomeModules findDeadCodeModules
  case eiTargetModules of
    Left unresolvedModules ->
      pure (Left (FindDeadCodeUnresolvedModules unresolvedModules))
    Right targetModules -> do
      eiConfig <- loadLoreConfig
      case eiConfig of
        Left configError ->
          pure (Left (FindDeadCodeInvalidConfig configError))
        Right config -> do
          eiAliveModules <- resolveLoadedHomeModulesByPattern config.loreConfigDeadCode.deadCodeConfigAliveModules
          case eiAliveModules of
            Left unresolvedAliveModules ->
              pure (Left (FindDeadCodeUnresolvedAliveModules unresolvedAliveModules))
            Right resolvedAliveModules -> do
              eiAliveNames <- resolveAliveRootNames config.loreConfigDeadCode.deadCodeConfigAliveSymbols
              pure $
                case eiAliveNames of
                  Left unresolvedSymbols ->
                    Left (FindDeadCodeUnresolvedSymbols unresolvedSymbols)
                  Right (Left invalidAliveSymbols) ->
                    Left (FindDeadCodeInvalidAliveSymbols invalidAliveSymbols)
                  Right (Right aliveRootNames) ->
                    Right
                      ResolvedFindDeadCodeRequest
                        { resolvedDeadCodeTargetModules = targetModules,
                          resolvedDeadCodeAliveModules = resolvedAliveModules,
                          resolvedDeadCodeAliveNames = aliveRootNames
                        }

resolveOptionalLoadedHomeModules ::
  (MonadLore m) =>
  Maybe [Text] ->
  m (Either [Text] (Maybe (Set.Set GHC.Module)))
resolveOptionalLoadedHomeModules maybeModuleNames =
  case maybeModuleNames of
    Nothing ->
      pure (Right Nothing)
    Just moduleNames -> do
      eiResolved <- resolveLoadedHomeModulesByName moduleNames
      pure (fmap Just eiResolved)

resolveLoadedHomeModulesByName ::
  (MonadLore m) =>
  [Text] ->
  m (Either [Text] (Set.Set GHC.Module))
resolveLoadedHomeModulesByName requestedModuleNames = do
  loadedModules <- loadedHomeModules
  let loadedModulesByName =
        Map.fromListWith
          (++)
          [ (moduleName, [module_])
          | (moduleName, module_) <- loadedModules
          ]
  pure $ collectLoadedHomeModuleResolutions (map (resolveOne loadedModulesByName) requestedModuleNames)
  where
    resolveOne loadedModulesByName requestedModuleName =
      case Map.lookup requestedModuleName loadedModulesByName of
        Nothing ->
          Left ("Module " <> quoteText requestedModuleName <> " is not present in the loaded home module graph.")
        Just [module_] ->
          Right [module_]
        Just _ ->
          Left ("Module " <> quoteText requestedModuleName <> " is ambiguous in the loaded home module graph.")

resolveLoadedHomeModulesByPattern ::
  (MonadLore m) =>
  [Text] ->
  m (Either [Text] (Set.Set GHC.Module))
resolveLoadedHomeModulesByPattern requestedModulePatterns = do
  loadedModules <- loadedHomeModules
  pure $ collectLoadedHomeModuleResolutions (map (resolveOne loadedModules) requestedModulePatterns)
  where
    resolveOne loadedModules requestedModulePattern =
      case compileModulePattern requestedModulePattern of
        Left _ ->
          Left $
            "Module pattern "
              <> quoteText requestedModulePattern
              <> " is invalid: module patterns must be nonempty strings."
        Right compiledPattern ->
          case [ module_
                 | (moduleName, module_) <- loadedModules,
                   compiledPattern `matchesModulePattern` mkNormalizedModuleName moduleName
               ] of
            [] ->
              Left ("Module pattern " <> quoteText requestedModulePattern <> " does not match any loaded home modules.")
            matchedModules ->
              Right matchedModules

collectLoadedHomeModuleResolutions ::
  [Either Text [GHC.Module]] ->
  Either [Text] (Set.Set GHC.Module)
collectLoadedHomeModuleResolutions resolutions =
  if null unresolvedMessages
    then Right (Set.fromList resolvedModules)
    else Left unresolvedMessages
  where
    unresolvedMessages =
      [ message
      | Left message <- resolutions
      ]
    resolvedModules =
      concat
        [ modules
        | Right modules <- resolutions
        ]

loadedHomeModules ::
  (MonadLore m) =>
  m [(Text, GHC.Module)]
loadedHomeModules = do
  moduleGraph <- GHC.getModuleGraph
  let loadedModules =
        map GHC.ms_mod (GHC.mgModSummaries moduleGraph)
      loadedModulePairs =
        [ (renderModuleName module_, module_)
        | module_ <- loadedModules
        ]
  pure loadedModulePairs

resolveAliveRootNames ::
  (MonadLore m) =>
  [Text] ->
  m (Either SymbolsUnresolved (Either [Text] (Set.Set GHC.Name)))
resolveAliveRootNames queries =
  if null queries
    then pure (Right (Right Set.empty))
    else do
      eiResolved <- resolveUniqueSymbolQueries queries
      case eiResolved of
        Left unresolved ->
          pure (Left unresolved)
        Right resolved -> do
          validations <- mapM resolveOneAliveQuery resolved.resolvedQueries
          let invalidMessages =
                [ invalidMessage
                | Left invalidMessage <- validations
                ]
              validRootNames =
                [ name
                | Right name <- validations
                ]
          pure $
            if null invalidMessages
              then Right (Right (Set.fromList validRootNames))
              else Right (Left invalidMessages)

resolveOneAliveQuery ::
  (MonadLore m) =>
  ResolvedSymbolQuery ->
  m (Either Text GHC.Name)
resolveOneAliveQuery resolvedQuery = do
  maybeAliveName <- firstAliveName candidates
  pure $
    case maybeAliveName of
      Just aliveName ->
        Right aliveName
      Nothing ->
        Left $
          "Symbol "
            <> quoteText resolvedQuery.queryText
            <> " resolved, but it is not a loaded home definition and cannot be used as a dead-code root."
  where
    candidates =
      dedupeNames [resolvedQuery.resolvedSymbol.name, resolvedQuery.resolvedRootName]

firstAliveName :: (MonadLore m) => [GHC.Name] -> m (Maybe GHC.Name)
firstAliveName [] =
  pure Nothing
firstAliveName (name : restNames) = do
  maybeSource <- resolveDefinitionSourceNamed name
  case maybeSource of
    Just _ ->
      pure (Just name)
    Nothing ->
      firstAliveName restNames

dedupeNames :: [GHC.Name] -> [GHC.Name]
dedupeNames =
  foldr
    ( \name deduped ->
        if name `elem` deduped
          then deduped
          else name : deduped
    )
    []
