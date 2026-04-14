{-# OPTIONS_GHC -Wno-orphans #-}

module Lore.Lookup
  ( Symbol (..),
    RootSymbolInfo (..),
    SymbolVisibility (..),
    ExportedSymbolNode (..),
    SymbolCategory (..),
    classifySymbolCategory,
    SymbolInfo (..),
    Instances (..),
    LookupInstancesQuery (..),
    MatchingInstance (..),
    LookupInstancesResult (..),
    findSymbols,
    listExportedSymbolsByModule,
    filterExportedSymbolNodesByTypeHint,
    lookupSymbolInfo,
    lookupRootSymbolInfo,
    lookupRootSymbolInfoWithChain,
    lookupIntersectingInstances,
    lookupIntersectingRootInstances,
    resolveInstances,
    resolveInstanceDefinitions,
  )
where

import Control.Monad (forM)
import Data.Char (isAlphaNum, isUpper)
import Data.Containers.ListUtils (nubOrdOn)
import Data.List (foldl', intercalate, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Core.FamInstEnv as GHC
import qualified GHC.Data.FastString as FastString
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Avail as Avail
import qualified GHC.Types.TyThing as GHC
import Lore.Definition (DefinitionSlice, resolveDefinitionSlice)
import qualified Lore.Internal.Ghc.TyThing as TyThing
import Lore.Internal.Lookup.NameToInstances (getNameToInstancesIndex)
import qualified Lore.Internal.Lookup.SymbolsMap as SymbolsMap
import Lore.Internal.Lookup.Types (NameToInstancesIndex (..), Symbol (..), SymbolVisibility (..), SymbolsMap, symbolExportedFrom)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (SomeException, handle)

findSymbols :: (MonadLore m) => Text -> m [Symbol]
findSymbols needle = do
  findMatchingSymbols needle <$> SymbolsMap.getSymbolsMap

listExportedSymbolsByModule :: (MonadLore m) => Text -> Maybe Text -> m [ExportedSymbolNode]
listExportedSymbolsByModule moduleName maybePackageName = do
  hscEnv <- GHC.getSession
  maybeModule <- resolveModule hscEnv moduleName maybePackageName
  case maybeModule of
    Nothing ->
      pure []
    Just module_ -> do
      exportedSymbols <- loadExportedSymbolsForModule hscEnv module_
      pure $
        sortOn
          (renderOutputable . nodeName)
          exportedSymbols

filterExportedSymbolNodesByTypeHint :: Text -> [ExportedSymbolNode] -> [ExportedSymbolNode]
filterExportedSymbolNodesByTypeHint typeHint =
  mapMaybe (filterExportedSymbolNodeByTypeHint normalizedTypeName)
  where
    normalizedTypeName = normalizeQueryOccName typeHint

resolveModule :: (MonadLore m) => GHC.HscEnv -> Text -> Maybe Text -> m (Maybe GHC.Module)
resolveModule _hscEnv moduleName maybePackageName =
  handle
    ( \(err :: SomeException) -> do
        Log.warn $ "Failed to resolve module " <> renderModuleRequest moduleName maybePackageName <> ": " <> show err
        pure Nothing
    )
    do
      let moduleName' = GHC.mkModuleName (T.unpack moduleName)
          packageQualifier = fmap (FastString.mkFastString . T.unpack) maybePackageName
      Just <$> GHC.lookupModule moduleName' packageQualifier

renderModuleRequest :: Text -> Maybe Text -> String
renderModuleRequest moduleName maybePackageName =
  case maybePackageName of
    Nothing ->
      show (T.unpack moduleName)
    Just packageName ->
      show (T.unpack moduleName) <> " in package " <> show (T.unpack packageName)

loadExportedSymbolsForModule :: (MonadLore m) => GHC.HscEnv -> GHC.Module -> m [ExportedSymbolNode]
loadExportedSymbolsForModule hscEnv module_ = do
  exportedAvailInfos <- loadExportedAvailInfosForModule hscEnv module_
  sortOn (renderOutputable . nodeName) . catMaybes <$> mapM buildRootNodeFromAvail exportedAvailInfos

buildLeafNode :: (MonadLore m) => GHC.Name -> m (Maybe ExportedSymbolNode)
buildLeafNode childName = do
  maybeChildThing <- GHC.lookupName childName
  pure $
    fmap
      ( \childThing ->
          ExportedSymbolNode
            { nodeName = childName,
              nodeThing = childThing,
              nodeChildren = []
            }
      )
      maybeChildThing

buildRootNodeFromAvail :: (MonadLore m) => Avail.AvailInfo -> m (Maybe ExportedSymbolNode)
buildRootNodeFromAvail availInfo = do
  let rootName = Avail.availName availInfo
      childNames =
        sortOn renderOutputable $
          map Avail.greNamePrintableName (Avail.availSubordinateGreNames availInfo)
  maybeRootThing <- GHC.lookupName rootName
  case maybeRootThing of
    Nothing ->
      pure Nothing
    Just rootThing -> do
      childNodes <- catMaybes <$> mapM buildLeafNode childNames
      pure $
        Just
          ExportedSymbolNode
            { nodeName = rootName,
              nodeThing = rootThing,
              nodeChildren = childNodes
            }

filterExportedSymbolNodeByTypeHint :: Text -> ExportedSymbolNode -> Maybe ExportedSymbolNode
filterExportedSymbolNodeByTypeHint typeHint node =
  if nodeMatches || not (null matchingChildren)
    then Just node {nodeChildren = matchingChildren}
    else Nothing
  where
    nodeMatches = TyThing.isMentionedByOccString (T.unpack typeHint) node.nodeThing
    matchingChildren = mapMaybe (filterExportedSymbolNodeByTypeHint typeHint) node.nodeChildren

loadExportedAvailInfosForModule :: (MonadLore m) => GHC.HscEnv -> GHC.Module -> m [Avail.AvailInfo]
loadExportedAvailInfosForModule _hscEnv module_ = do
  maybeModuleInfo <- GHC.getModuleInfo module_
  case maybeModuleInfo of
    Just moduleInfo ->
      case GHC.modInfoIface moduleInfo of
        Just modIface ->
          pure (deduplicateAvailInfos (GHC.mi_exports modIface))
        Nothing -> do
          Log.warn $ "Failed to get interface exports for module " <> show (GHC.moduleNameString (GHC.moduleName module_)) <> ": modInfoIface returned Nothing. Falling back to flat export names."
          pure (map (Avail.Avail . Avail.NormalGreName) (deduplicateNames (GHC.modInfoExports moduleInfo)))
    Nothing -> do
      Log.warn $ "Failed to get exports for module " <> show (GHC.moduleNameString (GHC.moduleName module_)) <> ": getModuleInfo returned Nothing."
      pure []

deduplicateAvailInfos :: [Avail.AvailInfo] -> [Avail.AvailInfo]
deduplicateAvailInfos =
  nubOrdOn renderOutputable

data SymbolInfo = SymbolInfo
  { symbolName :: GHC.Name,
    definedIn :: GHC.Module,
    exportedFrom :: [GHC.Module],
    symbolThing :: GHC.TyThing,
    symbolType :: Maybe GHC.Type,
    associatedClassInstances :: [GHC.ClsInst],
    associatedFamilyInstances :: [GHC.FamInst]
  }

data RootSymbolInfo = RootSymbolInfo
  { rootSymbolInfo :: SymbolInfo,
    rootSymbolChain :: [GHC.Name]
  }

data SymbolCategory
  = SymbolValue
  | SymbolData
  | SymbolNewtype
  | SymbolTypeAlias
  | SymbolClass
  | SymbolTypeFamily
  | SymbolDataFamily
  | SymbolConstructor
  | SymbolCoercionAxiom
  | SymbolUnknown
  deriving stock (Eq, Show)

data ExportedSymbolNode = ExportedSymbolNode
  { nodeName :: GHC.Name,
    nodeThing :: GHC.TyThing,
    nodeChildren :: [ExportedSymbolNode]
  }

instance Show SymbolInfo where
  show si =
    "Symbol: "
      <> showName si.symbolName
      <> ",\n Defined in: "
      <> showModule si.definedIn
      <> ",\n Exported from: "
      <> showModules si.exportedFrom
      <> ",\n Class instances: "
      <> intercalate ", " (map showClassInst si.associatedClassInstances)
      <> ",\n Family instances: "
      <> intercalate ", " (map showFamInst si.associatedFamilyInstances)
    where
      showName n = case GHC.nameModule_maybe n of
        Nothing -> "<UNKNOWN>." <> GHC.occNameString (GHC.nameOccName n)
        Just m -> GHC.moduleNameString (GHC.moduleName m) <> "." <> GHC.occNameString (GHC.nameOccName n)
      showModule m = GHC.moduleNameString (GHC.moduleName m)
      showModules xs = intercalate ", " (map showModule xs)
      showClassInst clsInst = GHC.showSDocUnsafe $ GHC.pprInstance clsInst
      showFamInst famInst =
        let famName = GHC.getOccString (GHC.fi_fam famInst)
         in famName

lookupSymbolInfo :: (MonadLore m) => Text -> m [SymbolInfo]
lookupSymbolInfo = getSymbolInfo' False

lookupRootSymbolInfo :: (MonadLore m) => Text -> m [SymbolInfo]
lookupRootSymbolInfo =
  fmap (map rootSymbolInfo) . lookupRootSymbolInfoWithChain

lookupRootSymbolInfoWithChain :: (MonadLore m) => Text -> m [RootSymbolInfo]
lookupRootSymbolInfoWithChain =
  getRootSymbolInfo' True

getSymbolInfo' :: (MonadLore m) => Bool -> Text -> m [SymbolInfo]
getSymbolInfo' resolveRoot =
  fmap (map rootSymbolInfo) . getRootSymbolInfo' resolveRoot

getRootSymbolInfo' :: (MonadLore m) => Bool -> Text -> m [RootSymbolInfo]
getRootSymbolInfo' resolveRoot needle = do
  symbolsMap <- SymbolsMap.getSymbolsMap
  let symbols = findMatchingSymbols needle symbolsMap
  resolvedRoots <-
    if resolveRoot
      then resolveRootSymbolsWithChain symbolsMap symbols
      else pure [ResolvedRootSymbol symbol [symbol.name] | symbol <- symbols]
  Log.debug $ "Found " <> show (length resolvedRoots) <> " symbols matching query \"" <> T.unpack needle <> "\"."
  catMaybes <$> forM resolvedRoots getRootSymbolInfo

getSymbolInfo :: (MonadLore m) => Symbol -> m (Maybe SymbolInfo)
getSymbolInfo symbol = do
  case GHC.nameModule_maybe symbol.name of
    Nothing -> do
      Log.warn $ "Symbol " <> GHC.showSDocUnsafe (GHC.ppr symbol.name) <> " does not have an associated module. Skipping instance resolution."
      pure Nothing
    Just m -> do
      Log.debug $ "Looking up symbol: " <> GHC.showSDocUnsafe (GHC.ppr symbol.name)
      maybeTyThing <- GHC.lookupName symbol.name
      case maybeTyThing of
        Nothing -> do
          Log.warn $ "Symbol " <> GHC.showSDocUnsafe (GHC.ppr symbol.name) <> " does not have a TyThing in the loaded session state. Skipping."
          pure Nothing
        Just tyThing -> do
          let symbolCategory = classifySymbolCategory tyThing
              symbolType = case tyThing of
                GHC.AnId id' -> Just (GHC.idType id')
                _ -> Nothing
          Log.debug $ "Symbol " <> GHC.showSDocUnsafe (GHC.ppr symbol.name) <> " is categorized as " <> show symbolCategory <> "."
          instancesInfo <- resolveInstances symbol.name
          Log.debug $ "Symbol " <> GHC.showSDocUnsafe (GHC.ppr symbol.name) <> " has " <> show (maybe 0 (length . classInstances) instancesInfo) <> " class instances and " <> show (maybe 0 (length . familyInstances) instancesInfo) <> " family instances."
          pure $
            Just
              SymbolInfo
                { symbolName = symbol.name,
                  definedIn = m,
                  exportedFrom = symbolExportedFrom symbol,
                  symbolThing = tyThing,
                  symbolType = symbolType,
                  associatedClassInstances = maybe [] classInstances instancesInfo,
                  associatedFamilyInstances = maybe [] familyInstances instancesInfo
                }

getRootSymbolInfo :: (MonadLore m) => ResolvedRootSymbol -> m (Maybe RootSymbolInfo)
getRootSymbolInfo resolvedRoot = do
  maybeSymbolInfo <- getSymbolInfo resolvedRoot.resolvedRootSymbol
  pure $
    fmap
      ( \symbolInfo ->
          RootSymbolInfo
            { rootSymbolInfo = symbolInfo,
              rootSymbolChain =
                deduplicateNames
                  (resolvedRoot.resolvedRootChain <> relatedRootNames symbolInfo.symbolThing)
            }
      )
      maybeSymbolInfo

relatedRootNames :: GHC.TyThing -> [GHC.Name]
relatedRootNames = \case
  GHC.ATyCon tyCon ->
    GHC.getName tyCon
      : maybe [] (map GHC.dataConName) (GHC.tyConDataCons_maybe tyCon)
  _ ->
    []

data Instances = Instances
  { classInstances :: [GHC.ClsInst],
    familyInstances :: [GHC.FamInst]
  }

classifySymbolCategory :: GHC.TyThing -> SymbolCategory
classifySymbolCategory = \case
  GHC.AnId {} -> SymbolValue
  GHC.AConLike {} -> SymbolConstructor
  GHC.ACoAxiom {} -> SymbolCoercionAxiom
  GHC.ATyCon tyCon
    | GHC.isClassTyCon tyCon -> SymbolClass
    | GHC.isDataFamilyTyCon tyCon -> SymbolDataFamily
    | GHC.isTypeFamilyTyCon tyCon -> SymbolTypeFamily
    | GHC.isTypeSynonymTyCon tyCon -> SymbolTypeAlias
    | GHC.isNewTyCon tyCon -> SymbolNewtype
    | GHC.isDataTyCon tyCon -> SymbolData
    | otherwise -> SymbolUnknown

data ResolvedRootSymbol = ResolvedRootSymbol
  { resolvedRootSymbol :: Symbol,
    resolvedRootChain :: [GHC.Name]
  }

resolveRootSymbolsWithChain :: (MonadLore m) => SymbolsMap -> [Symbol] -> m [ResolvedRootSymbol]
resolveRootSymbolsWithChain symbolsMap symbols = do
  Log.debug $ "Resolving root symbols for " <> show (length symbols) <> " symbols."
  resolvedRoots <- mergeResolvedRoots <$> mapM (resolveRootSymbolWithChain symbolsMap) symbols
  Log.debug "Finished resolving root symbols."
  pure resolvedRoots

resolveRootSymbolWithChain :: (MonadLore m) => SymbolsMap -> Symbol -> m ResolvedRootSymbol
resolveRootSymbolWithChain symbolsMap symbol = do
  rootChain <- resolveRootNameChain symbol.name
  let rootName = lastOr symbol.name rootChain
  maybeRootInfo <- getSymbolInfo (symbol {name = rootName})
  let rootSymbol =
        case maybeRootInfo of
          Just rootInfo ->
            Symbol
              { name = rootInfo.symbolName,
                visibility = Symbol'ExportedFrom rootInfo.exportedFrom
              }
          Nothing ->
            case SymbolsMap.lookupSymbolByNameInMap rootName symbolsMap of
              Just foundRootSymbol -> foundRootSymbol
              Nothing -> symbol {name = rootName}
  pure
    ResolvedRootSymbol
      { resolvedRootSymbol = rootSymbol,
        resolvedRootChain = deduplicateNames rootChain
      }

mergeResolvedRoots :: [ResolvedRootSymbol] -> [ResolvedRootSymbol]
mergeResolvedRoots =
  sortOn (renderResolvedRootKey . resolvedRootKey)
    . foldr mergeResolvedRoot []
  where
    mergeResolvedRoot resolvedRoot [] =
      [resolvedRoot]
    mergeResolvedRoot resolvedRoot (existingRoot : rest)
      | resolvedRootKey existingRoot == resolvedRootKey resolvedRoot =
          preferResolvedRoot
            existingRoot
            resolvedRoot
              { resolvedRootChain =
                  deduplicateNames (existingRoot.resolvedRootChain <> resolvedRoot.resolvedRootChain)
              }
            : rest
      | otherwise =
          existingRoot : mergeResolvedRoot resolvedRoot rest

    preferResolvedRoot left right =
      case compare (resolvedRootPriority left) (resolvedRootPriority right) of
        LT -> right
        EQ -> left
        GT -> left

resolvedRootKey :: ResolvedRootSymbol -> (Maybe GHC.Module, String)
resolvedRootKey resolvedRoot =
  ( GHC.nameModule_maybe resolvedRoot.resolvedRootSymbol.name,
    GHC.occNameString (GHC.nameOccName resolvedRoot.resolvedRootSymbol.name)
  )

renderResolvedRootKey :: (Maybe GHC.Module, String) -> String
renderResolvedRootKey (maybeModule, occName) =
  maybe "<no-module>" (GHC.moduleNameString . GHC.moduleName) maybeModule <> "." <> occName

resolvedRootPriority :: ResolvedRootSymbol -> Int
resolvedRootPriority resolvedRoot =
  let name = resolvedRoot.resolvedRootSymbol.name
   in if GHC.isTyConName name
        then 2
        else
          if GHC.isDataConName name
            then 1
            else 0

data LookupInstancesQuery = LookupInstancesQuery
  { lookupInstancesQueryText :: Text,
    lookupInstancesQueryMatches :: [GHC.Name]
  }

data MatchingInstance
  = MatchingClassInstance GHC.Name GHC.ClsInst
  | MatchingFamilyInstance GHC.Name GHC.FamInst

data LookupInstancesResult = LookupInstancesResult
  { lookupInstancesQueries :: [LookupInstancesQuery],
    lookupInstancesResults :: [MatchingInstance]
  }

lookupIntersectingInstances :: (MonadLore m) => [Text] -> m LookupInstancesResult
lookupIntersectingInstances =
  lookupIntersectingInstances' False

lookupIntersectingRootInstances :: (MonadLore m) => [Text] -> m LookupInstancesResult
lookupIntersectingRootInstances =
  lookupIntersectingInstances' True

lookupIntersectingInstances' :: (MonadLore m) => Bool -> [Text] -> m LookupInstancesResult
lookupIntersectingInstances' resolveRoot queries = do
  resolvedQueries <- mapM (resolveLookupInstancesQuery resolveRoot) queries
  pure $
    LookupInstancesResult
      { lookupInstancesQueries = map fst resolvedQueries,
        lookupInstancesResults = intersectAllMatchingInstances (map snd resolvedQueries)
      }

resolveLookupInstancesQuery :: (MonadLore m) => Bool -> Text -> m (LookupInstancesQuery, [MatchingInstance])
resolveLookupInstancesQuery resolveRoot queryText = do
  matchedNames <- findResolvedSymbolNames resolveRoot queryText
  matchedInstances <- lookupMatchingInstancesForNames matchedNames
  pure
    ( LookupInstancesQuery
        { lookupInstancesQueryText = queryText,
          lookupInstancesQueryMatches = matchedNames
        },
      matchedInstances
    )

findResolvedSymbolNames :: (MonadLore m) => Bool -> Text -> m [GHC.Name]
findResolvedSymbolNames resolveRoot needle
  | resolveRoot =
      deduplicateNames . concatMap (.rootSymbolChain) <$> getRootSymbolInfo' True needle
  | otherwise = do
      symbolsMap <- SymbolsMap.getSymbolsMap
      let symbols = findMatchingSymbols needle symbolsMap
      deduplicateNames <$> mapM (resolveSymbolName False) symbols

findMatchingSymbols :: Text -> SymbolsMap -> [Symbol]
findMatchingSymbols queryText symbolsMap =
  filterSymbolsByModuleHint moduleHint $
    SymbolsMap.lookupSymbolsInMap normalizedOccName symbolsMap
  where
    (moduleHint, occName) =
      splitQualifiedSymbolQuery queryText
    normalizedOccName =
      normalizeQueryOccName occName

    filterSymbolsByModuleHint Nothing symbols =
      symbols
    filterSymbolsByModuleHint (Just hintedModule) symbols =
      filter (symbolMatchesModuleHint hintedModule) symbols

    symbolMatchesModuleHint hintedModule symbol =
      symbolDefinedInModuleHint hintedModule symbol
        || any (moduleMatchesHint hintedModule) (symbolExportedFrom symbol)

    symbolDefinedInModuleHint hintedModule symbol =
      maybe False (moduleMatchesHint hintedModule) (GHC.nameModule_maybe symbol.name)

    moduleMatchesHint hintedModule module_ =
      T.pack (GHC.moduleNameString (GHC.moduleName module_)) == hintedModule

splitQualifiedSymbolQuery :: Text -> (Maybe Text, Text)
splitQualifiedSymbolQuery queryText =
  case qualifiedCandidates of
    (moduleHint, occName) : _ ->
      (Just moduleHint, occName)
    [] ->
      (Nothing, queryText)
  where
    segments = T.splitOn "." queryText
    qualifiedCandidates =
      reverse $
        mapMaybe mkCandidate [1 .. length segments - 1]

    mkCandidate prefixLen = do
      let moduleSegments = take prefixLen segments
          occSegments = drop prefixLen segments
          moduleHint = T.intercalate "." moduleSegments
          occName = T.intercalate "." occSegments
      if all isModuleNameSegment moduleSegments && not (T.null occName)
        then Just (moduleHint, occName)
        else Nothing

isModuleNameSegment :: Text -> Bool
isModuleNameSegment segment =
  case T.uncons segment of
    Nothing ->
      False
    Just (firstChar, rest) ->
      isUpper firstChar && T.all isModuleNameChar rest

isModuleNameChar :: Char -> Bool
isModuleNameChar char =
  isAlphaNum char || char == '_' || char == '\''

normalizeQueryOccName :: Text -> Text
normalizeQueryOccName occName =
  case T.stripPrefix "(" occName >>= T.stripSuffix ")" of
    Just strippedOccName
      | isOperatorOccName strippedOccName ->
          strippedOccName
    _ ->
      occName

isOperatorOccName :: Text -> Bool
isOperatorOccName text =
  not (T.null text) && T.all isOperatorChar text

isOperatorChar :: Char -> Bool
isOperatorChar char =
  char `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

resolveSymbolName :: (MonadLore m) => Bool -> Symbol -> m GHC.Name
resolveSymbolName resolveRoot symbol
  | resolveRoot =
      resolveRootName symbol.name
  | otherwise =
      pure symbol.name

lookupMatchingInstancesForNames :: (MonadLore m) => [GHC.Name] -> m [MatchingInstance]
lookupMatchingInstancesForNames names = do
  NameToInstancesIndex nameToInstancesIndex <- getNameToInstancesIndex
  pure $
    deduplicateMatchingInstances $
      concatMap (matchingInstancesForName nameToInstancesIndex) names

matchingInstancesForName :: GHC.NameEnv ([GHC.ClsInst], [GHC.FamInst]) -> GHC.Name -> [MatchingInstance]
matchingInstancesForName nameToInstancesIndex name =
  case GHC.lookupUFM nameToInstancesIndex name of
    Nothing -> []
    Just (clsInsts, famInsts) ->
      map (\clsInst -> MatchingClassInstance (GHC.getName clsInst) clsInst) clsInsts
        <> map (\famInst -> MatchingFamilyInstance (GHC.getName famInst) famInst) famInsts

intersectAllMatchingInstances :: [[MatchingInstance]] -> [MatchingInstance]
intersectAllMatchingInstances = \case
  [] -> []
  firstInstances : rest ->
    foldl' intersectMatchingInstances firstInstances rest

intersectMatchingInstances :: [MatchingInstance] -> [MatchingInstance] -> [MatchingInstance]
intersectMatchingInstances left right =
  filter (\matchingInstance -> GHC.elemNameEnv (matchingInstanceName matchingInstance) rightIndex) left
  where
    rightIndex =
      GHC.mkNameEnv [(matchingInstanceName matchingInstance, ()) | matchingInstance <- right]

deduplicateNames :: [GHC.Name] -> [GHC.Name]
deduplicateNames =
  sortOn renderOutputable
    . GHC.nonDetNameEnvElts
    . GHC.mkNameEnv
    . map (\name -> (name, name))

deduplicateMatchingInstances :: [MatchingInstance] -> [MatchingInstance]
deduplicateMatchingInstances =
  sortOn (renderOutputable . matchingInstanceName)
    . GHC.nonDetNameEnvElts
    . GHC.mkNameEnv
    . map (\matchingInstance -> (matchingInstanceName matchingInstance, matchingInstance))

matchingInstanceName :: MatchingInstance -> GHC.Name
matchingInstanceName = \case
  MatchingClassInstance name _ -> name
  MatchingFamilyInstance name _ -> name

resolveInstances :: (MonadLore m) => GHC.Name -> m (Maybe Instances)
resolveInstances name = do
  NameToInstancesIndex nameToInstancesIndex <- getNameToInstancesIndex
  case GHC.lookupUFM nameToInstancesIndex name of
    Nothing -> pure Nothing
    Just (clsInsts, famInsts) -> pure $ Just (Instances clsInsts famInsts)

resolveInstanceDefinitions :: (MonadLore m) => GHC.Name -> m [DefinitionSlice]
resolveInstanceDefinitions name = do
  NameToInstancesIndex nameToInstancesIndex <- getNameToInstancesIndex
  case GHC.lookupUFM nameToInstancesIndex name of
    Nothing -> pure []
    Just (clsInsts, famInsts) -> do
      let allNames = [GHC.getName clsInst | clsInst <- clsInsts] ++ [GHC.getName famInst | famInst <- famInsts]
      resolved <- mapM resolveDefinitionSlice allNames
      pure $ mapMaybe id resolved

resolveRootName :: (MonadLore m) => GHC.Name -> m GHC.Name
resolveRootName name =
  lastOr name <$> resolveRootNameChain name

resolveRootNameChain :: (MonadLore m) => GHC.Name -> m [GHC.Name]
resolveRootNameChain name = do
  maybeTyThing <- GHC.lookupName name
  pure $
    deduplicateNames $
      case maybeTyThing of
        Nothing ->
          [name]
        Just tyThing ->
          map GHC.getName (collectRootTyThingChain tyThing)
  where
    collectRootTyThingChain tyThing =
      tyThing
        : case GHC.tyThingParent_maybe tyThing of
          Nothing -> []
          Just parentTyThing -> collectRootTyThingChain parentTyThing

lastOr :: a -> [a] -> a
lastOr fallback = \case
  [] -> fallback
  values -> last values

renderOutputable :: (GHC.Outputable a) => a -> String
renderOutputable =
  GHC.showSDocUnsafe . GHC.ppr
