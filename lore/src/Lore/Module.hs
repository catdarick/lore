module Lore.Module
  ( ExportedSymbolNode (..),
    resolveModule,
    listSymbolsExportedByModule,
    filterExportedSymbolNodesByTypeHint,
  )
where

import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Data.FastString as FastString
import qualified GHC.Types.Avail as Avail
import GHC.Utils.Monad (mapMaybeM)
import qualified Lore.Internal.Ghc.TyThing as TyThing
import Lore.Internal.Lookup.Name (NormalizedModuleName, NormalizedOccName, mkGhcModuleName)
import Lore.Internal.Package (PackageData (packageName), discoverProject)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO (SomeException, handle)

data ExportedSymbolNode = ExportedSymbolNode
  { nodeName :: GHC.Name,
    nodeThing :: GHC.TyThing,
    nodeChildren :: [ExportedSymbolNode]
  }

resolveModule :: (MonadLore m) => NormalizedModuleName -> Maybe Text -> m (Maybe GHC.Module)
resolveModule moduleName maybePackageName =
  handle
    ( \(err :: SomeException) -> do
        Log.warn $ "Failed to resolve module " <> renderedModuleRequest <> ": " <> show err
        pure Nothing
    )
    do
      packageQualifier <- resolvePackageQualifier maybePackageName
      Just <$> GHC.lookupModule (mkGhcModuleName moduleName) packageQualifier
  where
    renderedModuleRequest =
      case maybePackageName of
        Nothing ->
          show moduleName
        Just packageName ->
          show moduleName <> " from package " <> show (T.unpack packageName)

    resolvePackageQualifier Nothing =
      pure Nothing
    resolvePackageQualifier (Just requestedPackageName) = do
      projectPackages <- discoverProject
      let projectPackageNames =
            map (T.pack . packageName) projectPackages
      pure $
        if requestedPackageName `elem` projectPackageNames
          then Nothing
          else Just (FastString.mkFastString (T.unpack requestedPackageName))

listSymbolsExportedByModule :: (MonadLore m) => GHC.Module -> m [ExportedSymbolNode]
listSymbolsExportedByModule mdl = do
  exportedAvailInfos <- loadExportedAvailInfosForModule mdl
  mapMaybeM buildRootNodeFromAvail exportedAvailInfos

filterExportedSymbolNodesByTypeHint :: NormalizedOccName -> [ExportedSymbolNode] -> [ExportedSymbolNode]
filterExportedSymbolNodesByTypeHint typeHint =
  mapMaybe filterExportedSymbolNodeByTypeHint
  where
    filterExportedSymbolNodeByTypeHint node =
      if nodeMatches || not (null matchingChildren)
        then Just node {nodeChildren = matchingChildren}
        else Nothing
      where
        nodeMatches = TyThing.isMentionedByOccName typeHint node.nodeThing
        matchingChildren = mapMaybe filterExportedSymbolNodeByTypeHint node.nodeChildren

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
      childNames = map Avail.greNamePrintableName (Avail.availSubordinateGreNames availInfo)
  maybeRootThing <- GHC.lookupName rootName
  case maybeRootThing of
    Nothing ->
      pure Nothing
    Just rootThing -> do
      childNodes <- mapMaybeM buildLeafNode childNames
      pure $
        Just
          ExportedSymbolNode
            { nodeName = rootName,
              nodeThing = rootThing,
              nodeChildren = childNodes
            }

loadExportedAvailInfosForModule :: (MonadLore m) => GHC.Module -> m [Avail.AvailInfo]
loadExportedAvailInfosForModule module_ = do
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
  where
    deduplicateAvailInfos =
      nubOrdOn Avail.availName

    deduplicateNames = nubOrd
