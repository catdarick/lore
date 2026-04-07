{-# OPTIONS_GHC -Wno-orphans #-}

module Internal.Lookup (findSymbol, getSymbolInfo, getRootSymbolInfo, FooBar (..), SomeClass (..), resolveInstances, Instances (..), resolveInstancesDefinitions) where

import Control.Monad (forM)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified GHC
import qualified GHC.Core.FamInstEnv as GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Types.TyThing as GHC
import qualified Internal.Logger as Log
import Internal.Lookup.NameToInstances (getNameToInstancesIndex)
import Internal.Lookup.SymbolsMap (getSymbolsMap)
import Internal.Lookup.Types (ExportedSymbol (..), NameToInstancesIndex (..), SymbolsMap (..))
import Monad (MonadLore)
import Internal.Definition (resolveDefinitionSlice, DefinitionSlice)


class SomeClass a where
  someFunction :: a -> String

-- | Some FooBar data type to demonstrate instance lookup
data FooBar
  = Foo
  | Bar
  deriving (Show, Eq)

instance SomeClass FooBar where
  someFunction Foo = "This is Foo"
  someFunction Bar = "This is Bar"

findSymbol :: (MonadLore m) => Text -> m [ExportedSymbol]
findSymbol needle = do
  SymbolsMap symbolsMap <- getSymbolsMap
  case Map.lookup needle symbolsMap of
    Nothing -> pure []
    Just names -> pure names

data SymbolInfo = SymbolInfo
  { symbolName :: GHC.Name,
    definedIn :: GHC.Module,
    exportedFrom :: [GHC.Module],
    symbolType :: Maybe GHC.Type,
    associatedClassInstances :: [GHC.ClsInst],
    associatedFamilyInstances :: [GHC.FamInst]
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

getSymbolInfo :: (MonadLore m) => Text -> m [SymbolInfo]
getSymbolInfo = getSymbolInfo' False

getRootSymbolInfo :: (MonadLore m) => Text -> m [SymbolInfo]
getRootSymbolInfo = getSymbolInfo' True

getSymbolInfo' :: (MonadLore m) => Bool -> Text -> m [SymbolInfo]
getSymbolInfo' resolveRoot needle = do
  SymbolsMap symbolsMap <- getSymbolsMap
  case Map.lookup needle symbolsMap of
    Nothing -> pure []
    Just names ->
      catMaybes <$> do
        forM names (getExportedSymbolInfo resolveRoot)

getExportedSymbolInfo :: (MonadLore m) => Bool -> ExportedSymbol -> m (Maybe SymbolInfo)
getExportedSymbolInfo resolveRoot es = do
  case GHC.nameModule_maybe es.name of
    Nothing -> do
      Log.warn $ "Symbol " <> GHC.showSDocUnsafe (GHC.ppr es.name) <> " does not have an associated module. Skipping instance resolution."
      pure Nothing
    Just m -> do
      targetName <- if resolveRoot then resolveRootName es.name else pure es.name
      tyThing <- GHC.lookupName targetName
      let symbolType = case tyThing of
            Nothing -> Nothing
            Just tt -> case tt of
              GHC.AnId id' -> Just (GHC.idType id')
              _ -> Nothing
      instancesInfo <- resolveInstances targetName
      pure $
        Just
          SymbolInfo
            { symbolName = targetName,
              definedIn = m,
              exportedFrom = es.exportedFrom,
              symbolType = symbolType,
              associatedClassInstances = maybe [] classInstances instancesInfo,
              associatedFamilyInstances = maybe [] familyInstances instancesInfo
            }

data Instances = Instances
  { classInstances :: [GHC.ClsInst],
    familyInstances :: [GHC.FamInst]
  }

resolveInstances :: (MonadLore m) => GHC.Name -> m (Maybe Instances)
resolveInstances name = do
  NameToInstancesIndex nameToInstancesIndex <- getNameToInstancesIndex
  case GHC.lookupUFM nameToInstancesIndex name of
    Nothing -> pure Nothing
    Just (clsInsts, famInsts) -> pure $ Just (Instances clsInsts famInsts)

resolveInstancesDefinitions :: (MonadLore m) => GHC.Name -> m [DefinitionSlice]
resolveInstancesDefinitions name = do
  NameToInstancesIndex nameToInstancesIndex <- getNameToInstancesIndex
  case GHC.lookupUFM nameToInstancesIndex name of
    Nothing -> pure []
    Just (clsInsts, famInsts) -> do
      let allNames = [GHC.getName clsInst | clsInst <- clsInsts] ++ [GHC.getName famInst | famInst <- famInsts]
      resolved <- mapM resolveDefinitionSlice allNames
      pure $ mapMaybe id resolved

resolveRootName :: (MonadLore m) => GHC.Name -> m GHC.Name
resolveRootName name = do
  mTyThing <- GHC.lookupName name
  pure $
    maybe name (GHC.getName . rootTyThing) mTyThing
  where
    rootTyThing :: GHC.TyThing -> GHC.TyThing
    rootTyThing tyThing =
      maybe tyThing rootTyThing (GHC.tyThingParent_maybe tyThing)
