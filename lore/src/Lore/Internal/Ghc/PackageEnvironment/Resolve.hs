module Lore.Internal.Ghc.PackageEnvironment.Resolve
  ( resolveDependencyPackageEnvironment,
    packageEnvironmentCacheKey,
    renderPackageResolutionError,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( GhcEnvironmentSnapshot (..),
    PackageDb (..),
    PackageDbStack (..),
    PackageIndex (..),
    PackageIndexEntry (..),
    PackageNameText (..),
    PackageResolutionError (..),
    ResolvedPackageEnvironment (..),
    UnitIdText (..),
  )

resolveDependencyPackageEnvironment ::
  GhcEnvironmentSnapshot ->
  Set.Set String ->
  Either PackageResolutionError ResolvedPackageEnvironment
resolveDependencyPackageEnvironment snapshot dependencyNames = do
  resolvedDependencyUnitIds <-
    fmap Set.unions (mapM resolveDependency (Set.toAscList dependencyNames))
  pure
    ResolvedPackageEnvironment
      { resolvedPackageDbStack = snapshot.ghcEnvironmentPackageDbStack,
        resolvedExposedUnitIds = resolvedDependencyUnitIds
      }
  where
    resolveDependency :: String -> Either PackageResolutionError (Set.Set UnitIdText)
    resolveDependency dependencyName =
      let packageName = PackageNameText dependencyName
       in case lookupSelectedUnitId packageName of
            Just selectedUnitIds
              | not (Set.null selectedUnitIds) ->
                  Right selectedUnitIds
            _ ->
              case lookupPackageEntries packageName of
                Nothing -> Left (MissingPackage packageName)
                Just [] -> Left (MissingPackage packageName)
                Just [entry] -> Right (Set.singleton entry.packageIndexUnitId)
                Just entries ->
                  Left
                    ( AmbiguousPackage
                        packageName
                        (List.sort (map (.packageIndexUnitId) entries))
                    )

    lookupSelectedUnitId packageName =
      Map.lookup packageName snapshot.ghcEnvironmentSelectedUnitIdsByPackageName

    lookupPackageEntries packageName =
      Map.lookup packageName snapshot.ghcEnvironmentPackageIndex.packageIndexByPackageName

packageEnvironmentCacheKey :: ResolvedPackageEnvironment -> Set.Set String
packageEnvironmentCacheKey environment =
  Set.fromList
    ( renderPackageDbKeys environment.resolvedPackageDbStack.unPackageDbStack
        <> map renderUnitIdKey (Set.toAscList environment.resolvedExposedUnitIds)
    )
  where
    renderPackageDbKeys packageDbs =
      [ "package-db:"
          <> show index
          <> ":"
          <> renderPackageDbKey packageDb
      | (index, packageDb) <- zip [0 :: Int ..] packageDbs
      ]

    renderPackageDbKey packageDb =
      case packageDb of
        GlobalPackageDb -> "global"
        UserPackageDb -> "user"
        SpecificPackageDb dbPath -> "path:" <> dbPath

    renderUnitIdKey unitId =
      "package:id:" <> unitId.unUnitIdText

renderPackageResolutionError :: PackageResolutionError -> String
renderPackageResolutionError packageResolutionError =
  case packageResolutionError of
    MissingPackage packageName ->
      "Missing dependency package in resolved package DB stack: "
        <> packageName.unPackageNameText
        <> "."
    AmbiguousPackage packageName matchingUnitIds ->
      "Ambiguous dependency package '"
        <> packageName.unPackageNameText
        <> "'. Matching unit IDs: "
        <> show (map (.unUnitIdText) matchingUnitIds)
        <> "."
