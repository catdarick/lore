module TestSupport
  ( fixtureLore,
    fixtureLoreAt,
    fixtureLoreAtWithConfig,
    withFixtureCopy,
    findSymbols,
    findRootSymbols,
    lookupRootSymbolInfo,
    lookupRootSymbolChains,
    listExportedSymbolsByModule,
    filterExportedSymbolNodesByTypeHint,
  )
where

import Control.Exception (bracket)
import Data.List (find, isInfixOf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified GHC.Plugins as GHC
import qualified Lore
import Lore.Logger (LoggerHandle, noLogHandle)
import Lore.Monad (LoreMonadT)
import Lore.Session (SessionConfig, runLore)
import qualified Lore.Session as Session
import System.Directory (copyFile, createDirectory, createDirectoryIfMissing, doesDirectoryExist, listDirectory, makeAbsolute, removeFile, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

fixtureLore :: LoreMonadT IO a -> IO a
fixtureLore action = do
  fixtureRoot <- makeAbsolute ("test" </> "fixtures" </> "demo")
  fixtureLoreAt fixtureRoot action

fixtureLoreAt :: FilePath -> LoreMonadT IO a -> IO a
fixtureLoreAt fixtureRoot action =
  fixtureLoreAtWithConfig
    (sessionConfigWithLogger noLogHandle)
    fixtureRoot
    action

fixtureLoreAtWithConfig :: SessionConfig -> FilePath -> LoreMonadT IO a -> IO a
fixtureLoreAtWithConfig sessionConfig fixtureRoot action =
  withClearedGhcEnvironment $
    runLore
      sessionConfig
        { Session.projectRoot = fixtureRoot,
          Session.ghcWorkDir = fixtureRoot </> ".lore-work-test"
        }
      action

sessionConfigWithLogger :: LoggerHandle -> SessionConfig
sessionConfigWithLogger loggerHandle =
  Session.SessionConfig
    { Session.projectRoot = ".",
      Session.ghcWorkDir = ".lore-work",
      Session.loggerHandle = loggerHandle,
      Session.customPrelude = Nothing,
      Session.parallelWorkersLimit = Session.WorkersAsNumProcessors,
      Session.isTestSuiteFunctionalityRequired = False
    }

withFixtureCopy :: (FilePath -> IO a) -> IO a
withFixtureCopy action = do
  fixtureRoot <- makeAbsolute ("test" </> "fixtures" </> "demo")
  bracket (prepareFixtureCopy fixtureRoot) removePathForcibly action
  where
    prepareFixtureCopy fixtureRoot = do
      fixtureCopyRoot <- createTempDirectoryPath
      copyDirectoryRecursive fixtureRoot fixtureCopyRoot
      pure fixtureCopyRoot

createTempDirectoryPath :: IO FilePath
createTempDirectoryPath = do
  timestamp <- round . (* 1_000_000) <$> getPOSIXTime
  (tempFilePath, handle) <- openTempFile "/tmp" ("lore-fixture-" <> show (timestamp :: Integer))
  hClose handle
  removeFile tempFilePath
  createDirectory tempFilePath
  pure tempFilePath

copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive sourceDir targetDir = do
  createDirectoryIfMissing True targetDir
  entries <- listDirectory sourceDir
  mapM_ copyEntry entries
  where
    copyEntry entryName = do
      let sourcePath = sourceDir </> entryName
          targetPath = targetDir </> entryName
      isDirectory <- doesDirectoryExist sourcePath
      if isDirectory
        then copyDirectoryRecursive sourcePath targetPath
        else copyFile sourcePath targetPath

withClearedGhcEnvironment :: IO a -> IO a
withClearedGhcEnvironment action =
  bracket (lookupEnv "GHC_ENVIRONMENT" <* unsetEnv "GHC_ENVIRONMENT") restore (const action)
  where
    restore =
      maybe (pure ()) (setEnv "GHC_ENVIRONMENT")

findSymbols :: (Lore.MonadLore m) => Text -> m [Lore.Symbol]
findSymbols query =
  Set.toList <$> Lore.findMatchingSymbols (Lore.parseAndNormalizeName query)

findRootSymbols :: (Lore.MonadLore m) => Text -> m [Lore.Symbol]
findRootSymbols query = do
  symbols <- findSymbols query
  pathsToRoot <- mapM (Lore.resolvePathToRoot . (.name)) symbols
  preferredRootNames <- pickPreferredRootNames (map (NE.last . (.unPathToRoot)) pathsToRoot)
  pure (catMaybes (map (`findSymbolByName` symbols) preferredRootNames))
  where
    findSymbolByName targetName =
      find ((== targetName) . (.name))

    pickPreferredRootNames rootNames =
      concat <$> mapM pickPreferredByOccName (Map.elems groupedByOccName)
      where
        groupedByOccName =
          Map.fromListWith
            (<>)
            [ (Lore.occName (Lore.parseAndNormalizeName (T.pack (GHC.getOccString name))), [name])
            | name <- rootNames
            ]

    pickPreferredByOccName [] =
      pure []
    pickPreferredByOccName namesForOcc = do
      categorizedNames <- mapM classify namesForOcc
      let nonValueNames =
            [ name
            | (name, category) <- categorizedNames,
              category /= Lore.SymbolValue
            ]
      pure $
        case nonValueNames of
          preferredName : _ -> [preferredName]
          [] -> take 1 namesForOcc

    classify name = do
      maybeInfo <- Lore.lookupSymbolInfo name
      pure (name, maybe Lore.SymbolUnknown (Lore.classifySymbolCategory . Lore.symbolThing) maybeInfo)

lookupRootSymbolInfo :: (Lore.MonadLore m) => Text -> m [Lore.SymbolInfo]
lookupRootSymbolInfo query = do
  rootSymbols <- findRootSymbols query
  catMaybes <$> mapM (Lore.lookupSymbolInfo . (.name)) rootSymbols

lookupRootSymbolChains :: (Lore.MonadLore m) => Text -> m [[GHC.Name]]
lookupRootSymbolChains query = do
  symbols <- findSymbols query
  pathsToRoot <- mapM (Lore.resolvePathToRoot . (.name)) symbols
  let mergedPaths = mergePathsToRootOn renderName pathsToRoot
  pure (map (NE.toList . (.unPathToRoot)) mergedPaths)
  where
    renderName name =
      case GHC.nameModule_maybe name of
        Nothing ->
          "<no-module>." <> GHC.getOccString name
        Just module_ ->
          GHC.moduleNameString (GHC.moduleName module_) <> "." <> GHC.getOccString name

mergePathsToRootOn :: (Ord b) => (a -> b) -> [Lore.PathToRoot a] -> [Lore.PathToRoot a]
mergePathsToRootOn getKey paths =
  Map.elems (Map.fromListWith mergePaths pathPairs)
  where
    pathPairs =
      [ (getKey (NE.last path.unPathToRoot), path)
      | path <- paths
      ]

    mergePaths path1 path2 =
      let values1 = NE.toList path1.unPathToRoot
          values2 = NE.toList path2.unPathToRoot
          keys1 = map getKey values1
          keys2 = map getKey values2
       in if
            | keys1 `isInfixOf` keys2 -> path2
            | keys2 `isInfixOf` keys1 -> path1
            | otherwise ->
                let (primaryValues, secondaryValues) =
                      if length values1 >= length values2
                        then (values1, values2)
                        else (values2, values1)
                    primaryKeys = map getKey primaryValues
                    secondaryUniquePart = filter (\value -> getKey value `notElem` primaryKeys) secondaryValues
                 in Lore.PathToRoot (NE.fromList (secondaryUniquePart <> primaryValues))

listExportedSymbolsByModule :: (Lore.MonadLore m) => Text -> Maybe Text -> m [Lore.ExportedSymbolNode]
listExportedSymbolsByModule moduleName maybePackageName = do
  let normalizedModuleName =
        Lore.mkNormalizedModuleName moduleName
  maybeModule <- Lore.resolveModule normalizedModuleName maybePackageName
  maybe (pure []) Lore.listSymbolsExportedByModule maybeModule

filterExportedSymbolNodesByTypeHint :: Text -> [Lore.ExportedSymbolNode] -> [Lore.ExportedSymbolNode]
filterExportedSymbolNodesByTypeHint typeHint =
  Lore.filterExportedSymbolNodesByTypeHint (Lore.occName (Lore.parseAndNormalizeName typeHint))
