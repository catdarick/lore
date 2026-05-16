module Lore.Internal.TemporalModules
  ( TemporalModule (..),
    createTemporalModule,
    listExistingTemporalModules,
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS (asks)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified GHC
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.Cache.Types (TemporalModulesRegistry (..))
import Lore.Monad (MonadLore)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (isRelative, normalise, takeBaseName, (</>))

data TemporalModule = TemporalModule
  { moduleName :: GHC.ModuleName,
    modulePath :: FilePath
  }
  deriving (Eq, Show)

createTemporalModule :: (MonadLore m) => m FilePath
createTemporalModule = do
  registryVar <- asks temporalModulesRegistryVar
  root <- asks projectRoot
  workDir <- asks sessionGhcWorkDir
  let temporalRootDir = resolveTemporalModulesRoot root workDir
  liftIO do
    MVar.modifyMVar registryVar \(TemporalModulesRegistry maybeModuleDir modulePaths) -> do
      moduleDir <- ensureTemporalModulesDirectory temporalRootDir maybeModuleDir
      createDirectoryIfMissing True moduleDir
      existingPaths <- keepExistingPaths modulePaths
      newPath <- createFreshTemporalModule moduleDir existingPaths
      pure (TemporalModulesRegistry (Just moduleDir) (existingPaths <> [newPath]), newPath)

listExistingTemporalModules :: (MonadLore m) => m [TemporalModule]
listExistingTemporalModules = do
  registryVar <- asks temporalModulesRegistryVar
  liftIO do
    MVar.modifyMVar registryVar \(TemporalModulesRegistry maybeModuleDir modulePaths) -> do
      existingPaths <- keepExistingPaths modulePaths
      pure (TemporalModulesRegistry maybeModuleDir existingPaths, map toTemporalModule existingPaths)

createFreshTemporalModule :: FilePath -> [FilePath] -> IO FilePath
createFreshTemporalModule moduleDir existingPaths =
  allocate (1 :: Int)
  where
    allocate :: Int -> IO FilePath
    allocate index = do
      let candidateModuleName = "Temp" <> show index
          candidatePath = normalise (moduleDir </> candidateModuleName <> ".hs")
      pathExists <- doesFileExist candidatePath
      if candidatePath `elem` existingPaths || pathExists
        then allocate (index + 1)
        else do
          TIO.writeFile candidatePath (renderModuleSource candidateModuleName)
          pure candidatePath

renderModuleSource :: String -> T.Text
renderModuleSource moduleName =
  T.concat
    [ "{-# OPTIONS_GHC -Wwarn #-}\n",
      "{-# OPTIONS_GHC -Wno-missing-home-modules #-}\n",
      "module ",
      T.pack moduleName,
      " where\n"
    ]

keepExistingPaths :: [FilePath] -> IO [FilePath]
keepExistingPaths modulePaths = do
  existingPaths <- filterM doesFileExist modulePaths
  pure (List.nub (map normalise existingPaths))

resolveTemporalModulesRoot :: FilePath -> FilePath -> FilePath
resolveTemporalModulesRoot root workDir =
  normalise (rootedWorkDir </> "tmp" </> "temporal-modules")
  where
    rootedWorkDir =
      if isRelative workDir
        then root </> workDir
        else workDir

ensureTemporalModulesDirectory :: FilePath -> Maybe FilePath -> IO FilePath
ensureTemporalModulesDirectory temporalRootDir maybeExistingDir =
  case maybeExistingDir of
    Just directoryPath ->
      pure directoryPath
    Nothing ->
      allocateUniqueTemporalDirectory temporalRootDir

allocateUniqueTemporalDirectory :: FilePath -> IO FilePath
allocateUniqueTemporalDirectory temporalRootDir = do
  suffix <- buildTemporalDirectorySuffix
  let candidatePath = temporalRootDir </> ("tmp-" <> suffix)
  exists <- doesDirectoryExist candidatePath
  if exists
    then allocateUniqueTemporalDirectory temporalRootDir
    else pure candidatePath

buildTemporalDirectorySuffix :: IO String
buildTemporalDirectorySuffix = do
  microsSinceEpoch <- round . (* 1_000_000) <$> getPOSIXTime
  pure (takeLast5 (toBase36 microsSinceEpoch))
  where
    takeLast5 value =
      let padded = replicate (max 0 (5 - length value)) '0' <> value
       in drop (length padded - 5) padded

toBase36 :: Integer -> String
toBase36 value
  | value <= 0 = "0"
  | otherwise = reverse (go value)
  where
    alphabet = "0123456789abcdefghijklmnopqrstuvwxyz"
    go number
      | number == 0 = []
      | otherwise =
          let (quotient, remainder) = quotRem number 36
           in alphabet !! fromInteger remainder : go quotient

toTemporalModule :: FilePath -> TemporalModule
toTemporalModule modulePath =
  TemporalModule
    { moduleName = GHC.mkModuleName (takeBaseName modulePath),
      modulePath = modulePath
    }
