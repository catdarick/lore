module Lore.Internal.HomeModules.SyntheticMain
  ( entryHomeModuleSource,
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import Data.Char (isAlpha, isAlphaNum, isSpace, ord, toUpper)
import Data.List (stripPrefix)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified GHC
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.Cache.Types (GeneratedMainModule (..), GeneratedMainModuleKey (..), GeneratedMainModulesRegistry (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Text.ParserCombinators.ReadP (readP_to_S, skipSpaces)

entryHomeModuleSource :: (MonadLore m) => String -> String -> FilePath -> m FilePath
entryHomeModuleSource packageName componentName sourcePath = do
  source <- liftIO (readFile sourcePath)
  if inferSourceModuleName source == GHC.mkModuleName "Main"
    then synthesizeMainModule packageName componentName sourcePath source
    else pure sourcePath

synthesizeMainModule :: (MonadLore m) => String -> String -> FilePath -> String -> m FilePath
synthesizeMainModule packageName componentName originalPath source = do
  ghcWorkDir <- asks sessionGhcWorkDir
  registryVar <- asks generatedMainModulesRegistryVar
  let homeModuleKey =
        GeneratedMainModuleKey
          { generatedMainPackageName = packageName,
            generatedMainComponentName = componentName,
            generatedMainOriginalPath = originalPath
          }
  (generatedMainModule, wasNew) <-
    liftIO $
      MVar.modifyMVar registryVar \(GeneratedMainModulesRegistry homeModulesByKey) -> do
        let (nextHomeModule, createdNewHomeModule) =
              case Map.lookup homeModuleKey homeModulesByKey of
                Just existingHomeModule -> (existingHomeModule, False)
                Nothing -> (mkGeneratedMainModule ghcWorkDir homeModuleKey, True)
            nextRegistry =
              GeneratedMainModulesRegistry
                (Map.insert homeModuleKey nextHomeModule homeModulesByKey)
        pure (nextRegistry, (nextHomeModule, createdNewHomeModule))

  let rewrittenSource = rewriteModuleHeader generatedMainModule.generatedMainModuleName originalPath source
  liftIO do
    createDirectoryIfMissing True (takeDirectory generatedMainModule.generatedMainPath)
    writeFile generatedMainModule.generatedMainPath rewrittenSource

  Log.debug $
    (if wasNew then "Created" else "Updated")
      <> " synthetic home module "
      <> generatedMainModule.generatedMainPath
      <> " for colliding module Main at "
      <> originalPath

  pure generatedMainModule.generatedMainPath

mkGeneratedMainModule ::
  FilePath ->
  GeneratedMainModuleKey ->
  GeneratedMainModule
mkGeneratedMainModule ghcWorkDir homeModuleKey =
  GeneratedMainModule
    { generatedMainModuleName = moduleName,
      generatedMainPath = generatedPath
    }
  where
    moduleName =
      syntheticModuleName
        homeModuleKey.generatedMainPackageName
        homeModuleKey.generatedMainComponentName
        homeModuleKey.generatedMainOriginalPath
    relPath = moduleNameToRelativePath moduleName
    generatedPath = ghcWorkDir </> "generated-main-modules" </> relPath

syntheticModuleName :: String -> String -> FilePath -> String
syntheticModuleName packageName componentName originalPath =
  "Main_"
    <> sanitizeModuleSegment packageName
    <> "_"
    <> sanitizeModuleSegment componentName
    <> "_H"
    <> show (stablePathFingerprint originalPath)

moduleNameToRelativePath :: String -> FilePath
moduleNameToRelativePath moduleName =
  moduleName <> ".hs"

sanitizeModuleSegment :: String -> String
sanitizeModuleSegment raw =
  case mapped of
    [] -> "Main"
    firstChar : rest
      | isAlpha firstChar -> toUpper firstChar : rest
      | otherwise -> 'M' : firstChar : rest
  where
    mapped = map sanitizeChar raw
    sanitizeChar c
      | isAlphaNum c = c
      | otherwise = '_'

stablePathFingerprint :: FilePath -> Int
stablePathFingerprint =
  abs . List.foldl' step 5381
  where
    step acc c = acc * 33 + ord c

inferSourceModuleName :: String -> GHC.ModuleName
inferSourceModuleName source =
  case splitAtModuleLine (lines source) of
    Just (_, (_, moduleName, _), _) -> moduleName
    Nothing -> GHC.mkModuleName "Main"

type ParsedModuleLine = (String, GHC.ModuleName, String)

parseModuleLine :: String -> Maybe ParsedModuleLine
parseModuleLine rawLine = do
  let indentation = takeWhile isSpace rawLine
      withoutIndentation = dropWhile isSpace rawLine
  moduleRemainder <- stripModuleKeyword withoutIndentation
  (moduleName, remainder) <- parseLeadingModuleName moduleRemainder
  pure (indentation, moduleName, remainder)

stripModuleKeyword :: String -> Maybe String
stripModuleKeyword line = do
  afterKeyword <- stripPrefix "module" line
  case afterKeyword of
    firstChar : _ | isSpace firstChar -> Just afterKeyword
    _ -> Nothing

parseLeadingModuleName :: String -> Maybe (GHC.ModuleName, String)
parseLeadingModuleName =
  pickLast . readP_to_S parser
  where
    parser = do
      skipSpaces
      moduleName <- GHC.parseModuleName
      pure moduleName

pickLast :: [a] -> Maybe a
pickLast =
  List.foldl' (\_ value -> Just value) Nothing

rewriteModuleHeader :: String -> FilePath -> String -> String
rewriteModuleHeader syntheticName originalPath source =
  case splitAtModuleLine (lines source) of
    Just (before, moduleLine, after) ->
      unlines (before <> [replaceModuleName syntheticName moduleLine, linePragma originalPath] <> after)
    Nothing ->
      unlines
        [ "module " <> syntheticName <> " (main) where",
          linePragma originalPath,
          source
        ]

linePragma :: FilePath -> String
linePragma originalPath =
  "{-# LINE 1 \"" <> originalPath <> "\" #-}"

splitAtModuleLine :: [String] -> Maybe ([String], ParsedModuleLine, [String])
splitAtModuleLine =
  go []
  where
    go _ [] = Nothing
    go prefix (line : rest) =
      case parseModuleLine line of
        Just parsedLine -> Just (reverse prefix, parsedLine, rest)
        Nothing -> go (line : prefix) rest

replaceModuleName :: String -> ParsedModuleLine -> String
replaceModuleName newModuleName (indentation, _, remainder) =
  indentation <> "module " <> newModuleName <> remainder
