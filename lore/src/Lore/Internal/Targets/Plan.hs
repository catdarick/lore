module Lore.Internal.Targets.Plan
  ( TargetsPlan (..),
    TargetKey (..),
    ComponentSpecificOptions (..),
    prepareTargetsPlan,
    commonComponentLanguage,
    commonSetIntersection,
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad (foldM, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import Data.Char (isAlpha, isAlphaNum, isSpace, ord, toUpper)
import Data.List (foldl', isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..), Language (..), setGhcOptionsAndExtensions)
import Lore.Internal.Package (ComponentData (..), PackageData (..), commonSetIntersection, componentMainModulePathCandidates, defaultExtensions, firstExistingPath)
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.Session.Cache.Types (GeneratedMainTarget (..), GeneratedMainTargetKey (..), GeneratedMainTargetsRegistry (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

data TargetKey
  = TargetModuleName GHC.ModuleName
  | TargetSourceFile FilePath
  deriving (Eq, Ord, Show)

data TargetsPlan = TargetsPlan
  { commonLanguage :: Maybe Language,
    commonExtensions :: Set.Set Extension,
    commonGhcOptions :: Set.Set GhcOption,
    targetsWithComponentOptions :: Map.Map TargetKey ComponentSpecificOptions
  }

data ComponentSpecificOptions = ComponentSpecificOptions
  { language :: Maybe Language,
    extensions :: Set.Set Extension,
    ghcOptions :: Set.Set GhcOption,
    baseDynFlags :: GHC.DynFlags
  }

prepareTargetsPlan :: (MonadLore m) => [PackageData] -> m TargetsPlan
prepareTargetsPlan packages = do
  sessionDynFlags <- GHC.getSessionDynFlags
  let rootedComponents =
        [ (pkg.packageName, pkg.packageRoot, component)
        | pkg <- packages,
          component <- pkg.components
        ]
      components = map (\(_, _, component) -> component) rootedComponents
      commonLanguage = commonComponentLanguage components
      commonExtensions = commonSetIntersection (map defaultExtensions components)
      commonGhcOptions = commonSetIntersection (map (.ghcOptions) components)

  targetsWithComponentOptionsByComponent <- forM rootedComponents \(packageName, packageRoot, component) -> do
    componentFlags <-
      setGhcOptionsAndExtensions
        component.language
        (Set.toList component.ghcOptions)
        (Set.toList component.defaultExtensions)
        sessionDynFlags
    let componentSpecificExtensions = component.defaultExtensions Set.\\ commonExtensions
        componentSpecificGhcOptions = component.ghcOptions Set.\\ commonGhcOptions
        componentSpecificLanguage =
          if component.language == commonLanguage
            then Nothing
            else component.language
        componentSpecificOptions =
          ComponentSpecificOptions
            { language = componentSpecificLanguage,
              extensions = componentSpecificExtensions,
              ghcOptions = componentSpecificGhcOptions,
              baseDynFlags = componentFlags
            }
    componentTargets <- targetsForComponent packageName packageRoot component
    pure $ Map.fromSet (const componentSpecificOptions) componentTargets
  targetsWithComponentOptions <- mergeTargetComponentOptions targetsWithComponentOptionsByComponent
  pure
    TargetsPlan
      { commonLanguage = commonLanguage,
        commonExtensions = commonExtensions,
        commonGhcOptions = commonGhcOptions,
        targetsWithComponentOptions = targetsWithComponentOptions
      }

commonComponentLanguage :: [ComponentData] -> Maybe Language
commonComponentLanguage [] = Nothing
commonComponentLanguage (component : restComponents)
  | all ((== component.language) . (.language)) restComponents = component.language
  | otherwise = Nothing

targetsForComponent :: (MonadLore m) => String -> FilePath -> ComponentData -> m (Set.Set TargetKey)
targetsForComponent packageName packageRoot component = do
  maybeMainSourcePath <- firstExistingPath (componentMainModulePathCandidates packageRoot component)
  maybeEntryTarget <- mapM (entryTargetSource packageName component.componentName) maybeMainSourcePath
  pure $
    Set.map TargetModuleName component.modules
      <> case maybeEntryTarget of
        Nothing -> Set.empty
        Just entrySourcePath -> Set.singleton (TargetSourceFile entrySourcePath)

entryTargetSource :: (MonadLore m) => String -> String -> FilePath -> m FilePath
entryTargetSource packageName componentName sourcePath = do
  source <- liftIO (readFile sourcePath)
  let declaredModule = inferSourceModuleName source
  if declaredModule == "Main"
    then synthesizeMainTarget packageName componentName sourcePath source
    else pure sourcePath

synthesizeMainTarget :: (MonadLore m) => String -> String -> FilePath -> String -> m FilePath
synthesizeMainTarget packageName componentName originalPath source = do
  ghcWorkDir <- asks sessionGhcWorkDir
  registryVar <- asks generatedMainTargetsRegistryVar
  let targetKey =
        GeneratedMainTargetKey
          { generatedMainPackageName = packageName,
            generatedMainComponentName = componentName,
            generatedMainOriginalPath = originalPath
          }
  (generatedTarget, wasNew) <-
    liftIO $
      MVar.modifyMVar registryVar \(GeneratedMainTargetsRegistry targetsByKey) -> do
        let nextTarget =
              case Map.lookup targetKey targetsByKey of
                Just existingTarget ->
                  existingTarget
                Nothing ->
                  mkGeneratedMainTarget ghcWorkDir targetKey
            updatedTargetsByKey = Map.insert targetKey nextTarget targetsByKey
            nextRegistry = GeneratedMainTargetsRegistry updatedTargetsByKey
            createdNewTarget = not (Map.member targetKey targetsByKey)
        pure (nextRegistry, (nextTarget, createdNewTarget))
  let rewrittenSource = rewriteModuleHeader generatedTarget.generatedMainModuleName originalPath source
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory generatedTarget.generatedMainPath)
    writeFile generatedTarget.generatedMainPath rewrittenSource
  Log.debug $
    (if wasNew then "Created" else "Updated")
      <> " synthetic entry target "
      <> generatedTarget.generatedMainPath
      <> " for colliding module Main at "
      <> originalPath
  pure generatedTarget.generatedMainPath

mkGeneratedMainTarget ::
  FilePath ->
  GeneratedMainTargetKey ->
  GeneratedMainTarget
mkGeneratedMainTarget ghcWorkDir targetKey =
  GeneratedMainTarget
    { generatedMainModuleName = moduleName,
      generatedMainPath = generatedPath
    }
  where
    moduleName =
      syntheticModuleName
        targetKey.generatedMainPackageName
        targetKey.generatedMainComponentName
        targetKey.generatedMainOriginalPath
    relPath = moduleNameToRelativePath moduleName
    generatedPath = ghcWorkDir </> "generated-main-targets" </> relPath

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
  abs . foldl' step 5381
  where
    step acc c = acc * 33 + ord c

inferSourceModuleName :: String -> String
inferSourceModuleName source =
  case firstDeclaredModuleName source of
    Just moduleName -> moduleName
    Nothing -> "Main"

firstDeclaredModuleName :: String -> Maybe String
firstDeclaredModuleName contents = do
  moduleLine <- listToMaybe (dropWhile (not . isModuleLine) (lines contents))
  parseModuleName moduleLine

isModuleLine :: String -> Bool
isModuleLine rawLine =
  "module " `isPrefixOf` dropWhile isSpace rawLine

parseModuleName :: String -> Maybe String
parseModuleName rawLine =
  let afterKeyword = dropWhile isSpace (drop (length ("module " :: String)) (dropWhile isSpace rawLine))
      moduleName = takeWhile isModuleChar afterKeyword
   in if null moduleName then Nothing else Just moduleName

isModuleChar :: Char -> Bool
isModuleChar c = isAlphaNum c || c == '_' || c == '.'

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

splitAtModuleLine :: [String] -> Maybe ([String], String, [String])
splitAtModuleLine =
  go []
  where
    go _ [] = Nothing
    go prefix (line : rest)
      | isModuleLine line = Just (reverse prefix, line, rest)
      | otherwise = go (line : prefix) rest

replaceModuleName :: String -> String -> String
replaceModuleName newModuleName rawLine =
  indentation <> "module " <> newModuleName <> remainder
  where
    indentation = takeWhile isSpace rawLine
    withoutIndent = dropWhile isSpace rawLine
    afterKeyword = drop (length ("module " :: String)) withoutIndent
    afterName = dropWhile isModuleChar (dropWhile isSpace afterKeyword)
    remainder = afterName

mergeTargetComponentOptions ::
  (MonadLore m) =>
  [Map.Map TargetKey ComponentSpecificOptions] ->
  m (Map.Map TargetKey ComponentSpecificOptions)
mergeTargetComponentOptions =
  foldM mergeComponentMap Map.empty
  where
    mergeComponentMap merged componentMap =
      foldM mergeTargetOptions merged (Map.toList componentMap)

    mergeTargetOptions merged (targetKey, newOptions) =
      case Map.lookup targetKey merged of
        Nothing ->
          pure (Map.insert targetKey newOptions merged)
        Just existingOptions
          | componentOptionsEquivalent existingOptions newOptions ->
              pure merged
          | otherwise -> do
              Log.warn $
                "Target planning conflict for "
                  <> renderTargetKey targetKey
                  <> ": component-specific options differ across components; keeping the first mapping."
              pure merged

renderTargetKey :: TargetKey -> String
renderTargetKey targetKey =
  case targetKey of
    TargetModuleName moduleName ->
      "module " <> GHC.moduleNameString moduleName
    TargetSourceFile sourcePath ->
      "source file " <> sourcePath

componentOptionsEquivalent :: ComponentSpecificOptions -> ComponentSpecificOptions -> Bool
componentOptionsEquivalent left right =
  left.language == right.language
    && left.extensions == right.extensions
    && left.ghcOptions == right.ghcOptions
