module Lore.Internal.Ghc.PackageEnvironment.Index
  ( buildPackageIndex,
  )
where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Distribution.InstalledPackageInfo as CabalInstalled
import qualified Distribution.Package as CabalPackage
import qualified Distribution.Pretty as CabalPretty
import Lore.Internal.BuildTool.Environment (runProcessInWorkingDir)
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( PackageDbFlagTarget (PackageDbFlagsForGhcPkg),
    PackageDbStack (..),
    PackageIndex (..),
    PackageIndexEntry (..),
    PackageNameText (..),
    UnitIdText (..),
    renderPackageDbStackFlags,
  )

buildPackageIndex :: FilePath -> FilePath -> PackageDbStack -> IO (Either String PackageIndex)
buildPackageIndex projectRoot ghcPkgExecutable packageDbStack = do
  result <-
    runProcessInWorkingDir
      projectRoot
      ghcPkgExecutable
      ("dump" : renderPackageDbStackFlags PackageDbFlagsForGhcPkg packageDbStack)
  pure $ do
    output <- firstError "Failed to dump package index via ghc-pkg." result
    packageEntries <- parsePackageEntries output
    pure (toPackageIndex packageEntries)

parsePackageEntries :: String -> Either String [PackageIndexEntry]
parsePackageEntries output =
  mapM parsePackageEntry (splitInstalledPackageInfoRecords output)

parsePackageEntry :: String -> Either String PackageIndexEntry
parsePackageEntry record =
  case CabalInstalled.parseInstalledPackageInfo (BS8.pack record) of
    Left parseErrors ->
      Left
        ( "Failed to parse installed package info entry from ghc-pkg dump. Errors: "
            <> show (NE.toList parseErrors)
        )
    Right (_, installedPackageInfo) ->
      Right
        PackageIndexEntry
          { packageIndexPackageName =
              PackageNameText
                ( CabalPretty.prettyShow
                    (CabalPackage.packageName (CabalInstalled.sourcePackageId installedPackageInfo))
                ),
            packageIndexUnitId =
              UnitIdText (CabalPretty.prettyShow (CabalInstalled.installedUnitId installedPackageInfo)),
            packageIndexVersion =
              CabalPretty.prettyShow
                (CabalPackage.packageVersion (CabalInstalled.sourcePackageId installedPackageInfo)),
            packageIndexExposed = CabalInstalled.exposed installedPackageInfo
          }

splitInstalledPackageInfoRecords :: String -> [String]
splitInstalledPackageInfoRecords output =
  mapMaybe nonEmptyChunk (go [] [] (lines output))
  where
    go :: [String] -> [String] -> [String] -> [String]
    go currentChunk parsedChunks remainingLines =
      case remainingLines of
        [] ->
          reverse (unlines (reverse currentChunk) : parsedChunks)
        line : restLines
          | trim line == "---" ->
              go [] (unlines (reverse currentChunk) : parsedChunks) restLines
          | otherwise ->
              go (line : currentChunk) parsedChunks restLines

    nonEmptyChunk :: String -> Maybe String
    nonEmptyChunk chunk
      | all (`elem` [' ', '\t', '\n', '\r']) chunk = Nothing
      | otherwise = Just chunk

toPackageIndex :: [PackageIndexEntry] -> PackageIndex
toPackageIndex packageEntries =
  PackageIndex
    { packageIndexByUnitId =
        Map.fromList
          [ (entry.packageIndexUnitId, entry)
          | entry <- packageEntries
          ],
      packageIndexByPackageName =
        Map.fromListWith (<>)
          [ (entry.packageIndexPackageName, [entry])
          | entry <- packageEntries
          ]
    }

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

firstError :: String -> Either String a -> Either String a
firstError message =
  either (Left . ((message <> " ") <>)) Right
