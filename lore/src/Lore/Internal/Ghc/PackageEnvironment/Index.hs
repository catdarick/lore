module Lore.Internal.Ghc.PackageEnvironment.Index
  ( buildPackageIndex,
    parsePackageEntries,
  )
where

import Data.Char (isSpace, toLower)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
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

data PackageIndexParseError
  = MissingPackageIndexField String
  | MissingPackageIndexUnitIdField
  | DuplicatePackageIndexField String [String]
  | InvalidPackageIndexExposedValue String
  | InvalidPackageIndexVisibilityValue String
  | EmptyPackageIndexRecord
  deriving (Eq, Show)

parsePackageEntries :: String -> Either String [PackageIndexEntry]
parsePackageEntries output =
  mapM parsePackageEntryAt (zip [1 :: Int ..] (splitInstalledPackageInfoRecords output))
  where
    parsePackageEntryAt :: (Int, String) -> Either String PackageIndexEntry
    parsePackageEntryAt (recordIndex, record) =
      case parsePackageEntry record of
        Left parseError ->
          Left
            ( "Failed to parse ghc-pkg dump record #"
                <> show recordIndex
                <> ". "
                <> renderPackageIndexParseError parseError
            )
        Right packageEntry ->
          Right packageEntry

parsePackageEntry :: String -> Either PackageIndexParseError PackageIndexEntry
parsePackageEntry record = do
  fields <- parseTopLevelPackageFields record
  packageName <- requireField "name" fields
  version <- requireField "version" fields
  unitId <- requireUnitId fields
  exposed <- requireExposed fields
  pure
    PackageIndexEntry
      { packageIndexPackageName = PackageNameText packageName,
        packageIndexUnitId = UnitIdText unitId,
        packageIndexVersion = version,
        packageIndexExposed = exposed
      }

parseTopLevelPackageFields :: String -> Either PackageIndexParseError (Map.Map String [String])
parseTopLevelPackageFields record
  | Map.null fields = Left EmptyPackageIndexRecord
  | otherwise = Right fields
  where
    fields =
      Map.fromListWith
        (<>)
        [ (fieldName, [trim fieldValue])
        | line <- lines record,
          not (isContinuationLine line),
          not (null (trim line)),
          Just (fieldName, fieldValue) <- [parseFieldLine line]
        ]

requireField :: String -> Map.Map String [String] -> Either PackageIndexParseError String
requireField fieldName fields =
  maybe (Left (MissingPackageIndexField fieldName)) Right =<< lookupSingleField fieldName fields

lookupSingleField :: String -> Map.Map String [String] -> Either PackageIndexParseError (Maybe String)
lookupSingleField fieldName fields =
  case Map.lookup fieldName fields of
    Nothing ->
      Right Nothing
    Just [] ->
      Right Nothing
    Just [fieldValue] ->
      Right (Just fieldValue)
    Just fieldValues ->
      Left (DuplicatePackageIndexField fieldName fieldValues)

requireUnitId :: Map.Map String [String] -> Either PackageIndexParseError String
requireUnitId fields =
  case requireField "id" fields of
    Right unitId -> Right unitId
    Left (MissingPackageIndexField "id") ->
      case requireField "unit-id" fields of
        Right unitId -> Right unitId
        Left (MissingPackageIndexField "unit-id") -> Left MissingPackageIndexUnitIdField
        Left parseError -> Left parseError
    Left parseError ->
      Left parseError

requireExposed :: Map.Map String [String] -> Either PackageIndexParseError Bool
requireExposed fields =
  case lookupSingleField "exposed" fields of
    Right (Just exposedValue) ->
      parseExposed exposedValue
    Right Nothing ->
      case lookupSingleField "visibility" fields of
        Right (Just visibilityValue) -> parseVisibility visibilityValue
        Right Nothing -> Right True
        Left parseError -> Left parseError
    Left parseError ->
      Left parseError

parseExposed :: String -> Either PackageIndexParseError Bool
parseExposed exposedValue =
  case map toLower (trim exposedValue) of
    "true" -> Right True
    "false" -> Right False
    _ -> Left (InvalidPackageIndexExposedValue exposedValue)

parseVisibility :: String -> Either PackageIndexParseError Bool
parseVisibility visibilityValue =
  case map toLower (trim visibilityValue) of
    "public" -> Right True
    "private" -> Right False
    _ -> Left (InvalidPackageIndexVisibilityValue visibilityValue)

parseFieldLine :: String -> Maybe (String, String)
parseFieldLine line =
  case break (== ':') line of
    (rawFieldName, ':' : rawFieldValue)
      | not (null trimmedFieldName) -> Just (map toLower trimmedFieldName, rawFieldValue)
      where
        trimmedFieldName = trim rawFieldName
    _ -> Nothing

isContinuationLine :: String -> Bool
isContinuationLine line =
  case line of
    firstChar : _ -> firstChar == ' ' || firstChar == '\t'
    [] -> False

renderPackageIndexParseError :: PackageIndexParseError -> String
renderPackageIndexParseError parseError =
  case parseError of
    MissingPackageIndexField fieldName ->
      "Missing required field '" <> fieldName <> "'."
    MissingPackageIndexUnitIdField ->
      "Missing required field 'id' or 'unit-id'."
    DuplicatePackageIndexField fieldName fieldValues ->
      "Duplicate field '" <> fieldName <> "' with values: " <> show fieldValues <> "."
    InvalidPackageIndexExposedValue exposedValue ->
      "Invalid value for field 'exposed': " <> show exposedValue <> ". Expected True or False."
    InvalidPackageIndexVisibilityValue visibilityValue ->
      "Invalid value for field 'visibility': " <> show visibilityValue <> ". Expected public or private."
    EmptyPackageIndexRecord ->
      "Record did not contain any top-level field lines."

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
        Map.fromListWith
          (<>)
          [ (entry.packageIndexPackageName, [entry])
          | entry <- packageEntries
          ]
    }

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

firstError :: String -> Either String a -> Either String a
firstError message =
  either (Left . ((message <> " ") <>)) Right
