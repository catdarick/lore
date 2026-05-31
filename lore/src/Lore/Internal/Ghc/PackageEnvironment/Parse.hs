module Lore.Internal.Ghc.PackageEnvironment.Parse
  ( parseGhcEnvironmentFile,
    packagePathToPackageDbStack,
    defaultPackageDbStack,
  )
where

import Data.Char (isSpace)
import qualified Data.Set as Set
import qualified Data.Text as T
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( PackageDb (..),
    PackageDbStack (..),
    ParsedGhcEnvironmentFile (..),
    UnitIdText (..),
  )
import System.FilePath (isRelative, normalise, searchPathSeparator, takeDirectory, (</>))

parseGhcEnvironmentFile :: FilePath -> T.Text -> Either String ParsedGhcEnvironmentFile
parseGhcEnvironmentFile environmentFilePath content =
  toParsedEnvironment <$> foldl parseLine (Right initialParseState) (zip [1 :: Int ..] (T.lines content))
  where
    parseLine :: Either String ParseState -> (Int, T.Text) -> Either String ParseState
    parseLine acc (lineNumber, rawLine) = do
      parseState <- acc
      case parseDirective lineNumber rawLine of
        Nothing -> pure parseState
        Just (Left err) -> Left err
        Just (Right directive) -> Right (applyDirective parseState directive)

    parseDirective :: Int -> T.Text -> Maybe (Either String ParsedDirective)
    parseDirective lineNumber rawLine
      | T.null line = Nothing
      | "--" `T.isPrefixOf` line = Nothing
      | line == "clear-package-db" = Just (Right ClearPackageDbDirective)
      | line == "global-package-db" = Just (Right GlobalPackageDbDirective)
      | line == "user-package-db" = Just (Right UserPackageDbDirective)
      | otherwise =
          case parseKeywordArgument "package-db" line of
            Just parsedArgument ->
              Just do
                packageDbPath <- parsedArgument
                pure (SpecificPackageDbDirective (resolvePackageDbPath packageDbPath))
            Nothing ->
              case parseKeywordArgument "package-id" line of
                Just parsedArgument ->
                  Just do
                    packageId <- parsedArgument
                    pure (PackageIdDirective (UnitIdText packageId))
                Nothing ->
                  unsupportedDirective
      where
        line = T.strip rawLine

        unsupportedDirective =
          Just
            ( Left
                ( "Unsupported GHC environment directive at line "
                    <> show lineNumber
                    <> ": "
                    <> T.unpack line
                )
            )

    resolvePackageDbPath :: FilePath -> FilePath
    resolvePackageDbPath packageDbPath
      | isRelative packageDbPath = normalise (takeDirectory environmentFilePath </> packageDbPath)
      | otherwise = normalise packageDbPath

defaultPackageDbStack :: PackageDbStack
defaultPackageDbStack =
  PackageDbStack [GlobalPackageDb, UserPackageDb]

packagePathToPackageDbStack :: String -> PackageDbStack
packagePathToPackageDbStack rawPackagePath
  | null trimmedPackagePath = defaultPackageDbStack
  | otherwise =
      PackageDbStack
        ( concatMap
            parsePathEntry
            (splitPackagePathPreservingEmptyEntries trimmedPackagePath)
        )
  where
    trimmedPackagePath = trim rawPackagePath

    parsePathEntry :: String -> [PackageDb]
    parsePathEntry entry
      | null entry = defaultPackageDbStack.unPackageDbStack
      | otherwise = [SpecificPackageDb (normalise entry)]

data ParseState = ParseState
  { parseStatePackageDbStack :: [PackageDb],
    parseStateSelectedUnitIds :: Set.Set UnitIdText
  }

initialParseState :: ParseState
initialParseState =
  ParseState
    { parseStatePackageDbStack = defaultPackageDbStack.unPackageDbStack,
      parseStateSelectedUnitIds = Set.empty
    }

data ParsedDirective
  = ClearPackageDbDirective
  | GlobalPackageDbDirective
  | UserPackageDbDirective
  | SpecificPackageDbDirective FilePath
  | PackageIdDirective UnitIdText

applyDirective :: ParseState -> ParsedDirective -> ParseState
applyDirective parseState parsedDirective =
  case parsedDirective of
    ClearPackageDbDirective ->
      parseState
        { parseStatePackageDbStack = []
        }
    GlobalPackageDbDirective ->
      parseState
        { parseStatePackageDbStack = parseState.parseStatePackageDbStack <> [GlobalPackageDb]
        }
    UserPackageDbDirective ->
      parseState
        { parseStatePackageDbStack = parseState.parseStatePackageDbStack <> [UserPackageDb]
        }
    SpecificPackageDbDirective packageDbPath ->
      parseState
        { parseStatePackageDbStack = parseState.parseStatePackageDbStack <> [SpecificPackageDb packageDbPath]
        }
    PackageIdDirective packageId ->
      parseState
        { parseStateSelectedUnitIds = Set.insert packageId parseState.parseStateSelectedUnitIds
        }

toParsedEnvironment :: ParseState -> ParsedGhcEnvironmentFile
toParsedEnvironment parseState =
  ParsedGhcEnvironmentFile
    { parsedEnvPackageDbStack = PackageDbStack parseState.parseStatePackageDbStack,
      parsedEnvSelectedUnitIds = parseState.parseStateSelectedUnitIds
    }

trim :: String -> String
trim = reverse . dropWhile isTrimChar . reverse . dropWhile isTrimChar
  where
    isTrimChar ch = isSpace ch

splitPackagePathPreservingEmptyEntries :: String -> [String]
splitPackagePathPreservingEmptyEntries packagePath =
  reverse (go [] [] packagePath)
  where
    go currentEntry parsedEntries remainingChars =
      case remainingChars of
        [] -> reverse currentEntry : parsedEntries
        nextChar : restChars
          | nextChar == searchPathSeparator ->
              go [] (reverse currentEntry : parsedEntries) restChars
          | otherwise ->
              go (nextChar : currentEntry) parsedEntries restChars

parseKeywordArgument :: T.Text -> T.Text -> Maybe (Either String String)
parseKeywordArgument keyword line =
  case T.stripPrefix keyword line of
    Nothing -> Nothing
    Just rest
      | T.null rest ->
          Just (Left missingArgumentMessage)
      | not (T.all isSpace (T.take 1 rest)) ->
          Nothing
      | T.null (T.strip rest) ->
          Just (Left missingArgumentMessage)
      | otherwise ->
          Just (Right (T.unpack (T.strip rest)))
  where
    missingArgumentMessage =
      "Missing argument for directive '"
        <> T.unpack keyword
        <> "'"
