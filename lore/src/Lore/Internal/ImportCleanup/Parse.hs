{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.ImportCleanup.Parse
  ( parseImports,
  )
where

import qualified Data.Text as T
import qualified GHC
import Lore.Internal.ImportCleanup.Types
  ( ImportCleanupWarning (..),
    ImportId (..),
    ParsedImport (..),
    ParsedImportListKind (..),
  )
import Lore.Internal.SourceSpan (srcSpanToSpan)
import Lore.Internal.SourceSpan.Types (Span (..))
import System.FilePath (isRelative, normalise, (</>))

parseImports :: FilePath -> FilePath -> GHC.ParsedModule -> ([ParsedImport], [ImportCleanupWarning])
parseImports projectRoot canonicalFilePath parsedModule =
  foldr collect ([], []) (zip [0 ..] hsmodImports)
  where
    GHC.L _ GHC.HsModule {GHC.hsmodImports} = GHC.pm_parsed_source parsedModule

    collect (importIndex, locatedImport) (importsAcc, warningsAcc) =
      case parseImport projectRoot canonicalFilePath importIndex locatedImport of
        Just (parsedImport, parseWarnings) ->
          (parsedImport : importsAcc, parseWarnings <> warningsAcc)
        Nothing ->
          (importsAcc, warningsAcc)

parseImport :: FilePath -> FilePath -> Int -> GHC.LImportDecl GHC.GhcPs -> Maybe (ParsedImport, [ImportCleanupWarning])
parseImport projectRoot canonicalFilePath importIndex locatedImport = do
  rawImportSpan <- srcSpanToSpan (GHC.locA (GHC.getLoc locatedImport))
  let parsedImportSpan = overwriteSpanFile canonicalFilePath rawImportSpan
      parsedImportDecl = GHC.unLoc locatedImport
      parsedImportId = ImportId importIndex
      parsedImportModuleName = T.pack (GHC.moduleNameString (GHC.unLoc parsedImportDecl.ideclName))
      parsedImportListKind =
        case parsedImportDecl.ideclImportList of
          Nothing -> ParsedOpenImport
          Just (GHC.Exactly, _) -> ParsedExplicitImport
          Just (GHC.EverythingBut, _) -> ParsedHidingImport
      parseWarnings =
        [ ImportSpanFileMismatch parsedImportId canonicalFilePath normalizedRawSpanFile
        | normalizedRawSpanFile /= canonicalFilePath
        ]
      normalizedRawSpanFile =
        normalizeRawSpanFile projectRoot rawImportSpan.spanFile
  pure
    ( ParsedImport
        { parsedImportId,
          parsedImportSpan,
          parsedImportModuleName,
          parsedImportListKind
        },
      parseWarnings
    )

overwriteSpanFile :: FilePath -> Span -> Span
overwriteSpanFile canonicalFilePath span' =
  span' {spanFile = canonicalFilePath}

normalizeRawSpanFile :: FilePath -> FilePath -> FilePath
normalizeRawSpanFile projectRoot rawSpanFile =
  let rootedRawPath =
        if isRelative rawSpanFile
          then projectRoot </> rawSpanFile
          else rawSpanFile
   in normalise rootedRawPath
