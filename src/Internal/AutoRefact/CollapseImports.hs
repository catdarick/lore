{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Internal.AutoRefact.CollapseImports
  ( collapseImportsInFiles,
  )
where

import Data.List (find, foldl', nubBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified GHC
import qualified GHC.Data.FastString as FastString
import GHC.Hs (GhcPs, HsModule (..), IE, ImportDecl (..), LImportDecl)
import qualified GHC.Utils.Outputable as Outputable
import Internal.AutoRefact.Edit (FileEdit (..), applyFileEdits)
import Internal.Diagnostics (Span (..))
import Monad (MonadLore)
import System.FilePath (normalise)
import qualified Data.Text as T
import Data.Text (Text)


collapseImportsInFiles :: (MonadLore m) => Map.Map FilePath GHC.ModSummary -> [FilePath] -> m Bool
collapseImportsInFiles modSummariesByFile filePaths = do
  edits <- concat <$> mapM collapseImportsInFile (nubBy (==) (map normalise filePaths))
  changedFiles <- applyFileEdits edits
  pure (not (null changedFiles))
  where
    collapseImportsInFile filePath =
      case Map.lookup filePath modSummariesByFile of
        Nothing ->
          pure []
        Just summary ->
          GHC.handleSourceError
            (const (pure []))
            do
              parsedModule <- GHC.parseModule summary
              pure (buildCollapseEdits filePath parsedModule)

data ImportKey = ImportKey
  { importModuleName :: Text,
    importQualified :: Bool,
    importAlias :: Maybe Text,
    importSource :: Bool,
    importPackageQualifier :: Maybe Text
  }
  deriving (Eq, Ord, Show)

data ImportShape
  = OpenImport
  | ExplicitImport [Text]
  | HidingImport
  deriving (Eq, Show)

data ParsedImport = ParsedImport
  { parsedImportDecl :: ImportDecl GhcPs,
    parsedImportSpan :: Span,
    parsedImportKey :: ImportKey,
    parsedImportShape :: ImportShape
  }

buildCollapseEdits :: FilePath -> GHC.ParsedModule -> [FileEdit]
buildCollapseEdits filePath parsedModule =
  concatMap (collapseGroup filePath) groupedImports
  where
    GHC.L _ HsModule {hsmodImports} = GHC.pm_parsed_source parsedModule
    parsedImports = mapMaybeToList parseImport hsmodImports
    groupedImports =
      Map.elems $
        Map.fromListWith
          (<>)
          [ (parsedImportKey parsedImport, [parsedImport])
          | parsedImport <- sortOn (spanStartKey . parsedImportSpan) parsedImports
          ]

parseImport :: LImportDecl GhcPs -> Maybe ParsedImport
parseImport locatedImport = do
  parsedImportSpan <- srcSpanToSpan (GHC.locA (GHC.getLoc locatedImport))
  let parsedImportDecl = GHC.unLoc locatedImport
      moduleNameText = T.pack (GHC.moduleNameString (GHC.unLoc parsedImportDecl.ideclName))
      aliasText = fmap (T.pack . GHC.moduleNameString . GHC.unLoc) parsedImportDecl.ideclAs
      parsedImportKey =
        ImportKey
          { importModuleName = moduleNameText,
            importQualified = parsedImportDecl.ideclQualified /= GHC.NotQualified,
            importAlias = aliasText,
            importSource = parsedImportDecl.ideclSource == GHC.IsBoot,
            importPackageQualifier = Nothing
          }
      parsedImportShape =
        case parsedImportDecl.ideclImportList of
          Nothing -> OpenImport
          Just (GHC.Exactly, GHC.L _ lies) ->
            ExplicitImport (map (renderImportItem . GHC.unLoc) lies)
          Just (GHC.EverythingBut, _) ->
            HidingImport
  pure ParsedImport {..}

collapseGroup :: FilePath -> [ParsedImport] -> [FileEdit]
collapseGroup _ [] = []
collapseGroup _ [_] = []
collapseGroup filePath imports
  | any ((== HidingImport) . parsedImportShape) imports =
      []
  | otherwise =
      case find ((== OpenImport) . parsedImportShape) imports of
        Just keptImport ->
          [ ReplaceSpanEdit filePath (extendToWholeLine importToRemove.parsedImportSpan) ""
          | importToRemove <- imports,
            parsedImportSpan importToRemove /= parsedImportSpan keptImport
          ]
        Nothing ->
          let keptImport = head imports
              mergedItems =
                deduplicateTexts $
                  concat
                    [ items
                    | ExplicitImport items <- map parsedImportShape imports
                    ]
              mergedText = renderImportDecl keptImport.parsedImportDecl mergedItems
              replacementEdit =
                ReplaceSpanEdit filePath keptImport.parsedImportSpan mergedText
              removalEdits =
                [ ReplaceSpanEdit filePath (extendToWholeLine importToRemove.parsedImportSpan) ""
                | importToRemove <- tail imports
                ]
           in replacementEdit : removalEdits

renderImportDecl :: ImportDecl GhcPs -> [Text] -> Text
renderImportDecl importDecl items =
  T.unwords $
    ["import"]
      <> ["{-# SOURCE #-}" | importDecl.ideclSource == GHC.IsBoot]
      <> ["qualified" | importDecl.ideclQualified == GHC.QualifiedPre]
      <> [modulePart]
      <> maybe [] (\alias -> ["as", alias]) aliasPart
      <> case items of
        [] -> []
        xs -> ["(" <> T.intercalate ", " xs <> ")"]
  where
    aliasPart =
      fmap (T.pack . GHC.moduleNameString . GHC.unLoc) importDecl.ideclAs
    modulePart =
      T.pack (GHC.moduleNameString (GHC.unLoc importDecl.ideclName))
        <> case importDecl.ideclQualified of
          GHC.QualifiedPost -> " qualified"
          _ -> ""

renderImportItem :: IE GhcPs -> Text
renderImportItem =
  T.pack . Outputable.showSDocUnsafe . Outputable.ppr

extendToWholeLine :: Span -> Span
extendToWholeLine span' =
  span'
    { spanEndLine = span'.spanEndLine + 1,
      spanEndCol = 1
    }

srcSpanToSpan :: GHC.SrcSpan -> Maybe Span
srcSpanToSpan = \case
  GHC.RealSrcSpan span' _ ->
    Just
      Span
        { spanFile = FastString.unpackFS (GHC.srcSpanFile span'),
          spanStartLine = GHC.srcSpanStartLine span',
          spanStartCol = GHC.srcSpanStartCol span',
          spanEndLine = GHC.srcSpanEndLine span',
          spanEndCol = GHC.srcSpanEndCol span'
        }
  GHC.UnhelpfulSpan {} ->
    Nothing

spanStartKey :: Span -> (Int, Int)
spanStartKey Span {spanStartLine, spanStartCol} = (spanStartLine, spanStartCol)

deduplicateTexts :: [Text] -> [Text]
deduplicateTexts =
  foldl'
    (\acc value -> if value `elem` acc then acc else acc <> [value])
    []

mapMaybeToList :: (a -> Maybe b) -> [a] -> [b]
mapMaybeToList f =
  catMaybes . map f
