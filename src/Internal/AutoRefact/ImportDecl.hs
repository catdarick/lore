{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Internal.AutoRefact.ImportDecl
  ( ImportKey (..),
    ImportShape (..),
    ParsedImport (..),
    parseImports,
    renderImportDecl,
    renderImportItem,
    parsedImportEffectiveQualifier,
    parsedImportModuleName,
    parsedImportQualified,
    srcSpanToSpan,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Data.FastString as FastString
import GHC.Hs (GhcPs, HsModule (..), IE, ImportDecl (..), LImportDecl)
import qualified GHC.Utils.Outputable as Outputable
import Internal.Diagnostics (Span (..))

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

parseImports :: GHC.ParsedModule -> [ParsedImport]
parseImports parsedModule =
  mapMaybeToList parseImport hsmodImports
  where
    GHC.L _ HsModule {hsmodImports} = GHC.pm_parsed_source parsedModule

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

renderImportDecl :: ImportDecl GhcPs -> ImportShape -> Text
renderImportDecl importDecl importShape =
  case importShape of
    HidingImport ->
      T.pack (Outputable.showSDocUnsafe (Outputable.ppr importDecl))
    OpenImport ->
      renderOpenOrExplicit importDecl []
    ExplicitImport items ->
      renderOpenOrExplicit importDecl items

renderOpenOrExplicit :: ImportDecl GhcPs -> [Text] -> Text
renderOpenOrExplicit importDecl items =
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

parsedImportQualified :: ParsedImport -> Bool
parsedImportQualified =
  (/= GHC.NotQualified) . ideclQualified . parsedImportDecl

parsedImportModuleName :: ParsedImport -> Text
parsedImportModuleName =
  T.pack . GHC.moduleNameString . GHC.unLoc . ideclName . parsedImportDecl

parsedImportEffectiveQualifier :: ParsedImport -> Maybe Text
parsedImportEffectiveQualifier parsedImport
  | parsedImportQualified parsedImport =
      Just $
        maybe
          (parsedImportModuleName parsedImport)
          id
          parsedImport.parsedImportKey.importAlias
  | otherwise =
      Nothing

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

mapMaybeToList :: (a -> Maybe b) -> [a] -> [b]
mapMaybeToList f =
  foldr
    (\value acc -> maybe acc (: acc) (f value))
    []
