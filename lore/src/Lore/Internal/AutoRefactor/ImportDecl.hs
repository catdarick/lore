{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lore.Internal.AutoRefactor.ImportDecl
  ( ImportId (..),
    QualifiedImportStyle (..),
    ImportItem (..),
    ImportList (..),
    ParsedImport (..),
    NormalizedImport (..),
    parseImports,
    normalizedImportFromParsed,
    renderNormalizedImport,
    parsedImportEffectiveQualifier,
    parsedImportQualified,
    parsedImportContainsSpan,
    srcSpanToSpan,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Hs (GhcPs, HsModule (..), IE, ImportDecl (..), LImportDecl)
import qualified GHC.Types.PkgQual as PkgQual
import qualified GHC.Utils.Outputable as Outputable
import Lore.Internal.List (mapMaybeToList, maybeToList)
import Lore.Internal.SourceSpan (spanContains, srcSpanToSpan)
import Lore.Internal.SourceSpan.Types (Span (..))
import Lore.Internal.SourceText (spanTextMaybe)

newtype ImportId = ImportId Int
  deriving (Eq, Ord, Show)

data QualifiedImportStyle
  = ImportUnqualified
  | ImportQualifiedPrefix
  | ImportQualifiedPostfix
  deriving (Eq, Ord, Show)

data ImportItem = ImportItem
  { importItemText :: Text,
    importItemSpan :: Maybe Span
  }
  deriving (Eq, Show)

data ImportList
  = OpenImport
  | ExplicitImport [ImportItem]
  | HidingImport [ImportItem]
  deriving (Eq, Show)

data ParsedImport = ParsedImport
  { parsedImportId :: ImportId,
    parsedImportOrder :: Int,
    parsedImportDecl :: ImportDecl GhcPs,
    parsedImportSpan :: Span,
    parsedImportListSpan :: Maybe Span,
    parsedImportModuleName :: Text,
    parsedImportQualifiedStyle :: QualifiedImportStyle,
    parsedImportAlias :: Maybe Text,
    parsedImportSource :: Bool,
    parsedImportSafe :: Bool,
    parsedImportPackageQualifier :: Maybe Text,
    parsedImportList :: ImportList
  }

data NormalizedImport = NormalizedImport
  { normalizedImportId :: Maybe ImportId,
    normalizedImportOrder :: Int,
    normalizedImportSpan :: Maybe Span,
    normalizedImportModuleName :: Text,
    normalizedImportQualifiedStyle :: QualifiedImportStyle,
    normalizedImportAlias :: Maybe Text,
    normalizedImportSource :: Bool,
    normalizedImportSafe :: Bool,
    normalizedImportPackageQualifier :: Maybe Text,
    normalizedImportList :: ImportList
  }
  deriving (Eq, Show)

parseImports :: Text -> GHC.ParsedModule -> [ParsedImport]
parseImports source parsedModule =
  mapMaybeToList (uncurry (parseImport source)) (zip [0 ..] hsmodImports)
  where
    GHC.L _ HsModule {hsmodImports} = GHC.pm_parsed_source parsedModule

parseImport :: Text -> Int -> LImportDecl GhcPs -> Maybe ParsedImport
parseImport source importIndex locatedImport = do
  parsedImportSpan <- srcSpanToSpan (GHC.locA (GHC.getLoc locatedImport))
  let parsedImportDecl = GHC.unLoc locatedImport
      parsedImportId = ImportId importIndex
      parsedImportOrder = importIndex
      parsedImportModuleName = T.pack (GHC.moduleNameString (GHC.unLoc parsedImportDecl.ideclName))
      parsedImportAlias = fmap (T.pack . GHC.moduleNameString . GHC.unLoc) parsedImportDecl.ideclAs
      parsedImportQualifiedStyle =
        case parsedImportDecl.ideclQualified of
          GHC.NotQualified -> ImportUnqualified
          GHC.QualifiedPre -> ImportQualifiedPrefix
          GHC.QualifiedPost -> ImportQualifiedPostfix
      parsedImportSource = parsedImportDecl.ideclSource == GHC.IsBoot
      parsedImportSafe = parsedImportDecl.ideclSafe
      parsedImportPackageQualifier =
        case parsedImportDecl.ideclPkgQual of
          PkgQual.NoRawPkgQual -> Nothing
          PkgQual.RawPkgQual rawPkgQual -> Just (rawPkgQualText rawPkgQual)
      parsedImportListSpan =
        parsedImportDecl.ideclImportList >>= \(importListKind, GHC.L locatedItems _) ->
          case importListKind of
            GHC.Exactly -> srcSpanToSpan (GHC.locA locatedItems)
            GHC.EverythingBut -> srcSpanToSpan (GHC.locA locatedItems)
      parsedImportList =
        case parsedImportDecl.ideclImportList of
          Nothing -> OpenImport
          Just (GHC.Exactly, GHC.L _ lies) ->
            ExplicitImport (map (renderLocatedImportItem source) lies)
          Just (GHC.EverythingBut, GHC.L _ lies) ->
            HidingImport (map (renderLocatedImportItem source) lies)
  pure ParsedImport {..}

renderLocatedImportItem :: Text -> GHC.LIE GhcPs -> ImportItem
renderLocatedImportItem source locatedItem =
  let maybeImportItemSpan = srcSpanToSpan (GHC.locA (GHC.getLoc locatedItem))
      importItemText =
        case maybeImportItemSpan >>= spanTextMaybe source of
          Just sourceText -> sourceText
          Nothing -> renderImportItem (GHC.unLoc locatedItem)
   in ImportItem
        { importItemText = importItemText,
          importItemSpan = maybeImportItemSpan
        }

normalizedImportFromParsed :: ParsedImport -> NormalizedImport
normalizedImportFromParsed ParsedImport {..} =
  NormalizedImport
    { normalizedImportId = Just parsedImportId,
      normalizedImportOrder = parsedImportOrder,
      normalizedImportSpan = Just parsedImportSpan,
      normalizedImportModuleName = parsedImportModuleName,
      normalizedImportQualifiedStyle = parsedImportQualifiedStyle,
      normalizedImportAlias = parsedImportAlias,
      normalizedImportSource = parsedImportSource,
      normalizedImportSafe = parsedImportSafe,
      normalizedImportPackageQualifier = parsedImportPackageQualifier,
      normalizedImportList = parsedImportList
    }

renderNormalizedImport :: NormalizedImport -> Text
renderNormalizedImport NormalizedImport {..} =
  T.unwords $
    ["import"]
      <> ["{-# SOURCE #-}" | normalizedImportSource]
      <> ["safe" | normalizedImportSafe]
      <> maybeToList (quoteText <$> normalizedImportPackageQualifier)
      <> ["qualified" | normalizedImportQualifiedStyle == ImportQualifiedPrefix]
      <> [modulePart]
      <> maybe [] (\alias -> ["as", alias]) normalizedImportAlias
      <> renderImportList normalizedImportList
  where
    modulePart =
      normalizedImportModuleName
        <> case normalizedImportQualifiedStyle of
          ImportQualifiedPostfix -> " qualified"
          _ -> ""

rawPkgQualText :: (Outputable.Outputable a) => a -> Text
rawPkgQualText rawPkgQual =
  let renderedPkgQual = T.strip (renderOutputable rawPkgQual)
   in case T.stripPrefix "\"" renderedPkgQual of
        Just withoutLeadingQuote ->
          case T.stripSuffix "\"" withoutLeadingQuote of
            Just unquotedPkgQual -> unquotedPkgQual
            Nothing -> renderedPkgQual
        Nothing ->
          renderedPkgQual

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""

renderImportList :: ImportList -> [Text]
renderImportList = \case
  OpenImport ->
    []
  ExplicitImport items ->
    ["(" <> T.intercalate ", " (map (.importItemText) items) <> ")"]
  HidingImport items ->
    ["hiding", "(" <> T.intercalate ", " (map (.importItemText) items) <> ")"]

renderImportItem :: IE GhcPs -> Text
renderImportItem =
  renderOutputable

renderOutputable :: (Outputable.Outputable a) => a -> Text
renderOutputable =
  T.pack . Outputable.showSDocUnsafe . Outputable.ppr

parsedImportQualified :: ParsedImport -> Bool
parsedImportQualified =
  (/= ImportUnqualified) . parsedImportQualifiedStyle

parsedImportEffectiveQualifier :: ParsedImport -> Maybe Text
parsedImportEffectiveQualifier parsedImport
  | parsedImportQualified parsedImport =
      Just (maybe parsedImport.parsedImportModuleName id parsedImport.parsedImportAlias)
  | otherwise =
      Nothing

parsedImportContainsSpan :: ParsedImport -> Span -> Bool
parsedImportContainsSpan parsedImport targetSpan =
  spanContains parsedImport.parsedImportSpan targetSpan
