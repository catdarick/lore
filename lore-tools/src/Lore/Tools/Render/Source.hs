module Lore.Tools.Render.Source
  ( definitionSliceToSourceFile,
    definitionSlicesToSourceFiles,
    declarationSpansToSourceSection,
    declarationSpansText,
    declarationBodyText,
    declarationSpansTitle,
    declarationSpansLineRange,
    definitionSourceRealSrcSpan,
    definitionSourcePathFromCurrentDirectory,
    definitionSourcePath,
  )
where

import Control.Applicative ((<|>))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Plugins as Plugins
import Lore (DeclarationSpans (..), DefinitionSlice (..), DefinitionSource (..), mergeDefinitionSlices)
import Lore.List (maximumMaybe, minimumMaybe)
import Lore.Tools.Render.Doc (SourceFile (..), SourceSection (..))
import Lore.SourceSpan (realSrcSpanFromSrcSpan)
import Lore.SourceText (readSpanText, relativeSourcePath)
import System.Directory (getCurrentDirectory)

definitionSliceToSourceFile :: DefinitionSlice -> IO SourceFile
definitionSliceToSourceFile definitionSlice = do
  renderedPath <- definitionSlicePath definitionSlice
  sections <- mapM declarationSpansToSourceSection (sortDeclarationSpans definitionSlice.declarationSpans)
  pure
    SourceFile
      { sourceFilePath = renderedPath,
        sourceFileSections = sections
      }

definitionSlicesToSourceFiles :: [DefinitionSlice] -> IO [SourceFile]
definitionSlicesToSourceFiles definitionSlices =
  mapM definitionSliceToSourceFile (mergeDefinitionModules definitionSlices)

definitionSlicePath :: DefinitionSlice -> IO Text
definitionSlicePath definitionSlice =
  realSrcSpanPath (definitionSliceRealSrcSpan definitionSlice)

definitionSliceRealSrcSpan :: DefinitionSlice -> Maybe GHC.RealSrcSpan
definitionSliceRealSrcSpan definitionSlice =
  case mapMaybe declarationSpansRealSrcSpan definitionSlice.declarationSpans of
    realSrcSpan : _ -> Just realSrcSpan
    [] -> Nothing

definitionSourceRealSrcSpan :: DefinitionSource -> Maybe GHC.RealSrcSpan
definitionSourceRealSrcSpan definitionSource =
  declarationSpansRealSrcSpan definitionSource.definitionSourceSpans

definitionSourcePath :: DefinitionSource -> IO Text
definitionSourcePath definitionSource =
  do
    currentDirectory <- getCurrentDirectory
    pure (definitionSourcePathFromCurrentDirectory currentDirectory definitionSource)

definitionSourcePathFromCurrentDirectory :: FilePath -> DefinitionSource -> Text
definitionSourcePathFromCurrentDirectory currentDirectory definitionSource =
  realSrcSpanPathFromCurrentDirectory currentDirectory (definitionSourceRealSrcSpan definitionSource)

realSrcSpanPath :: Maybe GHC.RealSrcSpan -> IO Text
realSrcSpanPath maybeSpan =
  do
    currentDirectory <- getCurrentDirectory
    pure (realSrcSpanPathFromCurrentDirectory currentDirectory maybeSpan)

realSrcSpanPathFromCurrentDirectory :: FilePath -> Maybe GHC.RealSrcSpan -> Text
realSrcSpanPathFromCurrentDirectory currentDirectory maybeSpan =
  case maybeSpan of
    Nothing ->
      "<definition source unavailable>"
    Just realSrcSpan ->
      T.pack $
        relativeSourcePath currentDirectory (Plugins.unpackFS (GHC.srcSpanFile realSrcSpan))

declarationSpansRealSrcSpan :: DeclarationSpans -> Maybe GHC.RealSrcSpan
declarationSpansRealSrcSpan spans =
  realSrcSpanFromSrcSpan spans.declarationSpan
    <|> (spans.signatureSpan >>= realSrcSpanFromSrcSpan)

declarationSpansToSourceSection :: DeclarationSpans -> IO SourceSection
declarationSpansToSourceSection declarationSpans = do
  sectionText <- declarationSpansText declarationSpans
  pure
    SourceSection
      { sourceSectionTitle = declarationSpansTitle declarationSpans,
        sourceSectionText = sectionText
      }

declarationSpansText :: DeclarationSpans -> IO Text
declarationSpansText spans = do
  declarationText <- readSpanText spans.declarationSpan
  signatureText <- traverse readSpanText spans.signatureSpan
  pure $
    maybe declarationText (<> "\n" <> declarationText) signatureText

declarationBodyText :: DeclarationSpans -> IO Text
declarationBodyText spans =
  readSpanText spans.declarationSpan

declarationSpansTitle :: DeclarationSpans -> Text
declarationSpansTitle declarationSpans =
  case declarationSpansLineRange declarationSpans of
    Nothing ->
      "definition"
    Just (startLine, endLine) ->
      "lines " <> T.pack (show startLine) <> "-" <> T.pack (show endLine)

declarationSpansLineRange :: DeclarationSpans -> Maybe (Int, Int)
declarationSpansLineRange declarationSpans = do
  firstSpan <- minimumMaybe realSrcSpans
  lastSpan <- maximumMaybe realSrcSpans
  pure (GHC.srcSpanStartLine firstSpan, GHC.srcSpanEndLine lastSpan)
  where
    realSrcSpans =
      mapMaybe realSrcSpanFromSrcSpan $
        maybeToList declarationSpans.signatureSpan <> [declarationSpans.declarationSpan]

sortDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
sortDeclarationSpans =
  sortOn (realSrcSpanFromSrcSpan . declarationSpan)

mergeDefinitionModules :: [DefinitionSlice] -> [DefinitionSlice]
mergeDefinitionModules =
  Map.elems . foldl insertSlice Map.empty
  where
    insertSlice acc slice =
      Map.insertWith mergeTwo slice.definitionModule slice acc

    mergeTwo new old =
      case mergeDefinitionSlices [old, new] of
        Just merged ->
          merged
        Nothing ->
          old
