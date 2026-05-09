module Lore.Mcp.Internal.SourceText
  ( readSpanText,
    readSpanLines,
    sliceRealSpan,
    relativeSourcePath,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC.Plugins as GHC
import Lore.Mcp.Internal.SourceSpan (realSrcSpanFromSrcSpan)
import System.FilePath (isRelative, makeRelative, normalise)

readSpanText :: GHC.SrcSpan -> IO Text
readSpanText span' =
  T.intercalate "\n" <$> readSpanLines span'

readSpanLines :: GHC.SrcSpan -> IO [Text]
readSpanLines span' =
  case realSrcSpanFromSrcSpan span' of
    Nothing ->
      pure ["<definition source unavailable>"]
    Just realSpan ->
      sliceRealSpan realSpan . T.lines <$> TIO.readFile (GHC.unpackFS (GHC.srcSpanFile realSpan))

sliceRealSpan :: GHC.RealSrcSpan -> [Text] -> [Text]
sliceRealSpan realSpan fileLines =
  case drop (GHC.srcSpanStartLine realSpan - 1) fileLines of
    [] ->
      []
    relevantLines ->
      zipWith
        sliceLine
        [GHC.srcSpanStartLine realSpan .. GHC.srcSpanEndLine realSpan]
        (take (GHC.srcSpanEndLine realSpan - GHC.srcSpanStartLine realSpan + 1) relevantLines)
  where
    sliceLine lineNo line
      | lineNo == GHC.srcSpanStartLine realSpan && lineNo == GHC.srcSpanEndLine realSpan =
          T.take width (T.drop startCol line)
      | lineNo == GHC.srcSpanStartLine realSpan =
          T.drop startCol line
      | lineNo == GHC.srcSpanEndLine realSpan =
          T.take endCol line
      | otherwise =
          line
      where
        startCol = GHC.srcSpanStartCol realSpan - 1
        endCol = GHC.srcSpanEndCol realSpan - 1
        width = endCol - startCol

relativeSourcePath :: FilePath -> FilePath -> FilePath
relativeSourcePath currentDirectory sourcePath =
  normalise $
    if isRelative sourcePath
      then sourcePath
      else makeRelative currentDirectory sourcePath
