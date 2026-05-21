module Lore.Mcp.Internal.LoreDoc.Markdown
  ( renderLoreDocMarkdown,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Natural (Natural)
import Lore.Mcp.Internal.LoreDoc
  ( LoreBlock (..),
    LoreDoc (..),
    NumberedList (..),
    SourceFile (..),
    SourceSection (..),
  )

renderLoreDocMarkdown :: LoreDoc -> Text
renderLoreDocMarkdown (LoreDoc blocks) =
  T.intercalate "\n\n" $
    filter (not . T.null) $
      map renderBlock blocks

renderBlock :: LoreBlock -> Text
renderBlock = \case
  Heading1 text ->
    "# " <> text
  Heading2 text ->
    "## " <> text
  Heading3 text ->
    "### " <> text
  Paragraph text ->
    text
  BulletList items ->
    renderList renderBulletMarker items
  NumberedListBlock numbered ->
    renderList (renderNumberMarker numbered.numberedListStart) numbered.numberedListItems
  SourceFileBlock source ->
    renderSourceFile source

renderSourceFile :: SourceFile -> Text
renderSourceFile sourceFile =
  T.intercalate "\n\n" (heading : sectionBlocks)
  where
    heading =
      "## " <> sourceFile.sourceFilePath
    sectionBlocks =
      map renderSourceSection sourceFile.sourceFileSections

renderSourceSection :: SourceSection -> Text
renderSourceSection sourceSection =
  T.intercalate
    "\n\n"
    [ "### " <> sourceSection.sourceSectionTitle,
      sourceSection.sourceSectionText
    ]

renderList :: (Int -> Text) -> [LoreDoc] -> Text
renderList renderMarker items =
  T.intercalate "\n" (concatMap renderOne (zip [1 ..] items))
  where
    markerWidth =
      maximum (0 : map (T.length . renderMarker . fst) (zip [1 ..] items))

    renderOne (index, item) =
      renderListItem (T.justifyLeft markerWidth ' ' (renderMarker index)) (renderLoreDocMarkdown item)

renderListItem :: Text -> Text -> [Text]
renderListItem markerText content =
  case T.splitOn "\n" content of
    [] ->
      [itemPrefix]
    firstLine : continuationLines ->
      (itemPrefix <> firstLine) : map (continuationPrefix <>) continuationLines
  where
    itemPrefix =
      markerText <> " "
    continuationPrefix =
      T.replicate (T.length itemPrefix) " "

renderBulletMarker :: Int -> Text
renderBulletMarker _ =
  "-"

renderNumberMarker :: Natural -> Int -> Text
renderNumberMarker start index =
  T.pack (show (start + fromIntegral index - 1)) <> "."
