module Lore.Mcp.Internal.Render
  ( ListMarker (..),
    ListRenderContext (..),
    RenderList (..),
    Renderable (..),
    SomeRenderable (..),
    Truncation (..),
    Indented (..),
    applyTruncation,
    indented,
    nextSkip,
    remainingItems,
    renderListItem,
    renderMarker,
    someRenderable,
    takeVisibleItems,
    (|>),
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

class Renderable a where
  renderText :: a -> Text

data SomeRenderable where
  SomeRenderable :: (Renderable a) => a -> SomeRenderable
  Append :: (Renderable a, Renderable b) => a -> b -> SomeRenderable

(|>) :: (Renderable a, Renderable b) => a -> b -> SomeRenderable
(|>) = Append

instance Renderable SomeRenderable where
  renderText (SomeRenderable x) = renderText x
  renderText (Append x y) =
    let renderedX = renderText x
        renderedY = renderText y
     in if any T.null [renderedX, renderedY]
          then renderedX <> renderedY
          else renderedX <> "\n" <> renderedY

instance Renderable Text where
  renderText = id

instance (Renderable a) => Renderable (Maybe a) where
  renderText Nothing = ""
  renderText (Just x) = renderText x

newtype Indented = Indented SomeRenderable

instance Renderable Indented where
  renderText (Indented item) =
    T.unlines $
      map ("  " <>) (renderLines (renderText item))

instance Renderable [SomeRenderable] where
  renderText items =
    T.unlines (concatMap (renderLines . renderText) items)

data ListRenderContext = ListRenderContext
  { totalItems :: Int,
    skip :: Int,
    renderedItems :: Int
  }

remainingItems :: ListRenderContext -> Int
remainingItems ctx =
  totalItems ctx - ctx.skip - renderedItems ctx

nextSkip :: ListRenderContext -> Int
nextSkip ctx =
  ctx.skip + renderedItems ctx

data ListMarker
  = BulletMarker
  | NumberMarker

data Truncation = Truncation
  { maxItems :: Int,
    itemName :: Text,
    skipArgName :: Maybe Text
  }

data RenderList = forall a. (Renderable a) => RenderList
  { renderHeader :: ListRenderContext -> Maybe Text,
    contentIndentWidth :: Int,
    markerStyle :: ListMarker,
    itemsList :: NonEmpty a,
    skip :: Int,
    truncation :: Maybe Truncation
  }

instance Renderable RenderList where
  renderText RenderList {renderHeader, contentIndentWidth, markerStyle, itemsList, skip, truncation} =
    let allItems = NE.toList itemsList
        effectiveSkip = min skip (length allItems)
        visibleItems = takeVisibleItems effectiveSkip truncation allItems
        renderedCount = length visibleItems

        ctx =
          ListRenderContext
            { totalItems = length allItems,
              skip = effectiveSkip,
              renderedItems = renderedCount
            }

        maxMarkerWidth =
          if renderedCount == 0
            then 0
            else T.length (renderMarker markerStyle (effectiveSkip + renderedCount))

        itemLines =
          concat $
            zipWith
              (\ix item -> renderListItem markerStyle maxMarkerWidth ix (renderText item))
              [effectiveSkip + 1 ..]
              visibleItems
        skippingLine =
          case truncation of
            Just trunc
              | skip > 0 -> [renderSkipping trunc ctx]
            _ -> []
        overflowLine =
          case truncation of
            Just trunc
              | remainingItems ctx > 0 -> [renderOverflow trunc ctx]
            _ -> []

        contentLines = skippingLine <> itemLines <> overflowLine
        contentIndent = T.replicate contentIndentWidth " "
     in case renderHeader ctx of
          Nothing ->
            T.unlines contentLines
          Just header ->
            T.unlines (header : map (contentIndent <>) contentLines)
    where
      renderSkipping Truncation {..} ctx =
        "... skipping " <> T.pack (show ctx.skip) <> " " <> itemName
      renderOverflow Truncation {..} ctx =
        "... and " <> T.pack (show (remainingItems ctx)) <> " more " <> itemName <> skipInfo
        where
          skipInfo = case skipArgName of
            Nothing -> ""
            Just argName ->
              " (set " <> argName <> " to " <> T.pack (show (nextSkip ctx)) <> " to get the next page if required)"

someRenderable :: (Renderable a) => a -> SomeRenderable
someRenderable = SomeRenderable

indented :: (Renderable a) => a -> SomeRenderable
indented =
  someRenderable . Indented . someRenderable

takeVisibleItems :: Int -> Maybe Truncation -> [a] -> [a]
takeVisibleItems skip truncation =
  applyTruncation truncation . drop skip

applyTruncation :: Maybe Truncation -> [a] -> [a]
applyTruncation Nothing = id
applyTruncation (Just Truncation {maxItems}) = take maxItems

renderMarker :: ListMarker -> Int -> Text
renderMarker BulletMarker _ = "-"
renderMarker NumberMarker ix = T.pack (show ix) <> "."

renderListItem :: ListMarker -> Int -> Int -> Text -> [Text]
renderListItem markerStyle maxMarkerWidth ix txt =
  case T.lines txt of
    [] -> [itemPrefix]
    firstLine : rest ->
      (itemPrefix <> firstLine) : map (itemTextIndent <>) rest
  where
    markerText = renderMarker markerStyle ix
    paddedMarker = T.justifyLeft maxMarkerWidth ' ' markerText
    itemPrefix = paddedMarker <> " "
    itemTextIndent = T.replicate (maxMarkerWidth + 1) " "

renderLines :: Text -> [Text]
renderLines =
  T.lines . stripTrailingNewlines

stripTrailingNewlines :: Text -> Text
stripTrailingNewlines =
  T.dropWhileEnd (== '\n')
