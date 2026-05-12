{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lore.Internal.AutoRefactor.Edit
  ( FileEdit (..),
    AppliedFileEdits (..),
    applyFileEdits,
    applyReplacementEdits,
    spanToOffsets,
    positionToOffset,
  )
where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.List (nubBy, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Diagnostics (Span (..))
import Lore.Internal.SourceSpan (spanStartKey)
import Lore.Internal.SourceText (positionToOffset, spanToOffsets)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data FileEdit
  = ReplaceSpanEdit FilePath Span Text
  deriving (Eq, Show)

data AppliedFileEdits = AppliedFileEdits
  { appliedChangedFiles :: [FilePath]
  }

applyFileEdits :: (MonadLore m) => [FileEdit] -> m AppliedFileEdits
applyFileEdits edits = do
  changedFiles <- fmap catMaybes $
    forM (Map.toList groupedEdits) \(filePath, fileEdits) -> do
      source <- liftIO $ TIO.readFile filePath
      let updatedSource = applyReplacementEdits source fileEdits
      if updatedSource == source
        then pure Nothing
        else do
          Log.info $ "Auto-refact: applying edits to " <> filePath
          liftIO $ TIO.writeFile filePath updatedSource
          pure (Just filePath)
  pure AppliedFileEdits {appliedChangedFiles = changedFiles}
  where
    groupedEdits =
      Map.fromListWith
        (<>)
        [ (editFilePath edit, [edit])
        | edit <- edits
        ]

applyReplacementEdits :: Text -> [FileEdit] -> Text
applyReplacementEdits source =
  foldl applyOne source
    . sortBy compareDescending
    . nubBy sameReplacement
  where
    compareDescending left right =
      compare (replacementStartKey right) (replacementStartKey left)

    sameReplacement (ReplaceSpanEdit _ leftSpan leftReplacement) (ReplaceSpanEdit _ rightSpan rightReplacement) =
      leftSpan == rightSpan && leftReplacement == rightReplacement

    applyOne contents (ReplaceSpanEdit _ editSpan editReplacement) =
      case spanToOffsets contents editSpan of
        Just (startOffset, endOffset) ->
          takeText startOffset contents <> editReplacement <> dropText endOffset contents
        Nothing ->
          contents

takeText :: Int -> Text -> Text
takeText =
  T.take

dropText :: Int -> Text -> Text
dropText =
  T.drop

editFilePath :: FileEdit -> FilePath
editFilePath = \case
  ReplaceSpanEdit filePath _ _ -> filePath

replacementStartKey :: FileEdit -> (Int, Int)
replacementStartKey = \case
  ReplaceSpanEdit _ span' _ -> spanStartKey span'
