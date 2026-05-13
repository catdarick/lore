{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lore.Internal.SourceEdit
  ( FileEdit (..),
    EditValidationWarning (..),
    AppliedFileEdits (..),
    applyFileEdits,
    applyReplacementEdits,
    applyReplacementEditsValidated,
    editFilePath,
    replacementStartKey,
    spanToOffsets,
    positionToOffset,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl', sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Internal.SourceSpan (spanStartKey)
import Lore.Internal.SourceSpan.Types (Span)
import Lore.Internal.SourceText (positionToOffset, spanToOffsets)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data FileEdit
  = ReplaceSpanEdit FilePath Span Text
  deriving (Eq, Show)

data EditValidationWarning
  = ConflictingFileEdits FilePath FileEdit FileEdit
  | InvalidFileEditSpan FilePath FileEdit
  deriving (Eq, Show)

data AppliedFileEdits = AppliedFileEdits
  { appliedChangedFiles :: [FilePath],
    appliedWarnings :: [EditValidationWarning]
  }
  deriving (Eq, Show)

data OffsetFileEdit = OffsetFileEdit
  { offsetFileEdit :: FileEdit,
    offsetStart :: Int,
    offsetEnd :: Int
  }
  deriving (Eq)

applyFileEdits :: (MonadLore m) => [FileEdit] -> m AppliedFileEdits
applyFileEdits edits = do
  fileResults <- forM (Map.toList groupedEdits) \(filePath, fileEdits) -> do
    source <- liftIO $ TIO.readFile filePath
    let (validatedEdits, validationWarnings) = validateReplacementEdits source filePath fileEdits
        updatedSource = applyValidatedReplacementEdits source validatedEdits
    if updatedSource == source
      then pure (Nothing, validationWarnings)
      else do
        Log.info $ "Auto-refactor: applying edits to " <> filePath
        liftIO $ TIO.writeFile filePath updatedSource
        pure (Just filePath, validationWarnings)

  let changedFiles = catMaybes (map fst fileResults)
      warnings = concatMap snd fileResults
  forM_ warnings (Log.warn . renderEditValidationWarning)
  pure
    AppliedFileEdits
      { appliedChangedFiles = changedFiles,
        appliedWarnings = warnings
      }
  where
    groupedEdits =
      foldl'
        (\acc edit -> Map.insertWith (flip (<>)) (editFilePath edit) [edit] acc)
        Map.empty
        edits

applyReplacementEdits :: Text -> [FileEdit] -> Text
applyReplacementEdits source edits =
  applyValidatedReplacementEdits source validatedEdits
  where
    (validatedEdits, _warnings) =
      validateReplacementEdits source "<unknown-file>" edits

applyReplacementEditsValidated :: Text -> FilePath -> [FileEdit] -> (Text, [EditValidationWarning])
applyReplacementEditsValidated source filePath edits =
  let (validatedEdits, warnings) = validateReplacementEdits source filePath edits
   in (applyValidatedReplacementEdits source validatedEdits, warnings)

applyValidatedReplacementEdits :: Text -> [OffsetFileEdit] -> Text
applyValidatedReplacementEdits source =
  foldl' applyOne source
    . sortBy compareDescendingOffsets
  where
    compareDescendingOffsets left right =
      compare (offsetStart right, offsetEnd right) (offsetStart left, offsetEnd left)

    applyOne contents OffsetFileEdit {offsetStart, offsetEnd, offsetFileEdit = ReplaceSpanEdit _ _ replacement} =
      T.take offsetStart contents <> replacement <> T.drop offsetEnd contents

validateReplacementEdits :: Text -> FilePath -> [FileEdit] -> ([OffsetFileEdit], [EditValidationWarning])
validateReplacementEdits source filePath edits =
  (validatedEdits, invalidWarnings <> reverse conflictWarnings)
  where
    dedupedEdits = dedupeExactEdits edits
    (invalidWarnings, offsetCandidates) =
      foldr collectOffset ([], []) dedupedEdits
    sortedCandidates =
      sortBy
        (\left right -> compare (offsetStart left, offsetEnd left) (offsetStart right, offsetEnd right))
        offsetCandidates

    collectOffset edit@(ReplaceSpanEdit editSourcePath editSpan _replacement) (warningsAcc, candidatesAcc) =
      case spanToOffsets source editSpan of
        Just (startOffset, endOffset)
          | startOffset <= endOffset ->
              (warningsAcc, OffsetFileEdit edit startOffset endOffset : candidatesAcc)
        _ ->
          (InvalidFileEditSpan editSourcePath edit : warningsAcc, candidatesAcc)
    (validatedEdits, _conflictedEdits, conflictWarnings) =
      foldl' classifyCandidate ([], [], []) sortedCandidates

    classifyCandidate (accepted, conflicted, warningsAcc) candidate
      | null conflictingAccepted && null conflictingConflicted =
          (candidate : accepted, conflicted, warningsAcc)
      | otherwise =
          let acceptedWithoutConflicts =
                foldl' removeFirst accepted conflictingAccepted
              newConflicted =
                dedupeOffsetEdits (candidate : conflictingAccepted <> conflictingConflicted <> conflicted)
              newWarnings =
                warningsAcc
                  <> map (conflictWarning candidate) conflictingAccepted
                  <> map (conflictWarning candidate) conflictingConflicted
           in (acceptedWithoutConflicts, newConflicted, newWarnings)
      where
        conflictingAccepted = filter (editsConflict candidate) accepted
        conflictingConflicted = filter (editsConflict candidate) conflicted

    conflictWarning left right =
      ConflictingFileEdits filePath left.offsetFileEdit right.offsetFileEdit

    removeFirst values value =
      case break (== value) values of
        (before, _ : after) -> before <> after
        (_, []) -> values

editsConflict :: OffsetFileEdit -> OffsetFileEdit -> Bool
editsConflict left right =
  sameSpan || overlaps
  where
    sameSpan =
      left.offsetStart == right.offsetStart
        && left.offsetEnd == right.offsetEnd
    overlaps =
      left.offsetStart < right.offsetEnd
        && right.offsetStart < left.offsetEnd

dedupeExactEdits :: [FileEdit] -> [FileEdit]
dedupeExactEdits =
  foldl'
    (\uniqueEdits edit -> if edit `elem` uniqueEdits then uniqueEdits else uniqueEdits <> [edit])
    []

dedupeOffsetEdits :: [OffsetFileEdit] -> [OffsetFileEdit]
dedupeOffsetEdits =
  foldl'
    (\uniqueEdits edit -> if edit `elem` uniqueEdits then uniqueEdits else uniqueEdits <> [edit])
    []

renderEditValidationWarning :: EditValidationWarning -> String
renderEditValidationWarning = \case
  ConflictingFileEdits warningFilePath leftEdit rightEdit ->
    "Auto-refactor: skipped conflicting edits in "
      <> warningFilePath
      <> ": "
      <> show leftEdit
      <> " conflicts with "
      <> show rightEdit
  InvalidFileEditSpan warningFilePath edit ->
    "Auto-refactor: skipped edit with invalid span in "
      <> warningFilePath
      <> ": "
      <> show edit

editFilePath :: FileEdit -> FilePath
editFilePath = \case
  ReplaceSpanEdit filePath _ _ -> filePath

replacementStartKey :: FileEdit -> (Int, Int)
replacementStartKey = \case
  ReplaceSpanEdit _ span' _ -> spanStartKey span'
