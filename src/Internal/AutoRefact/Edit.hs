{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Internal.AutoRefact.Edit
  ( FileEdit (..),
    AppliedFileEdits (..),
    applyFileEdits,
    restoreFileContents,
  )
where

import Control.Monad (forM, forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl', nubBy, sort, sortBy)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Internal.Diagnostics (Span (..))
import qualified Internal.Logger as Log
import Monad (MonadLore)

data FileEdit
  = AddImportEdit FilePath Text
  | ReplaceSpanEdit FilePath Span Text
  deriving (Eq, Show)

data AppliedFileEdits = AppliedFileEdits
  { appliedChangedFiles :: [FilePath],
    appliedOriginalContents :: Map.Map FilePath Text
  }

applyFileEdits :: (MonadLore m) => [FileEdit] -> m AppliedFileEdits
applyFileEdits edits = do
  results <- forM (Map.toList groupedEdits) \(filePath, fileEdits) -> do
    source <- liftIO $ TIO.readFile filePath
    let sourceAfterReplacements = applyReplacementEdits source fileEdits
        importBlock =
          sort $
            filter
              (not . importAlreadyPresent sourceAfterReplacements)
              (Set.toList (collectImportEdits fileEdits))
        updatedSource = insertImportStatements importBlock sourceAfterReplacements
    unless (updatedSource == source) do
      Log.info $ "Auto-refact: applying edits to " <> filePath
      liftIO $ TIO.writeFile filePath updatedSource
    pure
      if updatedSource == source
        then Nothing
        else Just source
  let changedEntries =
        [ (filePath, originalContents)
        | ((filePath, _), Just originalContents) <- zip (Map.toList groupedEdits) results
        ]
  pure
    AppliedFileEdits
      { appliedChangedFiles = map fst changedEntries,
        appliedOriginalContents = Map.fromList changedEntries
      }
  where
    groupedEdits =
      Map.fromListWith
        (<>)
        [ (editFilePath edit, [edit])
        | edit <- edits
        ]

restoreFileContents :: (MonadLore m) => Map.Map FilePath Text -> m ()
restoreFileContents originals =
  forM_ (Map.toList originals) \(filePath, originalContents) -> do
    Log.info $ "Auto-refact: restoring " <> filePath
    liftIO $ TIO.writeFile filePath originalContents

collectImportEdits :: [FileEdit] -> Set.Set Text
collectImportEdits =
  foldl'
    ( \acc -> \case
        AddImportEdit _ editImportText -> Set.insert editImportText acc
        ReplaceSpanEdit _ _ _ -> acc
    )
    Set.empty

applyReplacementEdits :: Text -> [FileEdit] -> Text
applyReplacementEdits source =
  foldl' applyOne source
    . sortBy compareDescending
    . nubBy sameReplacement
    . collectReplacementEdits
  where
    compareDescending left right =
      compare (replacementStartKey right) (replacementStartKey left)

    sameReplacement left right =
      case (left, right) of
        (ReplaceSpanEdit _ leftSpan leftReplacement, ReplaceSpanEdit _ rightSpan rightReplacement) ->
          leftSpan == rightSpan && leftReplacement == rightReplacement
        _ ->
          False

    applyOne contents (ReplaceSpanEdit _ editSpan editReplacement) =
      case spanToOffsets contents editSpan of
        Just (startOffset, endOffset) ->
          takeText startOffset contents <> editReplacement <> dropText endOffset contents
        Nothing ->
          contents
    applyOne contents (AddImportEdit _ _) =
      contents

collectReplacementEdits :: [FileEdit] -> [FileEdit]
collectReplacementEdits =
  foldr
    ( \edit acc ->
        case edit of
          ReplaceSpanEdit _ _ _ -> edit : acc
          AddImportEdit _ _ -> acc
    )
    []

importAlreadyPresent :: Text -> Text -> Bool
importAlreadyPresent source importText =
  let normalisedImport = unifySpaces (T.strip importText)
   in any ((== normalisedImport) . unifySpaces . T.strip) (T.lines source)

insertImportStatements :: [Text] -> Text -> Text
insertImportStatements newImports source =
  renderLines hadTrailingNewline $
    take insertionIndex sourceLines
      <> newImports
      <> drop insertionIndex sourceLines
  where
    sourceLines = T.lines source
    hadTrailingNewline = T.isSuffixOf "\n" source
    insertionIndex = findImportInsertionIndex sourceLines

findImportInsertionIndex :: [Text] -> Int
findImportInsertionIndex sourceLines =
  case importStartIndices of
    [] ->
      maybe (findPreambleInsertionIndex sourceLines) (+ 1) (findModuleHeaderEndIndex sourceLines)
    _ ->
      importDeclarationEnd sourceLines (last importStartIndices) + 1
  where
    importStartIndices =
      [ index
      | (index, line) <- zip [0 ..] sourceLines,
        isImportStart line
      ]

findModuleHeaderEndIndex :: [Text] -> Maybe Int
findModuleHeaderEndIndex sourceLines = do
  moduleStartIndex <-
    findIndexWith (\line -> "module " `T.isPrefixOf` T.stripStart line) sourceLines
  findIndexFrom moduleStartIndex (T.isSuffixOf "where" . T.strip) sourceLines

findPreambleInsertionIndex :: [Text] -> Int
findPreambleInsertionIndex =
  length . takeWhile isPreambleLine
  where
    isPreambleLine line =
      let stripped = T.stripStart line
       in T.null stripped
            || "{-#" `T.isPrefixOf` stripped
            || "--" `T.isPrefixOf` stripped
            || "{-" `T.isPrefixOf` stripped

isImportStart :: Text -> Bool
isImportStart line = "import " `T.isPrefixOf` T.stripStart line

importDeclarationEnd :: [Text] -> Int -> Int
importDeclarationEnd sourceLines =
  go
  where
    go currentIndex =
      case drop (currentIndex + 1) sourceLines of
        nextLine : _
          | isImportContinuation nextLine ->
              go (currentIndex + 1)
        _ ->
          currentIndex

isImportContinuation :: Text -> Bool
isImportContinuation line =
  not (T.null line)
    && T.length line > T.length (T.stripStart line)

renderLines :: Bool -> [Text] -> Text
renderLines hadTrailingNewline sourceLines =
  let rendered = T.intercalate "\n" sourceLines
   in if hadTrailingNewline then rendered <> "\n" else rendered

spanToOffsets :: Text -> Span -> Maybe (Int, Int)
spanToOffsets contents Span {spanStartLine, spanStartCol, spanEndLine, spanEndCol} = do
  startOffset <- positionToOffset contents (spanStartLine, spanStartCol)
  endOffset <- positionToOffset contents (spanEndLine, spanEndCol)
  pure (startOffset, endOffset)

positionToOffset :: Text -> (Int, Int) -> Maybe Int
positionToOffset contents (targetLine, targetCol)
  | targetLine < 1 || targetCol < 1 = Nothing
  | otherwise = go 1 1 0 (T.unpack contents)
  where
    go line col offset remaining
      | (line, col) == (targetLine, targetCol) = Just offset
      | otherwise =
          case remaining of
            [] -> Nothing
            '\n' : rest -> go (line + 1) 1 (offset + 1) rest
            _ : rest -> go line (col + 1) (offset + 1) rest

takeText :: Int -> Text -> Text
takeText =
  T.take

dropText :: Int -> Text -> Text
dropText =
  T.drop

unifySpaces :: Text -> Text
unifySpaces =
  T.unwords . T.words

findIndexWith :: (a -> Bool) -> [a] -> Maybe Int
findIndexWith predicate =
  go 0
  where
    go _ [] = Nothing
    go index (value : rest)
      | predicate value = Just index
      | otherwise = go (index + 1) rest

findIndexFrom :: Int -> (a -> Bool) -> [a] -> Maybe Int
findIndexFrom start predicate =
  fmap (+ start) . findIndexWith predicate . drop start

spanStartKey :: Span -> (Int, Int)
spanStartKey Span {spanStartLine, spanStartCol} = (spanStartLine, spanStartCol)

editFilePath :: FileEdit -> FilePath
editFilePath = \case
  AddImportEdit filePath _ -> filePath
  ReplaceSpanEdit filePath _ _ -> filePath

replacementStartKey :: FileEdit -> (Int, Int)
replacementStartKey = \case
  ReplaceSpanEdit _ span' _ -> spanStartKey span'
  AddImportEdit _ _ -> (-1, -1)
