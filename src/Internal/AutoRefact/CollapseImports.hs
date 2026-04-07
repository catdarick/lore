module Internal.AutoRefact.CollapseImports
  ( collapseImportsInFiles,
  )
where

import Data.List (find, foldl', nubBy, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified GHC
import Internal.AutoRefact.Edit (AppliedFileEdits (..), FileEdit (ReplaceSpanEdit), applyFileEdits)
import Internal.AutoRefact.ImportDecl
  ( ImportShape (..),
    ParsedImport (..),
    parseImports,
    renderImportDecl,
  )
import Internal.Diagnostics (Span (..))
import Monad (MonadLore)
import System.FilePath (normalise)

collapseImportsInFiles :: (MonadLore m) => Map.Map FilePath GHC.ModSummary -> [FilePath] -> m AppliedFileEdits
collapseImportsInFiles modSummariesByFile filePaths = do
  edits <- concat <$> mapM collapseImportsInFile (nubBy (==) (map normalise filePaths))
  applyFileEdits edits
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

buildCollapseEdits :: FilePath -> GHC.ParsedModule -> [FileEdit]
buildCollapseEdits filePath parsedModule =
  concatMap (collapseGroup filePath) groupedImports
  where
    parsedImports = parseImports parsedModule
    groupedImports =
      Map.elems $
        Map.fromListWith
          (<>)
          [ (parsedImportKey parsedImport, [parsedImport])
          | parsedImport <- sortOn (spanStartKey . parsedImportSpan) parsedImports
          ]

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
              mergedText = renderImportDecl keptImport.parsedImportDecl (ExplicitImport mergedItems)
              replacementEdit =
                ReplaceSpanEdit filePath keptImport.parsedImportSpan mergedText
              removalEdits =
                [ ReplaceSpanEdit filePath (extendToWholeLine importToRemove.parsedImportSpan) ""
                | importToRemove <- tail imports
                ]
           in replacementEdit : removalEdits

extendToWholeLine :: Span -> Span
extendToWholeLine span' =
  span'
    { spanEndLine = span'.spanEndLine + 1,
      spanEndCol = 1
    }

spanStartKey :: Span -> (Int, Int)
spanStartKey Span {spanStartLine, spanStartCol} = (spanStartLine, spanStartCol)

deduplicateTexts :: [T.Text] -> [T.Text]
deduplicateTexts =
  foldl'
    (\acc value -> if value `elem` acc then acc else acc <> [value])
    []
