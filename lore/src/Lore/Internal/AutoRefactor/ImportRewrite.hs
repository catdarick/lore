module Lore.Internal.AutoRefactor.ImportRewrite
  ( ImportRewriteResult (..),
    rewriteImportsInFile,
  )
where

import Data.Text (Text)
import qualified GHC
import Lore.Internal.AutoRefactor.ImportCleanupEdit (planImportCleanupEdits)
import Lore.Internal.AutoRefactor.ImportDecl (parseImports)
import Lore.Internal.AutoRefactor.ImportOps (ImportOperation)
import Lore.Internal.SourceEdit (FileEdit)

data ImportRewriteResult = ImportRewriteResult
  { rewriteEdits :: [FileEdit],
    rewriteLogs :: [String]
  }

rewriteImportsInFile :: FilePath -> GHC.ParsedModule -> Text -> [ImportOperation] -> ImportRewriteResult
rewriteImportsInFile _ _ _ [] =
  ImportRewriteResult [] []
rewriteImportsInFile filePath parsedModule source operations =
  let parsedImports = parseImports parsedModule
      (plannedEdits, plannedLogs) =
        planImportCleanupEdits filePath source parsedImports operations
   in ImportRewriteResult
        { rewriteEdits = plannedEdits,
          rewriteLogs = plannedLogs
        }
