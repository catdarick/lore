module Lore.Refactor.Imports
  ( ImportId (..),
    QualifiedImportStyle (..),
    ImportItem (..),
    ImportList (..),
    ParsedImport (..),
    NormalizedImport (..),
    NormalizedImportItem,
    unNormalizedImportItem,
    mkNormalizedImportItem,
    ImportRemovalTarget (..),
    mkFlatRemovalTarget,
    mkWholeImportItemTarget,
    mkScopedRemovalTarget,
    ImportOperation (..),
    RedundantImportRequest (..),
    parseImports,
    normalizedImportFromParsed,
    renderNormalizedImport,
    applyImportOperations,
    redundantImportRequestFromDiagnostic,
  )
where

import Lore.Internal.AutoRefactor.ImportDecl
  ( ImportId (..),
    ImportItem (..),
    ImportList (..),
    NormalizedImport (..),
    ParsedImport (..),
    QualifiedImportStyle (..),
    normalizedImportFromParsed,
    parseImports,
    renderNormalizedImport,
  )
import Lore.Internal.AutoRefactor.ImportNormalize
  ( applyImportOperations,
  )
import Lore.Internal.AutoRefactor.ImportOps
  ( ImportOperation (..),
    ImportRemovalTarget (..),
    NormalizedImportItem,
    mkFlatRemovalTarget,
    mkNormalizedImportItem,
    mkScopedRemovalTarget,
    mkWholeImportItemTarget,
    unNormalizedImportItem,
  )
import Lore.Internal.AutoRefactor.RedundantImports
  ( RedundantImportRequest (..),
    redundantImportRequestFromDiagnostic,
  )
