{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lore.Internal.AutoRefactor.Edit
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

import Lore.Internal.SourceEdit
