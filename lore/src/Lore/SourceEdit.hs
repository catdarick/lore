module Lore.SourceEdit
  ( FileEdit (..),
    EditValidationWarning (..),
    Span (..),
    applyReplacementEdits,
    applyReplacementEditsValidated,
  )
where

import Lore.Internal.SourceEdit
  ( EditValidationWarning (..),
    FileEdit (..),
    applyReplacementEdits,
    applyReplacementEditsValidated,
  )
import Lore.Internal.SourceSpan.Types (Span (..))
