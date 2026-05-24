module Lore.Definition.RenderSlice
  ( definitionSourceToRenderSlice,
  )
where

import Lore.Internal.Definition.Types (DefinitionSlice (..), DefinitionSource (..))

definitionSourceToRenderSlice :: DefinitionSource -> DefinitionSlice
definitionSourceToRenderSlice source =
  DefinitionSlice
    { definitionModule = source.definitionSourceModule,
      declarationSpans = [source.definitionSourceSpans]
    }
