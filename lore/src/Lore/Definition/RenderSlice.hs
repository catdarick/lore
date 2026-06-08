module Lore.Definition.RenderSlice
  ( definitionSourceToRenderSlice,
  )
where

import Lore.Internal.Definition.Types (DefinitionSlice (..), DefinitionSource (..), definitionSourceModule)

definitionSourceToRenderSlice :: DefinitionSource -> DefinitionSlice
definitionSourceToRenderSlice source =
  DefinitionSlice
    { definitionModule = definitionSourceModule source,
      declarationSpans = [source.definitionSourceSpans]
    }
