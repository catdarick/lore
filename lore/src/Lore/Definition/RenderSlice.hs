module Lore.Definition.RenderSlice
  ( definitionSourceToRenderSlice,
  )
where

import Lore.Internal.Definition.Types (DefinitionSlice (..), DefinitionSource (..), RequiredImport)

definitionSourceToRenderSlice :: DefinitionSource -> [RequiredImport] -> DefinitionSlice
definitionSourceToRenderSlice source imports =
  DefinitionSlice
    { definitionModule = source.definitionSourceModule,
      declarationSpans = [source.definitionSourceSpans],
      requiredImports = imports
    }
