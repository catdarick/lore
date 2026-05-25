module Lore.Internal.Lookup.TypeQuery
  ( ParsedTypeQuery (..),
    TypeQueryParseError (..),
    TypeQueryOccurrence (..),
    TypeQueryQualification (..),
    TypeQueryOccurrencePolicy (..),
    TypeQueryUnresolvedSymbolQuery (..),
    TypeQueryUnresolvedSymbols (..),
    TypeQueryNameResolutionError (..),
    ResolvedTypeQuery (..),
    parseTypeQuery,
    resolveParsedTypeQueryNames,
    withAdditionalInteractiveImports,
  )
where

import Lore.Internal.Lookup.TypeQuery.Names
  ( TypeQueryOccurrence (..),
    TypeQueryQualification (..),
  )
import Lore.Internal.Lookup.TypeQuery.Parse
  ( ParsedTypeQuery (..),
    TypeQueryParseError (..),
    parseTypeQuery,
  )
import Lore.Internal.Lookup.TypeQuery.Resolve
  ( ResolvedTypeQuery (..),
    TypeQueryNameResolutionError (..),
    TypeQueryOccurrencePolicy (..),
    TypeQueryUnresolvedSymbolQuery (..),
    TypeQueryUnresolvedSymbols (..),
    resolveParsedTypeQueryNames,
    withAdditionalInteractiveImports,
  )
