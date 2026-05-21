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
    TypeQueryResolutionError (..),
    parseTypeQuery,
    resolveParsedTypeQueryNames,
    resolveTypeQueryNames,
    withAdditionalInteractiveImports,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Text (Text)
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
import Lore.Monad (MonadLore)

data TypeQueryResolutionError
  = TypeQueryResolutionParseError !TypeQueryParseError
  | TypeQueryResolutionNameError !TypeQueryNameResolutionError

resolveTypeQueryNames ::
  (MonadLore m) =>
  Text ->
  m (Either TypeQueryResolutionError ResolvedTypeQuery)
resolveTypeQueryNames queryText =
  runExceptT do
    parsed <- ExceptT (firstError TypeQueryResolutionParseError <$> parseTypeQuery queryText)
    ExceptT (firstError TypeQueryResolutionNameError <$> resolveParsedTypeQueryNames parsed)
  where
    firstError constructor eiResult =
      case eiResult of
        Left err ->
          Left (constructor err)
        Right value ->
          Right value
