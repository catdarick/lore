module Lore.Internal.Lookup.TypeQuery.Resolve
  ( TypeQueryOccurrencePolicy (..),
    TypeQueryUnresolvedSymbolQuery (..),
    TypeQueryNameResolutionError (..),
    TypeQueryUnresolvedSymbols (..),
    ResolvedTypeQuery (..),
    resolveParsedTypeQueryNames,
    withAdditionalInteractiveImports,
  )
where

import Control.Monad.Reader (asks)
import Data.Char (isUpper)
import Data.List (sortOn)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Plugins as Plugins
import Lore.Internal.Lookup.ModulePreference (ModulePreferenceContext (..), PreferredModuleChoice (..), choosePreferredModuleForRoot)
import Lore.Internal.Lookup.Name (parseAndNormalizeName)
import Lore.Internal.Lookup.SymbolResolutionCore
  ( ResolvedRootGroup (..),
    collectHomeModuleNames,
    dedupeTexts,
    groupSymbolsByResolvedRoot,
    resolveRootNameFromName,
  )
import Lore.Internal.Lookup.SymbolsMap (findMatchingSymbolsInMap)
import qualified Lore.Internal.Lookup.SymbolsMap as SymbolsMap
import Lore.Internal.Lookup.TypeQuery.Names
  ( TypeQueryOccurrence (..),
    TypeQueryQualification (..),
  )
import Lore.Internal.Lookup.TypeQuery.Parse (ParsedTypeQuery (..))
import Lore.Internal.Lookup.Types (Symbol (..))
import Lore.Monad (MonadLore)
import Lore.Session (SessionContext (customPrelude))
import UnliftIO (finally)

-- Resolves unqualified type/class occurrences in a parsed type query using
-- Lore's symbol index and produces temporary GHC interactive imports.
-- Qualified occurrences are intentionally delegated to GHC unchanged.

data TypeQueryOccurrencePolicy
  = ResolveThroughLoreSymbolIndex
  | UserQualifiedName !GHC.ModuleName
  | IgnoreBoundOrVariable
  | UnsupportedOccurrence !Text

data TypeQueryUnresolvedSymbolQuery
  = TypeQueryUnresolvedSymbolQueryMissing !Text
  | TypeQueryUnresolvedSymbolQueryAmbiguous !Text ![Text]

newtype TypeQueryUnresolvedSymbols = MkTypeQueryUnresolvedSymbols
  { unresolvedTypeQuerySymbols :: [TypeQueryUnresolvedSymbolQuery]
  }

data TypeQueryNameResolutionError
  = TypeQueryUnresolvedSymbols !TypeQueryUnresolvedSymbols
  | TypeQueryUnsupportedOccurrence !Text

data ResolvedTypeQuery = ResolvedTypeQuery
  { resolvedTypeQueryParsed :: !ParsedTypeQuery,
    resolvedTypeQueryImports :: ![GHC.InteractiveImport]
  }

resolveParsedTypeQueryNames ::
  (MonadLore m) =>
  ParsedTypeQuery ->
  m (Either TypeQueryNameResolutionError ResolvedTypeQuery)
resolveParsedTypeQueryNames parsed = do
  homeModuleNames <- collectHomeModuleNames
  maybeCustomPrelude <- asks customPrelude
  let context =
        ModulePreferenceContext
          { modulePreferenceHomeModules = homeModuleNames,
            modulePreferenceCustomPrelude = GHC.mkModuleName . T.unpack <$> maybeCustomPrelude
          }
  eiResolution <- resolveOccurrences context parsed.parsedTypeQueryOccurrences
  pure $
    case eiResolution of
      Left err ->
        Left err
      Right resolvedModuleImports ->
        Right
          ResolvedTypeQuery
            { resolvedTypeQueryParsed = parsed,
              resolvedTypeQueryImports = dedupeInteractiveImports (buildImports homeModuleNames parsed resolvedModuleImports)
            }

resolveOccurrences ::
  (MonadLore m) =>
  ModulePreferenceContext ->
  [TypeQueryOccurrence] ->
  m (Either TypeQueryNameResolutionError [GHC.ModuleName])
resolveOccurrences context occurrences = do
  results <- mapM (resolveOneOccurrence context) occurrences
  let unsupported = [message | Left (TypeQueryUnsupportedOccurrence message) <- results]
      unresolved = [query | Left (TypeQueryUnresolvedSymbols (MkTypeQueryUnresolvedSymbols queries)) <- results, query <- queries]
      resolvedImports = [moduleName | Right (Just moduleName) <- results]
  pure $
    case unsupported of
      message : _ ->
        Left (TypeQueryUnsupportedOccurrence message)
      [] ->
        if null unresolved
          then Right resolvedImports
          else Left (TypeQueryUnresolvedSymbols (MkTypeQueryUnresolvedSymbols unresolved))

resolveOneOccurrence ::
  (MonadLore m) =>
  ModulePreferenceContext ->
  TypeQueryOccurrence ->
  m (Either TypeQueryNameResolutionError (Maybe GHC.ModuleName))
resolveOneOccurrence context occurrence =
  case classifyOccurrencePolicy occurrence of
    IgnoreBoundOrVariable ->
      pure (Right Nothing)
    UserQualifiedName {} ->
      pure (Right Nothing)
    UnsupportedOccurrence message ->
      pure (Left (TypeQueryUnsupportedOccurrence message))
    ResolveThroughLoreSymbolIndex -> do
      eiResolved <- resolveUniqueOccurrencePreferredModule context occurrence
      pure $
        case eiResolved of
          Left unresolved ->
            Left (TypeQueryUnresolvedSymbols (MkTypeQueryUnresolvedSymbols [unresolved]))
          Right maybeModuleName ->
            Right maybeModuleName

-- Intentional policy:
--

-- * Unqualified occurrences are resolved through Lore's symbol index. This lets

--   us auto-import unambiguous symbols and emit Lore-style ambiguity diagnostics.
--

-- * User-qualified occurrences are left intact and delegated to GHC. This

--   preserves native diagnostics for missing modules and qualified names.
resolveUniqueOccurrencePreferredModule ::
  (MonadLore m) =>
  ModulePreferenceContext ->
  TypeQueryOccurrence ->
  m (Either TypeQueryUnresolvedSymbolQuery (Maybe GHC.ModuleName))
resolveUniqueOccurrencePreferredModule context occurrence = do
  let query = occurrence.typeQueryOccurrenceText
      normalized = parseAndNormalizeName query
  symbolsMap <- SymbolsMap.getCachedSymbolsMap
  let matchingSymbols = Set.toList (findMatchingSymbolsInMap normalized symbolsMap)
  case matchingSymbols of
    [] ->
      pure (Left (TypeQueryUnresolvedSymbolQueryMissing query))
    [singleSymbol] -> do
      rootName <- resolveRootNameFromName singleSymbol.name
      pure (Right (preferredImportForRoot context (ResolvedRootGroup rootName (singleSymbol NE.:| []))))
    _ -> do
      symbolsWithRoots <- mapM resolveSymbolRoot matchingSymbols
      let groupedByRoot = groupSymbolsByResolvedRoot symbolsWithRoots
      case groupedByRoot of
        [singleRootGroup] ->
          pure (Right (preferredImportForRoot context singleRootGroup))
        _ -> do
          let hints =
                dedupeTexts
                  (map (renderDisambiguationHint context occurrence) groupedByRoot)
          pure (Left (TypeQueryUnresolvedSymbolQueryAmbiguous query hints))

classifyOccurrencePolicy :: TypeQueryOccurrence -> TypeQueryOccurrencePolicy
classifyOccurrencePolicy occurrence =
  case occurrence.typeQueryOccurrenceQualification of
    TypeQueryQualified moduleName ->
      UserQualifiedName moduleName
    TypeQueryUnqualified ->
      if shouldResolveUnqualified occurrence.typeQueryOccurrenceRdrName
        then ResolveThroughLoreSymbolIndex
        else IgnoreBoundOrVariable

shouldResolveUnqualified :: GHC.RdrName -> Bool
shouldResolveUnqualified rdrName
  | Plugins.isQual rdrName = False
  | Plugins.isRdrTc rdrName && startsUppercaseOccName occName = True
  | Plugins.isSymOcc occName = True
  | otherwise = False
  where
    occName =
      Plugins.rdrNameOcc rdrName

startsUppercaseOccName :: Plugins.OccName -> Bool
startsUppercaseOccName occName =
  case Plugins.occNameString occName of
    headChar : _ ->
      isUpper headChar
    [] ->
      False

resolveSymbolRoot :: (MonadLore m) => Symbol -> m (Symbol, Plugins.Name)
resolveSymbolRoot symbol = do
  rootName <- resolveRootNameFromName symbol.name
  pure (symbol, rootName)

renderDisambiguationHint ::
  ModulePreferenceContext ->
  TypeQueryOccurrence ->
  ResolvedRootGroup ->
  Text
renderDisambiguationHint context occurrence rootGroup =
  case choosePreferredModuleForRoot context rootGroup.resolvedRootName rootGroup.resolvedRootSymbols of
    PreferredModule moduleName ->
      T.pack (GHC.moduleNameString moduleName) <> "." <> baseName
    NoUsableModule ->
      baseName
  where
    baseName =
      T.pack (Plugins.occNameString (Plugins.rdrNameOcc occurrence.typeQueryOccurrenceRdrName))

preferredImportForRoot :: ModulePreferenceContext -> ResolvedRootGroup -> Maybe GHC.ModuleName
preferredImportForRoot context rootGroup =
  case choosePreferredModuleForRoot context rootGroup.resolvedRootName rootGroup.resolvedRootSymbols of
    PreferredModule moduleName ->
      Just moduleName
    NoUsableModule ->
      Nothing

buildImports ::
  Set.Set GHC.ModuleName ->
  ParsedTypeQuery ->
  [GHC.ModuleName] ->
  [GHC.InteractiveImport]
buildImports homeModuleNames parsed resolvedImports =
  map (mkImportForUnqualifiedResolvedSymbol homeModuleNames) resolvedImports
    <> qualifiedImports
  where
    qualifiedImports =
      [ mkImportForQualifiedOccurrence homeModuleNames moduleName
      | occurrence <- parsed.parsedTypeQueryOccurrences,
        TypeQueryQualified moduleName <- [occurrence.typeQueryOccurrenceQualification]
      ]

mkImportForUnqualifiedResolvedSymbol ::
  Set.Set GHC.ModuleName ->
  GHC.ModuleName ->
  GHC.InteractiveImport
mkImportForUnqualifiedResolvedSymbol homeModuleNames moduleName =
  if moduleName `Set.member` homeModuleNames
    then GHC.IIModule moduleName
    else GHC.IIDecl (GHC.simpleImportDecl moduleName)

mkImportForQualifiedOccurrence ::
  Set.Set GHC.ModuleName ->
  GHC.ModuleName ->
  GHC.InteractiveImport
mkImportForQualifiedOccurrence homeModuleNames moduleName =
  if moduleName `Set.member` homeModuleNames
    then GHC.IIModule moduleName
    else
      GHC.IIDecl $
        (GHC.simpleImportDecl moduleName)
          { GHC.ideclQualified = GHC.QualifiedPre
          }

dedupeInteractiveImports :: [GHC.InteractiveImport] -> [GHC.InteractiveImport]
dedupeInteractiveImports =
  map snd
    . sortOn fst
    . Map.toList
    . List.foldl'
      (\importsByKey import_ -> Map.insert (interactiveImportKey import_) import_ importsByKey)
      Map.empty

interactiveImportKey :: GHC.InteractiveImport -> (Text, Text)
interactiveImportKey import_ =
  case import_ of
    GHC.IIModule moduleName ->
      ("module", T.pack (GHC.moduleNameString moduleName))
    GHC.IIDecl importDecl ->
      let moduleName =
            T.pack (GHC.moduleNameString (GHC.unLoc importDecl.ideclName))
          qualifier =
            case importDecl.ideclQualified of
              GHC.NotQualified -> "unqualified"
              GHC.QualifiedPre -> "qualified-pre"
              GHC.QualifiedPost -> "qualified-post"
       in ("decl:" <> qualifier, moduleName)

withAdditionalInteractiveImports :: (MonadLore m) => [GHC.InteractiveImport] -> m a -> m a
withAdditionalInteractiveImports extraImports action =
  if null extraImports
    then action
    else do
      originalContext <- GHC.getContext
      GHC.setContext (originalContext <> extraImports)
      action `finally` GHC.setContext originalContext
