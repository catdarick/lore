module Lore.Internal.Lookup.SymbolResolutionCore
  ( ResolvedRootGroup (..),
    collectHomeModuleNames,
    resolveRootNameFromName,
    groupSymbolsByResolvedRoot,
    choosePreferredRootSymbol,
    chooseQualifierModuleName,
    symbolModuleNames,
    renderRootModuleName,
    renderRootName,
    findFirst,
    dedupeTexts,
  )
where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Plugins as Plugins
import Lore.Internal.Ghc.TyThing (tyThingRootName)
import Lore.Internal.Lookup.ModulePreference
  ( ModulePreferenceContext,
    PreferredModuleChoice (..),
    choosePreferredModuleForRoot,
  )
import Lore.Internal.Lookup.Types (Symbol (..), SymbolVisibility (..))
import Lore.Monad (MonadLore)

data ResolvedRootGroup = ResolvedRootGroup
  { resolvedRootName :: !Plugins.Name,
    resolvedRootSymbols :: !(NE.NonEmpty Symbol)
  }

collectHomeModuleNames :: (MonadLore m) => m (Set.Set GHC.ModuleName)
collectHomeModuleNames = do
  moduleGraph <- GHC.getModuleGraph
  pure $
    Set.fromList
      [ GHC.moduleName (GHC.ms_mod modSummary)
      | modSummary <- GHC.mgModSummaries moduleGraph
      ]

resolveRootNameFromName :: (MonadLore m) => Plugins.Name -> m Plugins.Name
resolveRootNameFromName name = do
  maybeTyThing <- GHC.lookupName name
  pure $
    case maybeTyThing of
      Nothing ->
        name
      Just tyThing ->
        tyThingRootName tyThing

groupSymbolsByResolvedRoot :: [(Symbol, Plugins.Name)] -> [ResolvedRootGroup]
groupSymbolsByResolvedRoot symbolsWithRoots =
  Map.elems $
    List.foldl'
      collectSymbol
      Map.empty
      symbolsWithRoots
  where
    collectSymbol groupedSymbols (symbol, rootName) =
      let key = renderRootName rootName
       in Map.alter
            ( \maybeExisting ->
                case maybeExisting of
                  Nothing ->
                    Just
                      ResolvedRootGroup
                        { resolvedRootName = rootName,
                          resolvedRootSymbols = symbol NE.:| []
                        }
                  Just existing ->
                    Just
                      existing
                        { resolvedRootSymbols = symbol NE.<| existing.resolvedRootSymbols
                        }
            )
            key
            groupedSymbols

choosePreferredRootSymbol :: ModulePreferenceContext -> ResolvedRootGroup -> Symbol
choosePreferredRootSymbol context group_ =
  let rootSymbolsList = NE.toList group_.resolvedRootSymbols
      preferredModule = chooseQualifierModuleName context group_
      maybePreferredSymbol =
        findFirst
          (\symbol -> preferredModule `elem` symbolModuleNames symbol)
          rootSymbolsList
   in maybe (NE.head group_.resolvedRootSymbols) id maybePreferredSymbol

chooseQualifierModuleName :: ModulePreferenceContext -> ResolvedRootGroup -> GHC.ModuleName
chooseQualifierModuleName context group_ =
  case choosePreferredModuleForRoot context group_.resolvedRootName group_.resolvedRootSymbols of
    PreferredModule moduleName ->
      moduleName
    NoUsableModule ->
      maybe
        (GHC.mkModuleName "<no-module>")
        Plugins.moduleName
        (Plugins.nameModule_maybe group_.resolvedRootName)

symbolModuleNames :: Symbol -> [GHC.ModuleName]
symbolModuleNames symbol =
  case symbol.visibility of
    Symbol'ExportedFrom exportedModules ->
      map Plugins.moduleName (Set.toList exportedModules)
    Symbol'Unexported ->
      maybe [] (pure . Plugins.moduleName) (Plugins.nameModule_maybe symbol.name)

renderRootName :: Plugins.Name -> String
renderRootName name =
  case Plugins.nameModule_maybe name of
    Nothing ->
      "<no-module>." <> Plugins.getOccString name
    Just module_ ->
      Plugins.moduleNameString (Plugins.moduleName module_) <> "." <> Plugins.getOccString name

renderRootModuleName :: Plugins.Name -> Text
renderRootModuleName name =
  case Plugins.nameModule_maybe name of
    Nothing ->
      "<no-module>"
    Just module_ ->
      T.pack (Plugins.moduleNameString (Plugins.moduleName module_))

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst predicate =
  foldr
    (\item maybeMatch -> if predicate item then Just item else maybeMatch)
    Nothing

dedupeTexts :: [Text] -> [Text]
dedupeTexts =
  reverse . snd . List.foldl' dedupeOne (Set.empty, [])
  where
    dedupeOne (seenKeys, dedupedValues) value =
      if value `Set.member` seenKeys
        then (seenKeys, dedupedValues)
        else (Set.insert value seenKeys, value : dedupedValues)
