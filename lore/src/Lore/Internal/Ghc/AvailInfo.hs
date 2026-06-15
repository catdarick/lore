{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Lore.Internal.Ghc.AvailInfo
  ( AvailableName (..),
    availInfoAvailableNames,
    availInfoSubordinateNames,
    availInfoForName,
    availInfosNameSet,
    availInfoNamesWithFields,
    fieldLabelAliasText,
  )
where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Avail as GHC
import qualified GHC.Types.FieldLabel as GHC.FieldLabel
import qualified Language.Haskell.Syntax.Basic as GHC.Syntax

data AvailableName = AvailableName
  { availableName :: !GHC.Name,
    availableFieldAlias :: !(Maybe Text)
  }

availInfosNameSet :: [GHC.AvailInfo] -> Set.Set GHC.Name
availInfosNameSet =
  Set.fromList . concatMap availInfoNamesWithFields

availInfoNamesWithFields :: GHC.AvailInfo -> [GHC.Name]
availInfoNamesWithFields =
  map (.availableName) . availInfoAvailableNames

availInfoSubordinateNames :: GHC.AvailInfo -> [GHC.Name]
#if MIN_VERSION_ghc(9,8,0)
availInfoSubordinateNames = GHC.availSubordinateNames
#else
availInfoSubordinateNames =
  map GHC.greNamePrintableName . GHC.availSubordinateGreNames
#endif

availInfoForName :: GHC.Name -> GHC.AvailInfo
#if MIN_VERSION_ghc(9,8,0)
availInfoForName = GHC.Avail
#else
availInfoForName = GHC.Avail . GHC.NormalGreName
#endif

{- ORMOLU_DISABLE -}
availInfoAvailableNames :: GHC.AvailInfo -> [AvailableName]
availInfoAvailableNames = \case 
#if MIN_VERSION_ghc(9,8,0)
  GHC.Avail name ->
    [availableNameWithoutAlias name]
  GHC.AvailTC parentName subordinateNames ->
    availableNameWithoutAlias parentName : map availableNameWithoutAlias subordinateNames
#else
  GHC.Avail greName ->
    [availableNameFromGreName greName]
  GHC.AvailTC parentName subordinateNames ->
    availableNameWithoutAlias parentName : map availableNameFromGreName subordinateNames
#endif
{- ORMOLU_ENABLE -}

availableNameWithoutAlias :: GHC.Name -> AvailableName
availableNameWithoutAlias name =
  AvailableName
    { availableName = name,
      availableFieldAlias = Nothing
    }

#if !MIN_VERSION_ghc(9,8,0)
availableNameFromGreName :: GHC.GreName -> AvailableName
availableNameFromGreName = \case
  GHC.FieldGreName fieldLabel ->
    AvailableName
      { availableName = GHC.greNamePrintableName (GHC.FieldGreName fieldLabel),
        availableFieldAlias = Just (fieldLabelAliasText fieldLabel)
      }
  GHC.NormalGreName name ->
    availableNameWithoutAlias name
#endif

fieldLabelAliasText :: GHC.FieldLabel -> Text
fieldLabelAliasText =
  T.pack
    . GHC.unpackFS
    . GHC.Syntax.field_label
    . GHC.FieldLabel.flLabel
