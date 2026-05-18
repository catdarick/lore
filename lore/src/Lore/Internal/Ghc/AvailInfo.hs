{-# LANGUAGE LambdaCase #-}

module Lore.Internal.Ghc.AvailInfo
  ( availInfosNameSet,
    availInfoNamesWithFields,
    availInfoGreNames,
    greNameFieldAliasText,
    fieldLabelAliasText,
  )
where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Avail as GHC
import qualified GHC.Types.FieldLabel as GHC.FieldLabel

availInfosNameSet :: [GHC.AvailInfo] -> Set.Set GHC.Name
availInfosNameSet availInfos =
  Set.fromList
    [ name
    | availInfo <- availInfos,
      name <- availInfoNamesWithFields availInfo
    ]

availInfoNamesWithFields :: GHC.AvailInfo -> [GHC.Name]
availInfoNamesWithFields = \case
  GHC.Avail greName ->
    [GHC.greNamePrintableName greName]
  GHC.AvailTC parentName subordinateNames ->
    parentName : map GHC.greNamePrintableName subordinateNames

availInfoGreNames :: GHC.AvailInfo -> [GHC.GreName]
availInfoGreNames = \case
  GHC.Avail greName ->
    [greName]
  GHC.AvailTC parentName subordinateNames ->
    GHC.NormalGreName parentName : subordinateNames

greNameFieldAliasText :: GHC.GreName -> Maybe Text
greNameFieldAliasText = \case
  GHC.FieldGreName fieldLabel ->
    Just (fieldLabelAliasText fieldLabel)
  GHC.NormalGreName _ ->
    Nothing

fieldLabelAliasText :: GHC.FieldLabel -> Text
fieldLabelAliasText fieldLabel =
  T.pack (GHC.getOccString (GHC.FieldLabel.fieldLabelPrintableName fieldLabel))
