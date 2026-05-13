{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.ImportOps
  ( NormalizedImportItem,
    unNormalizedImportItem,
    mkNormalizedImportItem,
    mkFlatRemovalTarget,
    mkWholeImportItemTarget,
    mkScopedRemovalTarget,
    ImportRemovalTarget (RemoveFlatBinding, RemoveWholeImportItem, RemoveParentChild),
    ImportOperation (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.AutoRefactor.ImportDecl (ImportId)

newtype NormalizedImportItem = NormalizedImportItem
  { unNormalizedImportItem :: Text
  }
  deriving stock (Eq, Ord, Show)

mkNormalizedImportItem :: Text -> NormalizedImportItem
mkNormalizedImportItem =
  NormalizedImportItem . normalizedFlatBindingText

mkFlatRemovalTarget :: Text -> ImportRemovalTarget
mkFlatRemovalTarget bindingText =
  RemoveFlatBinding (mkNormalizedImportItem bindingText)

mkWholeImportItemTarget :: Text -> ImportRemovalTarget
mkWholeImportItemTarget bindingText =
  RemoveWholeImportItem (mkNormalizedImportItem bindingText)

mkScopedRemovalTarget :: Text -> Text -> ImportRemovalTarget
mkScopedRemovalTarget parentBinding childBinding =
  RemoveParentChild
    (mkNormalizedImportItem parentBinding)
    (mkNormalizedImportItem childBinding)

data ImportRemovalTarget
  = RemoveFlatBinding NormalizedImportItem
  | RemoveWholeImportItem NormalizedImportItem
  | RemoveParentChild NormalizedImportItem NormalizedImportItem
  deriving (Eq, Ord, Show)

data ImportOperation
  = RemoveImportItems ImportId (NonEmpty ImportRemovalTarget)
  | RemoveWholeImport ImportId
  deriving (Eq, Show)

normalizedFlatBindingText :: Text -> Text
normalizedFlatBindingText rawText =
  unwrapOperatorParens . stripPatternKeyword . T.strip $ rawText
  where
    stripPatternKeyword text =
      maybe text T.strip (T.stripPrefix "pattern " text)

    unwrapOperatorParens text
      | T.length text >= 2,
        T.head text == '(',
        T.last text == ')',
        let inner = T.init (T.tail text),
        not (T.null inner),
        T.all (`notElem` [' ', '(', ')', ',']) inner =
          inner
      | otherwise =
          text
