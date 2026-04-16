{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lore.Internal.Lookup.Name
  ( NormalizedOccName,
    NormalizedModuleName,
    NormalizedName (occName, moduleName),
    normalizeName,
    parseAndNormalizeName,
    unNormalizedOccName,
    unNormalizedModuleName,
    normalizeModuleName,
    mkNormalizedModuleName,
    mkGhcModuleName,
    extractAndNormalizeOccName,
    extractAndNormalizeModuleName,
  )
where

import Control.DeepSeq (NFData)
import Data.Char (isAlphaNum, isUpper)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import qualified GHC.Types.Name as GHC

newtype NormalizedOccName = NormalizedOccName
  { unNormalizedOccName :: Text
  }
  deriving newtype (NFData, Eq, Ord, Show)

newtype NormalizedModuleName = NormalizedModuleName
  { unNormalizedModuleName :: Text
  }
  deriving newtype (NFData, Eq, Ord, Show)

data NormalizedName = NormalizedName
  { moduleName :: Maybe NormalizedModuleName,
    occName :: NormalizedOccName
  }
  deriving (Generic, NFData, Eq, Ord)

instance Show NormalizedName where
  show (NormalizedName maybeModuleName (NormalizedOccName occName)) =
    case maybeModuleName of
      Nothing -> T.unpack occName
      Just (NormalizedModuleName moduleName) -> T.unpack moduleName <> "." <> T.unpack occName

normalizeOccName :: Text -> NormalizedOccName
normalizeOccName occName = NormalizedOccName
  case T.stripPrefix "(" occName >>= T.stripSuffix ")" of
    Just inner -> inner
    Nothing -> occName

extractAndNormalizeOccName :: GHC.Name -> NormalizedOccName
extractAndNormalizeOccName ghcName =
  let occName = T.pack (GHC.getOccString ghcName)
   in normalizeOccName occName

normalizeName :: GHC.Name -> NormalizedName
normalizeName ghcName =
  NormalizedName
    { moduleName = extractAndNormalizeModuleName <$> GHC.nameModule_maybe ghcName,
      occName = extractAndNormalizeOccName ghcName
    }

normalizeModuleName :: GHC.ModuleName -> NormalizedModuleName
normalizeModuleName mdlName =
  let moduleName = T.pack $ GHC.moduleNameString mdlName
   in NormalizedModuleName moduleName

mkNormalizedModuleName :: Text -> NormalizedModuleName
mkNormalizedModuleName = NormalizedModuleName

mkGhcModuleName :: NormalizedModuleName -> GHC.ModuleName
mkGhcModuleName (NormalizedModuleName moduleName) =
  GHC.mkModuleName (T.unpack moduleName)

extractAndNormalizeModuleName :: GHC.Module -> NormalizedModuleName
extractAndNormalizeModuleName mdl = do
  normalizeModuleName $ GHC.moduleName mdl

parseAndNormalizeName :: Text -> NormalizedName
parseAndNormalizeName queryText =
  case qualifiedCandidates of
    (moduleHint, occName) : _ ->
      NormalizedName
        { moduleName = Just (NormalizedModuleName moduleHint),
          occName = normalizeOccName occName
        }
    [] ->
      NormalizedName
        { moduleName = Nothing,
          occName = normalizeOccName queryText
        }
  where
    segments = T.splitOn "." queryText
    qualifiedCandidates =
      reverse $
        mapMaybe mkCandidate [1 .. length segments - 1]

    mkCandidate prefixLen = do
      let moduleSegments = take prefixLen segments
          occSegments = drop prefixLen segments
          moduleHint = T.intercalate "." moduleSegments
          occName = T.intercalate "." occSegments
      if all isModuleNameSegment moduleSegments && not (T.null occName)
        then Just (moduleHint, occName)
        else Nothing

isModuleNameSegment :: Text -> Bool
isModuleNameSegment segment =
  case T.uncons segment of
    Nothing ->
      False
    Just (firstChar, rest) ->
      isUpper firstChar && T.all isModuleNameChar rest

isModuleNameChar :: Char -> Bool
isModuleNameChar char =
  isAlphaNum char || char == '_' || char == '\''
