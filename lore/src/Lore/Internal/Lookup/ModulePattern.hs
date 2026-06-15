module Lore.Internal.Lookup.ModulePattern
  ( ModulePattern,
    ModulePatternError (..),
    compileModulePattern,
    matchesModulePattern,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.Lookup.Name (NormalizedModuleName, unNormalizedModuleName)

data ModulePattern
  = ExactModulePattern Text
  | WildcardModulePattern WildcardModulePattern
  | MatchAllModules
  deriving stock (Eq, Show)

data WildcardModulePattern = WildcardModulePattern'
  { startsWithWildcard :: Bool,
    endsWithWildcard :: Bool,
    literalParts :: NonEmpty Text
  }
  deriving stock (Eq, Show)

data ModulePatternError
  = EmptyModulePattern
  deriving stock (Eq, Show)

compileModulePattern :: Text -> Either ModulePatternError ModulePattern
compileModulePattern rawPattern
  | T.null rawPattern = Left EmptyModulePattern
  | otherwise =
      case NonEmpty.nonEmpty (filter (not . T.null) (T.splitOn "*" rawPattern)) of
        Nothing -> Right MatchAllModules
        Just literalParts
          | "*" `T.isInfixOf` rawPattern ->
              Right
                ( WildcardModulePattern
                    WildcardModulePattern'
                      { startsWithWildcard = "*" `T.isPrefixOf` rawPattern,
                        endsWithWildcard = "*" `T.isSuffixOf` rawPattern,
                        literalParts
                      }
                )
          | otherwise ->
              Right (ExactModulePattern rawPattern)

matchesModulePattern :: ModulePattern -> NormalizedModuleName -> Bool
matchesModulePattern (ExactModulePattern expected) moduleName =
  expected == unNormalizedModuleName moduleName
matchesModulePattern MatchAllModules _ =
  True
matchesModulePattern (WildcardModulePattern wildcardPattern) moduleName =
  startsAtRequiredPosition
    && literalPartsOccurInOrder initialOffset remainingParts
    && endsAtRequiredPosition
  where
    moduleText = unNormalizedModuleName moduleName
    firstPart = NonEmpty.head wildcardPattern.literalParts
    finalPart = NonEmpty.last wildcardPattern.literalParts

    startsAtRequiredPosition =
      wildcardPattern.startsWithWildcard || firstPart `T.isPrefixOf` moduleText

    (initialOffset, remainingParts)
      | wildcardPattern.startsWithWildcard = (0, NonEmpty.toList wildcardPattern.literalParts)
      | otherwise = (T.length firstPart, NonEmpty.tail wildcardPattern.literalParts)

    endsAtRequiredPosition =
      wildcardPattern.endsWithWildcard || finalPart `T.isSuffixOf` moduleText

    literalPartsOccurInOrder _ [] =
      True
    literalPartsOccurInOrder offset (part : rest) =
      case findLiteralEndOffset offset part of
        Nothing -> False
        Just nextOffset -> literalPartsOccurInOrder nextOffset rest

    findLiteralEndOffset offset part =
      let candidateText = T.drop offset moduleText
          (beforeMatch, fromMatch) = T.breakOn part candidateText
       in if T.null fromMatch
            then Nothing
            else Just (offset + T.length beforeMatch + T.length part)
