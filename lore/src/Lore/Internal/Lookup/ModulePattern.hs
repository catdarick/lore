module Lore.Internal.Lookup.ModulePattern
  ( ModulePattern,
    ModulePatternError (..),
    compileModulePattern,
    matchesModulePattern,
  )
where

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
    literalParts :: [Text]
  }
  deriving stock (Eq, Show)

data ModulePatternError
  = EmptyModulePattern
  deriving stock (Eq, Show)

compileModulePattern :: Text -> Either ModulePatternError ModulePattern
compileModulePattern rawPattern
  | T.null rawPattern = Left EmptyModulePattern
  | T.all (== '*') rawPattern = Right MatchAllModules
  | "*" `T.isInfixOf` rawPattern =
      Right
        ( WildcardModulePattern
            WildcardModulePattern'
              { startsWithWildcard = "*" `T.isPrefixOf` rawPattern,
                endsWithWildcard = "*" `T.isSuffixOf` rawPattern,
                literalParts = filter (not . T.null) (T.splitOn "*" rawPattern)
              }
        )
  | otherwise =
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
    firstPart = head wildcardPattern.literalParts
    finalPart = last wildcardPattern.literalParts

    startsAtRequiredPosition =
      wildcardPattern.startsWithWildcard || firstPart `T.isPrefixOf` moduleText

    (initialOffset, remainingParts)
      | wildcardPattern.startsWithWildcard = (0, wildcardPattern.literalParts)
      | otherwise = (T.length firstPart, tail wildcardPattern.literalParts)

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
