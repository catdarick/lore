module Lore.Mcp.Internal.LoreDoc
  ( LoreDoc (..),
    LoreBlock (..),
    NumberedList (..),
    SourceFile (..),
    SourceSection (..),
    ToLoreDoc (..),
    paragraph,
    heading1,
    heading2,
    heading3,
    bulletList,
    numberedListFrom,
    sourceFile,
  )
where

import Data.Text (Text)
import GHC.Natural (Natural)

newtype LoreDoc = LoreDoc
  { loreDocBlocks :: [LoreBlock]
  }
  deriving stock (Eq, Show)

instance Semigroup LoreDoc where
  LoreDoc xs <> LoreDoc ys =
    LoreDoc (xs <> ys)

instance Monoid LoreDoc where
  mempty =
    LoreDoc []

data LoreBlock
  = Heading1 Text
  | Heading2 Text
  | Heading3 Text
  | Paragraph Text
  | BulletList [LoreDoc]
  | NumberedListBlock NumberedList
  | SourceFileBlock SourceFile
  deriving stock (Eq, Show)

data NumberedList = NumberedList
  { numberedListStart :: Natural,
    numberedListItems :: [LoreDoc]
  }
  deriving stock (Eq, Show)

data SourceFile = SourceFile
  { sourceFilePath :: Text,
    sourceFileSections :: [SourceSection]
  }
  deriving stock (Eq, Show)

data SourceSection = SourceSection
  { sourceSectionTitle :: Text,
    sourceSectionText :: Text
  }
  deriving stock (Eq, Show)

class ToLoreDoc a where
  toLoreDoc :: a -> LoreDoc

instance ToLoreDoc LoreDoc where
  toLoreDoc = id

paragraph :: Text -> LoreDoc
paragraph text =
  LoreDoc [Paragraph text]

heading1 :: Text -> LoreDoc
heading1 text =
  LoreDoc [Heading1 text]

heading2 :: Text -> LoreDoc
heading2 text =
  LoreDoc [Heading2 text]

heading3 :: Text -> LoreDoc
heading3 text =
  LoreDoc [Heading3 text]

bulletList :: [LoreDoc] -> LoreDoc
bulletList items =
  LoreDoc [BulletList items]

numberedListFrom :: Natural -> [LoreDoc] -> LoreDoc
numberedListFrom start items =
  LoreDoc [NumberedListBlock NumberedList {numberedListStart = start, numberedListItems = items}]

sourceFile :: SourceFile -> LoreDoc
sourceFile source =
  LoreDoc [SourceFileBlock source]
