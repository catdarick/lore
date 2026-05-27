module LoreDocMarkdownSpec
  ( spec,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore.Tools.Render.Doc
  ( LoreBlock (..),
    LoreDoc (..),
    SourceFile (..),
    SourceSection (..),
    bulletList,
    numberedListFrom,
    paragraph,
  )
import Lore.Tools.Render.Markdown (renderLoreDocMarkdown)
import Test.Hspec

spec :: Spec
spec =
  describe "renderLoreDocMarkdown" do
    it "renders heading blocks" do
      renderLoreDocMarkdown (LoreDoc [Heading1 "A", Heading2 "B", Heading3 "C"])
        `shouldBe` "# A\n\n## B\n\n### C"

    it "preserves paragraph newlines" do
      renderLoreDocMarkdown (paragraph "line1\nline2\nline3")
        `shouldBe` "line1\nline2\nline3"

    it "renders source files without fenced code blocks" do
      let rendered =
            renderLoreDocMarkdown
              ( LoreDoc
                  [ SourceFileBlock
                      SourceFile
                        { sourceFilePath = "src/Foo.hs",
                          sourceFileSections =
                            [ SourceSection
                                { sourceSectionTitle = "lines 12-24",
                                  sourceSectionText = "foo :: Int -> Int\nfoo x = x + 1"
                                }
                            ]
                        }
                  ]
              )
      rendered
        `shouldBe` "## src/Foo.hs\n\n### lines 12-24\n\nfoo :: Int -> Int\nfoo x = x + 1"
      rendered `shouldNotContainText` "```"

    it "renders numbered list with non-1 start" do
      renderLoreDocMarkdown (numberedListFrom 31 [paragraph "pageDef31", paragraph "pageDef32"])
        `shouldBe` "31. pageDef31\n32. pageDef32"

    it "indents multiline list item continuation lines" do
      let rendered =
            renderLoreDocMarkdown
              ( numberedListFrom
                  2
                  [ paragraph "first line\nsecond line",
                    bulletList [paragraph "inner one\ninner two"]
                  ]
              )
      rendered `shouldContainText` "2. first line\n   second line"
      rendered `shouldContainText` "3. - inner one\n     inner two"

shouldContainText :: Text -> Text -> Expectation
shouldContainText actual expected =
  T.isInfixOf expected actual `shouldBe` True

shouldNotContainText :: Text -> Text -> Expectation
shouldNotContainText actual expected =
  T.isInfixOf expected actual `shouldBe` False
