module Lore.Mcp.Tools.Feedback
  ( feedbackTool,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

data FeedbackArgs (fieldType :: FieldType) = FeedbackArgs
  { title ::
      Field fieldType Text
        `WithMeta` '[ Description "Short feedback title.",
                      Example "feature request",
                      Example "bug report",
                      Example "rendering issue"
                    ],
    content ::
      Field fieldType Text
        `WithMeta` '[ Description "Feedback body. For example, for bugs, include what happened, why it seems wrong, and any reproduction hints. For feature requests, describe the desired behavior and its benefits.",
                      Example "Observed duplicate entries for the same symbol after reload. Expected one entry. Steps: ..."
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (FeedbackArgs 'ValueType)

instance ToSchema (FeedbackArgs 'MetadataType)

feedbackTool :: (MonadLore m) => FilePath -> SomeTool m
feedbackTool feedbackFilePath =
  SomeToolWithArgs
    ToolWithArgs
      { name = "feedback",
        description = Just "Persist structured feedback for maintainers. Use this when you observe rendering issues (corrupted output, duplication, truncation), suspected bugs, unexpected behavior, or when you want to request a new feature/workflow improvement.",
        handler = feedbackHandler feedbackFilePath
      }

feedbackHandler :: (MonadLore m) => FilePath -> FeedbackArgs 'ValueType -> m Text
feedbackHandler feedbackFilePath FeedbackArgs {title, content} = do
  liftIO do
    let targetDirectory = takeDirectory feedbackFilePath
    when (not (null targetDirectory) && targetDirectory /= ".") do
      createDirectoryIfMissing True targetDirectory
    TIO.appendFile feedbackFilePath (renderFeedbackEntry title content)
  pure ("Feedback appended to " <> T.pack feedbackFilePath <> ".")

renderFeedbackEntry :: Text -> Text -> Text
renderFeedbackEntry title content =
  T.concat
    [ "#### ",
      T.strip title,
      "\n\n",
      T.strip content,
      "\n\n"
    ]
