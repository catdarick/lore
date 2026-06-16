module Lore.Mcp.Tools.FindReferences
  ( findReferencesTool,
  )
where

import qualified Data.Aeson as J
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), Maximum, Minimum, WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..), renderToolRun)
import qualified Lore.Tools.FindReferences as ToolsFindReferences
import Lore.Tools.Pagination (ToolPolicy (..), limitToIntWithDefault, mcpDefaultToolPolicy)
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result
  ( PageRequest (..),
    ResultLimit (..),
  )

data FindReferencesArgs (fieldType :: FieldType) = FindReferencesArgs
  { symbol ::
      Field fieldType Text
        `WithMeta` '[ Description "Exact symbol name to find references for. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope. Examples: \"lookupOrZero\", \"Some.Module.someFunction\", \"Some.Module.fieldName@OwnerType\"."
                    ],
    skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial results to skip. Use it only if a previous result was truncated and you want to see the next page of results.",
                      Example 15,
                      Minimum 0,
                      Maximum 9999
                    ],
    maxResults ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Optional cap for references to return.",
                      Example 2,
                      Minimum 1,
                      Maximum 15
                    ],
    verbosity ::
      Field fieldType FindReferencesVerbosity
        `WithMeta` '[ Description "Controls source context size around each reference. Low returns just the usage, use this when you just need usage examples. Medium returns usage plus root-symbol placement where it is used. High returns broad context, including surrounding control-flow branching."
                    ]
  }
  deriving stock (Generic)

data FindReferencesVerbosity
  = Low
  | Medium
  | High
  deriving stock (Eq, Generic, Show)

instance J.FromJSON FindReferencesVerbosity

instance ToSchema FindReferencesVerbosity

instance J.FromJSON (FindReferencesArgs 'ValueType)

instance ToSchema (FindReferencesArgs 'MetadataType)

findReferencesTool :: (MonadLore m) => SomeTool m
findReferencesTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "findReferences",
        description = Just "Find source references to a known symbol across loaded home modules. Results include source locations and context controlled by verbosity.",
        handler = findReferencesHandler
      }

findReferencesHandler :: (MonadLore m) => FindReferencesArgs 'ValueType -> m LoreDoc
findReferencesHandler FindReferencesArgs {symbol, skip, maxResults, verbosity} = do
  result <-
    ToolsFindReferences.findReferences
      ToolsFindReferences.FindReferencesOptions
        { ToolsFindReferences.findReferencesQuery = symbol,
          ToolsFindReferences.findReferencesPageRequest =
            PageRequest
              { pageOffset = max 0 (fromMaybe 0 skip),
                pageLimit = Limit (clampFindReferencesMaxResults maxResults)
              },
          ToolsFindReferences.findReferencesVerbosity = toCoreVerbosity verbosity
        }
  pure $ renderToolRun ToolsFindReferences.renderFindReferencesOutput result

clampFindReferencesMaxResults :: Maybe Int -> Int
clampFindReferencesMaxResults =
  max 1 . min defaultReferenceCap . fromMaybe defaultReferenceCap

toCoreVerbosity :: FindReferencesVerbosity -> ToolsFindReferences.FindReferencesVerbosity
toCoreVerbosity = \case
  Low -> ToolsFindReferences.Low
  Medium -> ToolsFindReferences.Medium
  High -> ToolsFindReferences.High

defaultReferenceCap :: Int
defaultReferenceCap =
  limitToIntWithDefault 15 (referenceLimit mcpDefaultToolPolicy)
