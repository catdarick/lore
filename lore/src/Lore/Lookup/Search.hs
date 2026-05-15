module Lore.Lookup.Search
  ( tokenizeSearchTextValues,
    rankSearchTexts,
  )
where

import Data.Text (Text)
import Lore.Internal.Lookup.Search.Score (rankSearchTexts)
import Lore.Internal.Lookup.Search.Tokenize (tokenizeSearchText)
import Lore.Internal.Lookup.Search.Types (SearchToken (..))

tokenizeSearchTextValues :: Text -> [Text]
tokenizeSearchTextValues =
  map unSearchToken . tokenizeSearchText
