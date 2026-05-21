module Lore.Internal.Lookup.TypeQuery.Parse
  ( ParsedTypeQuery (..),
    TypeQueryParseError (..),
    parseTypeQuery,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Driver.Env as DriverEnv
import qualified GHC.Driver.Main as DriverMain
import Lore.Internal.Lookup.TypeQuery.Names
  ( TypeQueryOccurrence,
    collectTypeQueryOccurrences,
  )
import Lore.Monad (MonadLore)
import UnliftIO (tryAny)

data ParsedTypeQuery = ParsedTypeQuery
  { parsedTypeQueryText :: !Text,
    parsedTypeQueryAst :: !(GHC.LHsType GHC.GhcPs),
    parsedTypeQueryOccurrences :: ![TypeQueryOccurrence]
  }

data TypeQueryParseError
  = TypeQueryParseFailed !Text
  | TypeQueryUnsupportedParsedType !Text

parseTypeQuery ::
  (MonadLore m) =>
  Text ->
  m (Either TypeQueryParseError ParsedTypeQuery)
parseTypeQuery queryText = do
  hscEnv <- GHC.getSession
  eiParsedType <-
    liftIO $
      tryAny $
        DriverEnv.runHsc hscEnv $
          DriverMain.hscParseType (T.unpack queryText)
  case eiParsedType of
    Left err ->
      pure (Left (TypeQueryParseFailed (T.pack (show err))))
    Right parsedType ->
      pure $
        case collectTypeQueryOccurrences parsedType of
          Left unsupported ->
            Left (TypeQueryUnsupportedParsedType unsupported)
          Right occurrences ->
            Right
              ParsedTypeQuery
                { parsedTypeQueryText = queryText,
                  parsedTypeQueryAst = parsedType,
                  parsedTypeQueryOccurrences = occurrences
                }
