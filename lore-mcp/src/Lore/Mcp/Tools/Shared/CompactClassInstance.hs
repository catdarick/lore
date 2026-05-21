module Lore.Mcp.Tools.Shared.CompactClassInstance
  ( CompactClassInstance (..),
    renderCompactClassInstanceLabel,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Core.Class as Class
import qualified GHC.Core.InstEnv as InstEnv
import qualified GHC.Plugins as Plugins
import qualified GHC.Types.TyThing as TyThing
import Lore (SymbolInfo (..))
import Lore.Mcp.Tools.Shared.Outputable (renderOutputable)

data CompactClassInstance = CompactClassInstance SymbolInfo InstEnv.ClsInst

renderCompactClassInstanceLabel :: CompactClassInstance -> Text
renderCompactClassInstanceLabel (CompactClassInstance symbolInfo classInstance) =
  case symbolContext symbolInfo of
    SymbolContextTypeLike
      | classParameterCount classInstance <= 1 ->
          classNameText classInstance
    SymbolContextClass
      | classParameterCount classInstance == 2 ->
          classArgumentsText classInstance
    _ ->
      stripInstancePrefix (compactRenderedInstanceText (renderOutputable classInstance))

data SymbolContext
  = SymbolContextTypeLike
  | SymbolContextClass
  | SymbolContextOther

symbolContext :: SymbolInfo -> SymbolContext
symbolContext symbolInfo =
  case symbolInfo.symbolThing of
    TyThing.ATyCon tyCon
      | Plugins.isClassTyCon tyCon ->
          SymbolContextClass
      | otherwise ->
          SymbolContextTypeLike
    _ ->
      SymbolContextOther

classParameterCount :: InstEnv.ClsInst -> Int
classParameterCount classInstance =
  length (Class.classTyVars classInstance.is_cls)

classNameText :: InstEnv.ClsInst -> Text
classNameText classInstance =
  T.pack (Plugins.getOccString (Plugins.getName classInstance.is_cls))

classArgumentsText :: InstEnv.ClsInst -> Text
classArgumentsText classInstance =
  case classInstance.is_tys of
    [] ->
      classNameText classInstance
    types_ ->
      T.intercalate " " (map (compactRenderedInstanceText . renderOutputable) types_)

compactRenderedInstanceText :: Text -> Text
compactRenderedInstanceText =
  T.unwords
    . takeWhile (not . isDefinitionCommentLine)
    . filter (not . T.null)
    . map (stripTrailingComment . T.strip)
    . T.lines

stripInstancePrefix :: Text -> Text
stripInstancePrefix text =
  fromMaybe text (T.stripPrefix "instance " text)

stripTrailingComment :: Text -> Text
stripTrailingComment text =
  T.strip $
    case T.breakOn " -- " text of
      (prefix, suffix)
        | T.null suffix -> text
        | otherwise -> prefix

isDefinitionCommentLine :: Text -> Bool
isDefinitionCommentLine =
  T.isPrefixOf "-- Defined"
