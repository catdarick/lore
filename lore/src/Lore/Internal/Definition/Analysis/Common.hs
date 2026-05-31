module Lore.Internal.Definition.Analysis.Common
  ( collectLocatedRdrNames,
    collectTyped,
    dotFieldLabelRdrNamePs,
    dotFieldLabelRdrNameRn,
    fieldLabelStringToRdrName,
    locatedSpan,
    nameUniqueKey,
  )
where

import Data.Data (Data, Typeable, cast, gmapQ)
import Data.Maybe (maybeToList)
import qualified GHC
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Unique as GHCUnique

collectLocatedRdrNames :: GHC.ParsedSource -> [GHC.LocatedN GHC.RdrName]
collectLocatedRdrNames parsedSource =
  collectTyped parsedSource

collectTyped :: forall b a. (Typeable b, Data a) => a -> [b]
collectTyped = go
  where
    go :: forall x. (Data x) => x -> [b]
    go value =
      maybeToList (cast value) <> concat (gmapQ go value)

dotFieldLabelRdrNamePs :: GHC.DotFieldOcc GHC.GhcPs -> GHC.RdrName
dotFieldLabelRdrNamePs dotFieldOccurrence =
  fieldLabelStringToRdrName (GHC.unLoc dotFieldOccurrence.dfoLabel)

dotFieldLabelRdrNameRn :: GHC.DotFieldOcc GHC.GhcRn -> GHC.RdrName
dotFieldLabelRdrNameRn dotFieldOccurrence =
  fieldLabelStringToRdrName (GHC.unLoc dotFieldOccurrence.dfoLabel)

fieldLabelStringToRdrName :: GHC.FieldLabelString -> GHC.RdrName
fieldLabelStringToRdrName fieldLabelString =
  GHC.mkRdrUnqual $
    GHC.mkVarOcc $
      GHC.showSDocUnsafe (GHC.ppr fieldLabelString)

locatedSpan :: GHC.LocatedN a -> GHC.SrcSpan
locatedSpan =
  GHC.locA . GHC.getLoc

nameUniqueKey :: GHC.Name -> Int
nameUniqueKey =
  fromIntegral . GHCUnique.getKey . GHC.getUnique
