module Lore.Tools.CreateTemporalModule
  ( CreateTemporalModuleOutput (..),
    createTemporalModule,
    renderCreateTemporalModule,
  )
where

import qualified Data.Text as T
import qualified Lore as Core
import Lore.Tools.Render.Doc (LoreDoc, bulletList, heading2, numberedListFrom, paragraph)

newtype CreateTemporalModuleOutput = CreateTemporalModuleOutput
  { temporalModulePath :: FilePath
  }
  deriving stock (Eq, Show)

createTemporalModule :: (Core.MonadLore m) => m CreateTemporalModuleOutput
createTemporalModule = do
  path <- Core.createTemporalModule
  pure (CreateTemporalModuleOutput path)

renderCreateTemporalModule :: CreateTemporalModuleOutput -> LoreDoc
renderCreateTemporalModule output =
  paragraph ("Temporal module initialized at: " <> T.pack output.temporalModulePath)
    <> heading2 "Workflow"
    <> numberedListFrom
      1
      [ paragraph "Write custom logic and necessary imports directly into this file.",
        paragraph "Call reloadHomeModules to compile and load it into the session.",
        paragraph "Use executeCode to run your target functions."
      ]
    <> heading2 "Notes"
    <> bulletList
      [ paragraph "Active Haskell extensions set may be different.",
        paragraph "Reuse this file until your debugging task is done.",
        paragraph "Delete it when finished to detach."
      ]
