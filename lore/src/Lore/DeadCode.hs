-- | Dead-code analysis API.
--
-- Note: instance definitions (class/type/data family instances) are marked
-- alive iff at least one definition corresponding to an instance-head type is
-- alive. If an instance head has no local type-definition matches (for
-- example, external-only heads like `Int`), the instance is treated as alive
-- by default.
module Lore.DeadCode
  ( DeadCodeOptions (..),
    DeadDefinition (..),
    DeadDefinitionKind (..),
    DeadCodeResult (..),
    findDeadCode,
  )
where

import Lore.Internal.DeadCode
