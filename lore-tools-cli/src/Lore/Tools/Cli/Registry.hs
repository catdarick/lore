module Lore.Tools.Cli.Registry
  ( cliTools,
  )
where

import Lore.Tools.Cli.Internal.Tool
  ( LoreCliM,
    SomeCliTool (SomeCliTool),
  )
import Lore.Tools.Cli.Tools.CreateTemporalModule (createTemporalModuleCliTool)
import Lore.Tools.Cli.Tools.DebugCacheMemory (debugCacheMemoryCliTool)
import Lore.Tools.Cli.Tools.DiscoverDirectory (discoverDirectoryCliTool)
import Lore.Tools.Cli.Tools.DiscoverProject (discoverProjectCliTool)
import Lore.Tools.Cli.Tools.ExecuteCode (executeCodeCliTool)
import Lore.Tools.Cli.Tools.FindDeadCode (findDeadCodeCliTool)
import Lore.Tools.Cli.Tools.FindReferences (findReferencesCliTool)
import Lore.Tools.Cli.Tools.GetDefinition (getDefinitionCliTool)
import Lore.Tools.Cli.Tools.GetTypeOfExpression (getTypeOfExpressionCliTool)
import Lore.Tools.Cli.Tools.ListExportedSymbols (listExportedSymbolsCliTool)
import Lore.Tools.Cli.Tools.LookupInstances (lookupInstancesCliTool)
import Lore.Tools.Cli.Tools.LookupSymbolInfo (lookupSymbolInfoCliTool)
import Lore.Tools.Cli.Tools.Reload (reloadCliTool)
import Lore.Tools.Cli.Tools.ResolveInstance (resolveInstanceCliTool)
import Lore.Tools.Cli.Tools.RunTestSuite (runTestSuiteCliTool)
import Lore.Tools.Cli.Tools.SearchSymbols (searchSymbolsCliTool)

cliTools :: [SomeCliTool LoreCliM]
cliTools =
  [ SomeCliTool reloadCliTool,
    SomeCliTool discoverProjectCliTool,
    SomeCliTool discoverDirectoryCliTool,
    SomeCliTool searchSymbolsCliTool,
    SomeCliTool lookupSymbolInfoCliTool,
    SomeCliTool listExportedSymbolsCliTool,
    SomeCliTool getDefinitionCliTool,
    SomeCliTool findDeadCodeCliTool,
    SomeCliTool findReferencesCliTool,
    SomeCliTool lookupInstancesCliTool,
    SomeCliTool resolveInstanceCliTool,
    SomeCliTool createTemporalModuleCliTool,
    SomeCliTool debugCacheMemoryCliTool,
    SomeCliTool getTypeOfExpressionCliTool,
    SomeCliTool executeCodeCliTool,
    SomeCliTool runTestSuiteCliTool
  ]
