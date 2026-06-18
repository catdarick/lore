# Lore tools

This guide is for developers who decide whether Lore belongs in their agent workflow.

Lore tools give an agent bounded, compiler-aware answers. Raw terminal commands still matter, but they usually return text: file names, matching lines, or long logs. Lore returns resolved Haskell facts: symbols, types, source definitions, references, instances, diagnostics, and test status.

After source changes, the agent can run [`reloadHomeModules`](tools/reloadHomeModules.md). That refreshes the GHC session and the indexes used by the other tools.

## Project and modules

| Tool | What the agent can learn | Benefit over terminal commands |
| --- | --- | --- |
| [`discoverProject`](tools/discoverProject.md) | Packages, components, source dirs, shared dependencies, extensions, and GHC options. | It reads Cabal/Stack structure directly. File listing shows paths, but not the build shape that GHC uses. |
| [`discoverDirectory`](tools/discoverDirectory.md) | A bounded tree for a project directory. | It respects the project boundary and `.gitignore`, and it trims noisy directories. Plain file listing can flood the context. |
| [`listExportedSymbols`](tools/listExportedSymbols.md) | One module's public API, including re-exports. | It asks GHC what the module exports. Text search misses re-exports and hides constructor or method structure. |

## Symbols and source

| Tool | What the agent can learn | Benefit over text search |
| --- | --- | --- |
| [`searchSymbols`](tools/searchSymbols.md) | Likely symbols when the exact name is unknown. | It searches names, modules, aliases, and type heads. Text search only searches characters and gives many false positives. |
| [`lookupSymbolInfo`](tools/lookupSymbolInfo.md) | Type signatures, declaration headers, constructors, instances, definition locations, and exports. | It gives interface facts without opening full files or starting GHCi manually. |
| [`getDefinitions`](tools/getDefinitions.md) | Source for exact symbols, with optional dependency expansion. | It returns the needed declarations, not entire files. This saves tokens compared with broad file output. |
| [`findReferences`](tools/findReferences.md) | Resolved uses of a known symbol. | It follows GHC names, so overloaded names and record fields are not confused with same-text matches. |
| [`findDeadCode`](tools/findDeadCode.md) | Top-level declarations unreachable from project roots. | It uses a definition graph and component roots. Text search cannot tell whether code is reachable. |

## Types, instances, and evaluation

| Tool | What the agent can learn | Benefit over terminal commands |
| --- | --- | --- |
| [`lookupInstances`](tools/lookupInstances.md) | Indexed class and family instances whose heads mention several names. | It queries GHC's instance index instead of searching fragile `instance` text. |
| [`resolveInstance`](tools/resolveInstance.md) | The exact class instance GHC selects for a concrete type. | It removes guesswork from instance resolution, especially with wrappers or constraints. |
| [`getTypeOfExpression`](tools/getTypeOfExpression.md) | The inferred type of an expression. | It asks the project interpreter directly. A terminal workflow needs a temporary file, GHCi setup, or manual imports. |
| [`executeCode`](tools/executeCode.md) | A small runtime result in the project context. | It is lighter than writing a script or running a full test suite for one expression. |
| [`createTemporalModule`](tools/createTemporalModule.md) | A temporary module for multi-line debugging code. | It gives the agent a controlled place for helpers instead of patching real source files. |

## Validation and optional tools

| Tool | Availability | What the agent can do | Benefit over terminal commands |
| --- | --- | --- | --- |
| [`reloadHomeModules`](tools/reloadHomeModules.md) | Enabled by default | Compile home modules and refresh Lore's indexes. | It returns structured status, grouped diagnostics, snippets, and safe import fixes instead of a long build log. |
| [`runTestSuite`](tools/runTestSuite.md) | Disabled by default | Compile first, then run Cabal or Stack test components. | It returns component-level status and diagnostics that are easier for recovery workflows to track than raw test output. |
| [`notifyKnowledgeReset`](tools/notifyKnowledgeReset.md) | Definition cache enabled | Reset definition duplicate-suppression memory. | It keeps definition caching correct after a client-side context reset. Terminal commands have no matching state. |
| [`feedback`](tools/feedback.md) | Feedback file configured | Save a concise issue report for maintainers. | It captures structured feedback in the project instead of leaving it in chat history. |
| [Custom command tools](tools/custom-command-tools.md) | Project configured | Run a trusted project command through MCP. | The project can expose a stable, documented command surface instead of asking the agent to invent commands. |

## A context-efficient agent workflow

1. The agent searches with `searchSymbols` only when it does not know the exact symbol name.
2. It asks `lookupSymbolInfo` when a type or declaration header is enough.
3. It asks `getDefinitions` when it needs implementation source, starting with `None` expansion.
4. It follows references, instances, tests, or diagnostics with narrow tools instead of broad terminal output.
