# Lore tools

Lore tools are designed to reduce context pressure in large Haskell codebases. Raw terminal commands still matter, but they usually return text: long file lists, broad search matches, whole source files, or noisy build logs. Lore returns bounded compiler-aware facts: project shape, module APIs, exact source definitions, references, instances, types, diagnostics, and test status.

After source changes, [`reloadHomeModules`](tools/reloadHomeModules.md) refreshes the GHC session and the indexes used by the other tools, returning paginated GHC diagnostics instead of raw CLI noise.

## Project and modules

| Tool | Returned information | Why enable it instead of relying on terminal commands |
| --- | --- | --- |
| [`discoverProject`](tools/discoverProject.md) | Packages, components, source dirs, shared dependencies, extensions, and GHC options. | It exposes build shape before source reads. File listing shows paths, but not the components and options that GHC uses. |
| [`discoverDirectory`](tools/discoverDirectory.md) | A bounded tree for a project directory. | It exposes layout without flooding context with ignored, generated, or deeply nested files. |
| [`listExportedSymbols`](tools/listExportedSymbols.md) | One module's public API, including re-exports. | It answers API questions before implementation reads. Text search misses re-exports and hides constructor or method structure. |

## Symbols and source

| Tool | Returned information | Why enable it instead of relying on text search |
| --- | --- | --- |
| [`searchSymbols`](tools/searchSymbols.md) | Likely symbols when the exact name is unknown. | It returns a small fuzzy/semantic candidate list from names, modules, aliases, and type heads instead of many same-text matches. |
| [`lookupSymbolInfo`](tools/lookupSymbolInfo.md) | Type signatures, declaration headers, constructors, instances, definition locations, and exports. | It often answers the question without retrieving source, keeping implementation context out until needed. |
| [`getDefinitions`](tools/getDefinitions.md) | Source for exact symbols, with optional dependency expansion. | It retrieves declarations and selected dependency layers instead of full files, which is the main source-context saver. |
| [`findReferences`](tools/findReferences.md) | Resolved uses of a known symbol. | It returns focused snippets around real GHC references, avoiding broad grep output and same-text false positives. |
| [`findDeadCode`](tools/findDeadCode.md) | Top-level declarations unreachable from project roots. | It uses a definition graph and component roots. Text search cannot tell whether code is reachable. |

## Types, instances, and evaluation

| Tool | Returned information | Why enable it instead of relying on terminal commands |
| --- | --- | --- |
| [`lookupInstances`](tools/lookupInstances.md) | Indexed class and family instances whose heads mention several names. | It returns compact instance heads instead of requiring source-file searches and reads. |
| [`resolveInstance`](tools/resolveInstance.md) | The exact class instance GHC selects for a concrete type. | It gives one selected instance, plus needed constraints, instead of manual instance-chain reading. |
| [`getTypeOfExpression`](tools/getTypeOfExpression.md) | The inferred type of an expression. | It answers type questions without source retrieval, temporary files, or manual GHCi setup. |
| [`executeCode`](tools/executeCode.md) | A small runtime result in the project context. | It checks one expression without adding source files or carrying full test output in context. |
| [`createTemporalModule`](tools/createTemporalModule.md) | A temporary module for multi-line debugging code. | It provides a controlled place for helpers instead of patching real source files. |

## Validation and optional tools

| Tool | Availability | Capability | Why enable it instead of relying on terminal commands |
| --- | --- | --- | --- |
| [`reloadHomeModules`](tools/reloadHomeModules.md) | Enabled by default | Compile home modules and refresh Lore's indexes. | It returns structured status, grouped diagnostics, snippets, pagination, and safe import fixes instead of a long build log. |
| [`runTestSuite`](tools/runTestSuite.md) | Disabled by default | Compile first, then run Cabal or Stack test components. | It returns component-level status and focused diagnostics so recovery workflows do not need raw test logs in context. |
| [`notifyKnowledgeReset`](tools/notifyKnowledgeReset.md) | Definition cache enabled | Reset definition duplicate-suppression memory. | It keeps definition caching correct after a client-side context reset. Terminal commands have no matching state. |
| [`feedback`](tools/feedback.md) | Feedback file configured | Save a concise issue report for maintainers. | It captures structured feedback in the project instead of leaving it in chat history. |
| [Custom command tools](tools/custom-command-tools.md) | Project configured | Run a trusted project command through MCP. | The project can expose a stable, documented command surface instead of relying on ad hoc command invention. |

## How agents typically compose the tools

1. Discovery tools (`discoverProject`, `discoverDirectory`, `listExportedSymbols`) provide build shape and API surface before implementation source is retrieved.
2. `searchSymbols` provides candidates when the exact symbol name is unknown.
3. `lookupSymbolInfo` provides type, declaration-header, constructor, export, and location metadata before source retrieval.
4. `getDefinitions` retrieves implementation source only when needed; `None` is the smallest expansion, while `Direct` and `Recursive` add dependency context.
5. Reference, instance, test, and diagnostic tools keep later investigation narrow instead of falling back to broad terminal output.
