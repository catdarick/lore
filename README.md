# lore

`lore` is a powerful monorepo providing programmable project analysis, structural navigation, and Haskell tooling through the Model Context Protocol (MCP). It lets agents read, navigate, evaluate, and test Haskell projects interactively using `GHC` APIs.

The repository is split into four packages:

- **`lore`**: The core GHC session and analysis library.
- **`lore-tools`**: The intermediate tooling and logical layer wrapping `lore`.
- **`lore-mcp`**: The MCP server executable. Adapts the core capabilities into the Model Context Protocol.
- **`lore-tools-cli`**: The CLI executable. Exposes the tools directly via a terminal interface.

*Note: The system has been tested primarily with GHC 9.6.5 and GHC 9.6.7. It is not guaranteed to compile or run correctly on lower or higher versions of GHC.*

## Installation

Build or install the workspace using standard Haskell tooling (`stack` or `cabal`).

```bash
# To compile the packages locally:
stack build
# or
cabal build

# To install the executables globally into your PATH:
stack install
# or
cabal install
```

When installed, two main executables become available on your system: `lore-mcp` and `lore-cli`.
## Usage (CLI Mode)

For direct human interaction from the terminal, use the `lore-cli` executable. It opens an interactive REPL by default, but can also be run non-interactively to execute specific tools—making it ideal for CI/CD pipelines (for example, to automatically check for dead code).

```bash
# Interactive REPL:
lore-cli

# Non-interactive execution in CI (example):
lore-cli find-dead-code

# If running from the project root:
stack run lore-cli
# or
cabal run lore-cli
```

## Usage (MCP Mode)

The primary way for agents and IDEs to use this project is running the `lore-mcp` server. It implements the Model Context Protocol, communicating over `stdio` by default.

```bash
# If installed globally:
lore-mcp

# If running from the project root:
stack run lore-mcp
# or
cabal run lore-mcp
```

Clients that support MCP can connect to this standard input/output stream and automatically discover and execute the tools below.

### Available MCP Tools

The `lore-mcp` server exposes a rich suite of tools for deep codebase analysis and interaction:

*   **`reloadHomeModules`**: Reloads all home modules in the active session, checking for errors and applying safe auto-fixes. Returns diagnostic information and the updated compilation status.
*   **`discoverProject`**: Scans the workspace for Haskell package manifests (`package.yaml` or `.cabal`). Returns a structured tree of available packages and their respective components (libraries, executables, test suites).
*   **`discoverDirectory`**: Recursively scans a directory path up to a specified depth. Returns a structured directory tree.
*   **`listExportedSymbols`**: Queries a specific module for its API surface. Returns a list of all direct and re-exported symbols, with optional filtering by type.
*   **`searchSymbols`**: Performs a fuzzy or semantic search for Haskell symbols (functions, types, classes, etc.) across the project. Returns matching symbols alongside their signatures and defining modules.
*   **`lookupSymbolInfo`**: Retrieves detailed metadata for a given Haskell symbol. Returns its type signature, defining module, and structural metadata.
*   **`getDefinition`**: Retrieves the exact source code block for one or more symbols. Can optionally resolve and return definitions of their dependencies recursively.
*   **`findDeadCode`**: Analyzes project-wide reachability based on configured entry points. Returns a list of potentially unused top-level declarations.
*   **`resolveInstance`**: Resolves a specific typeclass application (e.g., `Render (Maybe Foo)`). Returns the exact instance declaration that GHC selects.
*   **`findReferences`**: Finds usage sites of a given symbol. Returns the file locations and surrounding source code context for each reference.
*   **`lookupInstances`**: Searches for loaded typeclass or family instance declarations mentioning specific names. Returns the matching instance heads.
*   **`getTypeOfExpression`**: Infers the type of an arbitrary Haskell expression within the active project environment. Returns the computed type signature.
*   **`executeCode`**: Evaluates a single-line Haskell expression or IO action. Returns both the `stdout` stream and the final evaluated result.
*   **`createTemporalModule`**: Creates a temporary Haskell file integrated into the current project session. Returns the file path, allowing developers to author multi-line definitions or complex blocks for quick active evaluation.
*   **`runTestSuite`**: Executes the project's test suite, forwarding any provided arguments. Returns the standard test execution output.
*   **`notifyKnowledgeReset`**: Clears the server's internal definition cache (used to suppress duplicate code blocks across multiple `getDefinition` calls).
*   **`feedback`**: Records structured user feedback (such as bug reports or feature requests) to a configured log file.

## Configuration & Environment Variables

`lore-mcp` relies on environment variables for session and tool configuration. These are read at startup.

### General Session Configuration

| Environment Variable | Default | Description |
| :--- | :--- | :--- |
| `LORE_PROJECT_ROOT` | `"."` | Sets the project root directory. |
| `LORE_GHC_WORK_DIR` | `".lore-work"` | Directory for GHC build artifacts. |
| `LORE_CUSTOM_PRELUDE` | (Base Prelude) | Module name to import instead of standard `Prelude`. |
| `LORE_PARALLEL_WORKERS_LIMIT` | `auto` | Number of concurrent workers (`auto` or a positive integer). |
| `LORE_LOG_LEVEL` | (Disabled) | Minimum log level (`debug`, `info`, `warn`, `error`). Output goes to stderr. |
| `LORE_DEFAULT_TEST_ARGS` | (Empty) | Prepends default arguments (e.g., `--arg 1`) to `runTestSuite` calls. |

### Advanced Tool Configuration

*   **`LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE`**: If `true`, `getDefinition` remembers and skips rendering duplicate source definitions in subsequent calls. Also registers `lore_notifyKnowledgeReset` to clear this cache.
*   **`LORE_MCP_FEEDBACK_FILE`**: Providing a path registers the `feedback` tool, appending feedback to that file.
*   **Disable Specific Tools**: Prefix any tool name in upper snake case with `LORE_MCP_TOOL_ENABLED_`. Example: `LORE_MCP_TOOL_ENABLED_EXECUTE_CODE=false`. By default, all tools are enabled except `runTestSuite`.
*   **Enabling `runTestSuite`**: Set `LORE_MCP_TOOL_ENABLED_RUN_TEST_SUITE=true`. Note that this tool dynamically adds the `directory` package to interpreter dependencies. If enabling this causes project loading to fail with a missing package error, you must add `directory` to the `dependencies` in your project's `package.yaml` or `.cabal`, rebuild your project, and reload the modules.

## Project Metadata (`lore.yaml`)

Projects can define a `.lore.yaml` (or `lore.yaml`) at the project root to control indexing, synonym mapping, and dead code detection.

```yaml
dead-code:
  alive-modules:
    - "Main"
    - "Dev"
  alive-symbols:
    - "runApplication"

symbol-search:
  synonym-groups:
    - ["customer", "client"]
    - ["enqueue", "schedule", "submit"]
    - ["credit line", "commitment"]
```

*   **`dead-code`**: Defines explicit "alive roots" for the `findDeadCode` tool. By default, `main` functions in executables and test suites are considered alive. **Note on library code:** If a library symbol is only used within test suites, it will intentionally be reported as dead code. If you are developing a library, you must add its public API modules or entry-point symbols to `alive-modules` or `alive-symbols` to keep them from being marked as dead.
*   **`symbol-search`**: Customizes synonym expansion for the symbol search tool. This helps natural language queries find symbols that use different but equivalent project vocabulary. Lore ships with a built-in synonym base for common programming terms, abbreviations, and phrases (`db`/`database`, `get`/`fetch`/`retrieve`, `pr`/`pull request`, and similar). Project `synonym-groups` are added on top of those defaults so project-specific vocabulary can match local naming conventions.
*   **`symbol-search.synonym-groups`**: Each entry is a group of equivalent terms. A term may be a single word, abbreviation, operator, or phrase. Phrases must be written as one YAML string, for example `"credit line"` or `"pull request"`. Each group must contain at least two distinct normalized terms.

*Note: Edits to `lore.yaml` take effect dynamically without requiring index rebuilds.*
