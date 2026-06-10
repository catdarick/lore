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

## Configuration

`lore-cli` and `lore-mcp` both read startup configuration from `lore.yaml` and environment variables. Precedence is:

```text
code defaults < lore.yaml < environment variables < frontend runtime requirements
```

Frontend runtime requirements are imposed by the executable. For example, interactive CLI sessions require test-suite support, and MCP requires test-suite support when the `runTestSuite` tool is enabled.

Startup settings are read once when the process starts. Changing `session` or `mcp` settings requires restarting the process. Operational project settings under `dead-code` and `symbol-search` are reloaded from disk by the relevant operations, so edits there can take effect without restarting.

### `lore.yaml`

Place `lore.yaml` in the launch directory, or in `LORE_PROJECT_ROOT` when that environment variable is set.

```yaml
session:
  project-root: .
  ghc-work-dir: .lore-work
  custom-prelude: CustomPrelude
  parallel-workers-limit: auto
  log-level: info
  default-test-args:
    - --match
    - some test name

dead-code:
  alive-modules:
    - Main
  alive-symbols:
    - runApplication

symbol-search:
  synonym-groups:
    - [customer, client]
    - [BillPay, Spot]

mcp:
  enable-definition-knowledge-cache: true
  feedback-file: .lore-work/mcp-feedback.md
  tools:
    runTestSuite: true
    notifyKnowledgeReset: false
```

YAML values use native YAML types. Booleans are booleans, `session.default-test-args` is a list of arguments, and `session.parallel-workers-limit` is either `auto` or a positive integer. `mcp.feedback-file: null` or a missing value leaves the `feedback` tool unavailable.

### Environment Equivalents

Environment variables keep their existing string syntax and override matching YAML values.

| YAML | Environment |
| :--- | :--- |
| `session.project-root` | `LORE_PROJECT_ROOT` |
| `session.ghc-work-dir` | `LORE_GHC_WORK_DIR` |
| `session.custom-prelude` | `LORE_CUSTOM_PRELUDE` |
| `session.parallel-workers-limit` | `LORE_PARALLEL_WORKERS_LIMIT` |
| `session.log-level` | `LORE_LOG_LEVEL` |
| `session.default-test-args` | `LORE_DEFAULT_TEST_ARGS` |
| `mcp.enable-definition-knowledge-cache` | `LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE` |
| `mcp.feedback-file` | `LORE_MCP_FEEDBACK_FILE` |
| `mcp.tools.<toolName>` | `LORE_MCP_TOOL_ENABLED_<TOOL_NAME>` |

`LORE_DEFAULT_TEST_ARGS` is parsed as shell-style text because it is one string, for example `--match "some test"`. YAML `session.default-test-args` should use a list and does not use shell parsing.

MCP tool environment suffixes are generated from tool names. For example, `mcp.tools.runTestSuite` maps to `LORE_MCP_TOOL_ENABLED_RUN_TEST_SUITE`. By default, all MCP tools are enabled except `runTestSuite`.

### Path Semantics

`LORE_PROJECT_ROOT` has a bootstrap role. If it is set, Lore loads `<LORE_PROJECT_ROOT>/lore.yaml` and also uses it as the environment override for `session.project-root`. If it is not set, Lore loads `./lore.yaml` from the launch directory, and that file may set `session.project-root` to another directory. Lore does not then load a second config file from the YAML-selected project root.

Relative paths are resolved as follows:

*   The config file path is resolved from the startup working directory or `LORE_PROJECT_ROOT`, then converted to an absolute path before Lore changes directories.
*   `session.project-root` in YAML is relative to the directory containing `lore.yaml`.
*   `session.ghc-work-dir` is relative to the resolved project root.
*   `mcp.feedback-file` is relative to the resolved project root.
*   Absolute paths remain unchanged.

### Project Settings

*   **`dead-code`**: Defines explicit "alive roots" for the `findDeadCode` tool. By default, `main` functions in executables and test suites are considered alive. **Note on library code:** If a library symbol is only used within test suites, it will intentionally be reported as dead code. If you are developing a library, you must add its public API modules or entry-point symbols to `alive-modules` or `alive-symbols` to keep them from being marked as dead.
*   **`symbol-search`**: Customizes synonym expansion for the symbol search tool. This helps natural language queries find symbols that use different but equivalent project vocabulary. Lore ships with a built-in synonym base for common programming terms, abbreviations, and phrases (`db`/`database`, `get`/`fetch`/`retrieve`, `pr`/`pull request`, and similar). Project `synonym-groups` are added on top of those defaults so project-specific vocabulary can match local naming conventions.
*   **`symbol-search.synonym-groups`**: Each entry is a group of equivalent terms. A term may be a single word, abbreviation, operator, or phrase. Phrases must be written as one YAML string, for example `"credit line"` or `"pull request"`. Each group must contain at least two distinct normalized terms.

*Note: Edits to `dead-code` and `symbol-search` in `lore.yaml` take effect dynamically without requiring index rebuilds.*
