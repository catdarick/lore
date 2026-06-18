# lore-mcp

`lore-mcp` is a Model Context Protocol server for understanding and working with Haskell projects through the GHC API.

It gives coding agents compiler-aware tools for project discovery, symbol navigation, source retrieval, typeclass instances, expression typechecking and evaluation, compilation diagnostics, test execution, and dead-code analysis. The server communicates over standard input and output, so it can be used by any MCP client that supports a local stdio command.

For the integrated Pi experience with automatic binary setup, branch-aware definition memory, and recovery summaries, see [`pi-lore`](https://github.com/catdarick/lore/blob/main/pi-lore/README.md).

## Why use Lore instead of text search alone?

Lore loads the project with its real package environment and compiler options. This lets the agent answer questions that require GHC's view of the code, such as:

- Which declaration does this symbol name resolve to?
- What is the inferred type of this expression in the current project?
- Which typeclass instance will GHC select for a concrete type?
- Where is a function or type used?
- What are the compilation diagnostics?
- Which top-level declarations are unreachable from configured entry points?

Text search is still useful for strings and arbitrary source patterns. Lore complements it with resolved names, types, module interfaces, and project-aware compilation.

## Requirements

- GHC and Cabal for building `lore-mcp`.
- A target project using Cabal or Stack.
- The build tool and project compiler available from the environment in which the server starts.
- `hpack` when the target project uses `package.yaml` and its generated Cabal file needs to be refreshed.

`lore-mcp` links against the GHC API. Build it with the **same full GHC version** as the project it will inspect. For example, a GHC 9.6.5 server is not compatible with a GHC 9.6.7 project.

## Build with Cabal

Clone the repository and build the executable from the repository root:

```bash
git clone https://github.com/catdarick/lore.git
cd lore

cabal build exe:lore-mcp
cabal list-bin exe:lore-mcp
```

To choose the exact compiler explicitly:

```bash
cabal build exe:lore-mcp -w ghc-9.6.5
cabal list-bin exe:lore-mcp -w ghc-9.6.5
```

Replace `9.6.5` with the full version used by the target project.

Run the package tests with:

```bash
cabal test lore-mcp-test
```

For an optimized build:

```bash
cabal build exe:lore-mcp --enable-optimization=2
```

## Run

Start the executable with the target project as its working directory:

```bash
cd /path/to/haskell-project
/path/to/lore-mcp
```

With no arguments, the process starts an MCP server over stdio. A typical MCP client configuration looks like this:

```json
{
  "mcpServers": {
    "lore": {
      "command": "/absolute/path/to/lore-mcp",
      "args": [],
      "cwd": "/absolute/path/to/haskell-project"
    }
  }
}
```

The exact configuration shape depends on the MCP client.

Check which Lore, GHC, and target versions are embedded in a binary with:

```bash
lore-mcp --version-json
```

Example output:

```json
{
  "loreVersion": "0.1.0.0",
  "ghcVersion": "9.6.5",
  "target": "linux-x64-gnu"
}
```

## Tools

`lore-mcp` exposes compiler-aware tools for agents. The [tool guide](../docs/Tools.md) explains what the agent can learn and why these results are more useful than plain text output for Haskell work.

A typical agent workflow is narrow:

1. Search with [`searchSymbols`](../docs/tools/searchSymbols.md) only when the exact name is unknown.
2. Inspect interfaces with [`lookupSymbolInfo`](../docs/tools/lookupSymbolInfo.md) before reading source.
3. Fetch implementation source with [`getDefinitions`](../docs/tools/getDefinitions.md) only for the symbols involved.
4. Validate edits with [`reloadHomeModules`](../docs/tools/reloadHomeModules.md) and, when enabled, [`runTestSuite`](../docs/tools/runTestSuite.md).

Plain command output is still useful, but it is unstructured text. Lore returns bounded Haskell facts: resolved symbols, source definitions, references, types, instances, diagnostics, and structured validation status.

All built-in tools except `runTestSuite` are enabled by default. `notifyKnowledgeReset`, `feedback`, and project-defined command tools appear only when their features are configured.

## Configuration

Configuration is optional. Without `lore.yaml`, Lore uses defaults and treats the current working directory as the project root.

Place `lore.yaml` in the directory where `lore-mcp` starts. When `LORE_PROJECT_ROOT` is set, Lore instead loads `<LORE_PROJECT_ROOT>/lore.yaml`.

Configuration precedence is:

```text
built-in defaults < lore.yaml < environment variables
```

`session` and `mcp` settings are read at process startup. Restart the server after changing them. `dead-code` and `symbol-search` settings are reloaded by the relevant operations and can take effect without restarting.

### Example `lore.yaml`

```yaml
session:
  project-root: .
  ghc-work-dir: .lore-work
  parallel-workers-limit: auto
  log-level: info

# Roots that findDeadCode should always treat as reachable.
dead-code:
  alive-modules:
    - Blog.Public
    - Blog.Plugin.*
  alive-symbols:
    - runBlog

# Project vocabulary added to Lore's built-in search synonyms.
symbol-search:
  synonym-groups:
    - [article, post]
    - [author, writer]

mcp:
  enable-definition-knowledge-cache: false
  feedback-file: .lore-work/mcp-feedback.md
  tools:
    runTestSuite: true
    executeCode: false
```

### Session settings

| Setting | Default | Description |
| --- | --- | --- |
| `session.project-root` | `.` | Haskell project root. In YAML, a relative path is resolved from the directory containing `lore.yaml`. |
| `session.ghc-work-dir` | `.lore-work` | Lore working directory, resolved relative to the project root. |
| `session.custom-prelude` | unset | Module loaded as a custom interpreter prelude. |
| `session.parallel-workers-limit` | `auto` | Number of workers used for parallel module loading, or `auto` to use the processor count. |
| `session.log-level` | normal logging disabled | One of `debug`, `info`, `warning`, or `error`. |
| `session.default-test-args` | `[]` | Arguments appended to built-in test-suite runs. Use a YAML list so arguments are preserved exactly. |

Project provider detection uses this order:

1. `stack.yaml` → Stack
2. `cabal.project` → Cabal
3. `package.yaml` → Cabal
4. exactly one root-level `*.cabal` file → Cabal

When multiple root Cabal files exist, add a `cabal.project` to define package selection explicitly.

### MCP settings

| Setting | Default | Description |
| --- | --- | --- |
| `mcp.enable-definition-knowledge-cache` | `false` | Suppress unchanged definitions already returned by `getDefinitions`. Intended for clients that can synchronize cache state, such as `pi-lore`. |
| `mcp.feedback-file` | unset | Enables the `feedback` tool and writes entries to this project-relative or absolute file. |
| `mcp.tools.<toolName>` | enabled, except `runTestSuite` | Enable or disable an individual built-in or configured custom tool. |
| `mcp.custom-tools` | `[]` | Define additional MCP tools backed by shell commands. |

### Enable the built-in test runner

`runTestSuite` is disabled by default because it executes project tests and can be expensive. Enable it explicitly:

```yaml
mcp:
  tools:
    runTestSuite: true
```

Default arguments can be provided for every built-in run:

```yaml
session:
  default-test-args:
    - --test-show-details=direct
```

Arguments passed by the MCP caller are added to the configured defaults.

### Custom command tools

A custom tool runs a shell command from the project environment. Placeholders use `@{argumentName}` syntax:

```yaml
mcp:
  custom-tools:
    - name: lintPackage
      description: Run the project linter for a package
      command: ./scripts/lint @{package}
      args:
        - name: package
          description: Cabal package name
          nullable: false
          quote-mode: single
  tools:
    lintPackage: true
```

Each argument can be written as a short required-string form:

```yaml
args:
  - package
  - target
```

Or as an object with these options:

| Field | Default | Description |
| --- | --- | --- |
| `name` | required | Placeholder name used by `@{name}`. |
| `description` | unset | Description included in the generated MCP schema. |
| `nullable` | `false` | Allows the caller to pass `null`, which is substituted as an empty string. |
| `escape-quotes` | `false` | Escapes double quotes before insertion. |
| `quote-mode` | `single` | `single`, `double`, or `none`. `single` is the safest choice for a normal argument. |

Every placeholder must have a declared argument. Custom names cannot duplicate built-in tools, with one exception: a custom tool named `runTestSuite` replaces the built-in runner.

To use a project-specific test command while preserving the structured success/failure result expected by clients:

```yaml
mcp:
  custom-tools:
    - name: runTestSuite
      description: Run the project's test script
      command: ./scripts/test @{testArgs}
      args:
        - name: testArgs
          description: Optional raw arguments passed to the script
          nullable: true
          quote-mode: none
  tools:
    runTestSuite: true
```

Exit code `0` is reported as success; a non-zero exit code is reported as failure. The result also contains stdout and stderr.

> **Security:** custom tools, `executeCode`, and `runTestSuite` execute code or commands from the project environment. Enable them only for projects and configuration you trust. Use `quote-mode: none` only when raw shell fragments are intentional.

### Environment variables

Environment values override matching YAML settings.

| YAML setting | Environment variable |
| --- | --- |
| `session.project-root` | `LORE_PROJECT_ROOT` |
| `session.ghc-work-dir` | `LORE_GHC_WORK_DIR` |
| `session.custom-prelude` | `LORE_CUSTOM_PRELUDE` |
| `session.parallel-workers-limit` | `LORE_PARALLEL_WORKERS_LIMIT` |
| `session.log-level` | `LORE_LOG_LEVEL` |
| `session.default-test-args` | `LORE_DEFAULT_TEST_ARGS` |
| `mcp.enable-definition-knowledge-cache` | `LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE` |
| `mcp.feedback-file` | `LORE_MCP_FEEDBACK_FILE` |
| `mcp.tools.<toolName>` | `LORE_MCP_TOOL_ENABLED_<TOOL_NAME>` |

Tool names are converted to uppercase snake case. For example:

```text
runTestSuite -> LORE_MCP_TOOL_ENABLED_RUN_TEST_SUITE
notifyKnowledgeReset -> LORE_MCP_TOOL_ENABLED_NOTIFY_KNOWLEDGE_RESET
```

Boolean environment values accept `true`, `false`, `1`, `0`, `yes`, `no`, `on`, and `off`, case-insensitively.

`LORE_DEFAULT_TEST_ARGS` is a single shell-style string, for example:

```bash
export LORE_DEFAULT_TEST_ARGS='--match "specific example"'
```

Prefer the YAML list form when possible because it avoids shell parsing.

### Path behavior

- With no `LORE_PROJECT_ROOT`, Lore loads `./lore.yaml` from the startup working directory.
- With `LORE_PROJECT_ROOT`, Lore loads `<LORE_PROJECT_ROOT>/lore.yaml` and uses that value as the project-root environment override.
- `session.project-root` in YAML is relative to the directory containing `lore.yaml`.
- `session.ghc-work-dir` and a relative `mcp.feedback-file` are resolved from the final project root.
- Lore does not load a second configuration file after `session.project-root` redirects the project.

## Dead-code roots

`findDeadCode` evaluates reachability separately for non-test components and test components.

A non-test executable `main` is a root for the non-test reachability graph. A test-suite `main` is a root only within that test component. References from tests do not make library declarations alive in the non-test graph, so a library symbol used only by tests is still reported as dead. This is intentional: test usage should not hide library code that is otherwise unreachable from non-test components.

Add public library modules and other externally called entry points explicitly when they are not reachable from a non-test executable:

```yaml
dead-code:
  alive-modules:
    - MyLibrary
    - MyLibrary.Public
    - MyLibrary.Plugin.*
  alive-symbols:
    - MyLibrary.startServer
```

Configured `alive-modules` and `alive-symbols` are added as roots to the non-test reachability graph. `alive-modules` entries use the same case-sensitive `*` module-pattern syntax as `searchSymbols.modulePatterns`; exact module names still work. They are useful for public APIs, plugin entry points, GHCi helpers, framework callbacks, and other code invoked outside the statically visible call graph.

## Symbol-search synonyms

Lore includes general programming synonyms and abbreviations. Add project-specific vocabulary with groups of equivalent terms:

```yaml
symbol-search:
  synonym-groups:
    - [account, profile]
    - [author, writer]
    - ["pull request", PR]
```

Project groups are merged with Lore's built-in vocabulary; they do not replace it. Each group must contain at least two distinct normalized terms, and every term in a group is a direct, bidirectional synonym of the other terms in that group. Overlapping groups do not create transitive matches.

Multi-word terms should be quoted as one YAML string. They match only when their normalized tokens occur contiguously and in order within one indexed name, module, argument type, or result type.

## Troubleshooting

### The server was built with the wrong GHC

Compare the project compiler with the server identity:

```bash
cabal exec --write-ghc-environment-files=never -- ghc --numeric-version
/path/to/lore-mcp --version-json
```

For Stack projects, use:

```bash
stack exec -- ghc --numeric-version
```

Rebuild `lore-mcp` with the exact reported version when they differ.

### The server cannot detect a build provider

Start it from the project root, or set `LORE_PROJECT_ROOT`. Ensure the root contains `stack.yaml`, `cabal.project`, `package.yaml`, or exactly one `*.cabal` file.

### Changes are not reflected in symbol or interpreter tools

The agent or MCP client should call `reloadHomeModules`. It refreshes the loaded modules and indexes, but also clears values previously introduced into the interpreter context.

### `runTestSuite` is missing

It is disabled by default. Enable it under `mcp.tools`, or set:

```bash
export LORE_MCP_TOOL_ENABLED_RUN_TEST_SUITE=true
```

### Definitions are unexpectedly omitted

When definition knowledge caching is enabled, unchanged definitions already returned to the client may be suppressed. A compatible client should synchronize or reset this state. Disable the feature for a plain MCP integration that does not manage it.
