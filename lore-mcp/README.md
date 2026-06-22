# lore-mcp

`lore-mcp` is Lore's stdio Model Context Protocol server. It exposes the shared compiler-aware Haskell tools to MCP clients that can launch a local process over standard input and output.

This README covers the server: building, running, configuration mechanics, environment overrides, and direct MCP troubleshooting. Shared tool behavior belongs in the [tool guide](../docs/Tools.md) and the per-tool pages under [`docs/tools/`](../docs/tools/).

For Pi users who want managed binary setup, branch-aware definition memory, recovery summaries, and interactive settings, use [`pi-lore`](https://github.com/catdarick/lore/blob/main/pi-lore/README.md).

## Shared tools

`lore-mcp` exposes Lore's shared tools for project discovery, module APIs, symbol lookup, source definition retrieval, references, instances, type inference, expression evaluation, diagnostics, tests, dead-code analysis, and project-defined command tools.

The [tool guide](../docs/Tools.md) is the canonical index. It explains why each tool is useful for reducing context pressure and links to exact input/output examples.

Most built-in tools are enabled by default. `notifyKnowledgeReset` is disabled by default; raw `lore-mcp` users should enable it only when their client summarizes or compacts chats and the agent is instructed to call it after that reset. `feedback` and project-defined command tools appear only when their backing features are configured.

## Requirements

- GHC and Cabal for building `lore-mcp`.
- A target project using Cabal or Stack.
- The target project's build tool and compiler available in the server environment.
- `hpack` when the target project uses `package.yaml` and its generated Cabal file needs to be refreshed.

Lore links against the GHC API, so the server binary must be built with the same full GHC version as the inspected project. The root [GHC compatibility](../README.md#ghc-compatibility) section is the canonical explanation.

## Build with Cabal

From the repository root:

```bash
cabal build exe:lore-mcp
cabal list-bin exe:lore-mcp
```

To choose the exact compiler explicitly:

```bash
cabal build exe:lore-mcp -w ghc-9.6.5
cabal list-bin exe:lore-mcp -w ghc-9.6.5
```

Replace `9.6.5` with the full compiler version used by the target project.

Run the package tests with:

```bash
cabal test lore-mcp-test
```

For an optimized server binary:

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

Check the Lore, GHC, and target identity embedded in a binary with:

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

## Configuration

For a practical first `lore.yaml`, start with the root [quick start](../README.md#quick-start-for-a-target-project). This section documents the exact configuration mechanics for direct MCP use.

Configuration is optional. Without `lore.yaml`, Lore uses defaults and treats the current working directory as the project root.

Place `lore.yaml` in the directory where `lore-mcp` starts. When `LORE_PROJECT_ROOT` is set, Lore instead loads `<LORE_PROJECT_ROOT>/lore.yaml`.

Configuration precedence is:

```text
built-in defaults < lore.yaml < environment variables
```

`session` and `mcp` settings are read at process startup. Restart the server after changing them. Tool-owned settings such as dead-code roots and symbol-search synonyms are loaded by the tools that use them; their semantics are documented with those tools.

### Minimal `lore.yaml`

```yaml
session:
  project-root: .
  ghc-work-dir: .lore-work
```

### Expanded example

```yaml
session:
  project-root: .
  ghc-work-dir: .lore-work
  parallel-workers-limit: auto
  log-level: info
  # Keep test-tool output focused. These are good defaults for Hspec.
  default-test-args:
    - --format=failed-examples
    - --no-color

# Tool-owned settings. See the linked tool docs before changing semantics.
dead-code:
  alive-modules:
    - Blog.Public
    - Blog.Plugin.*
  alive-symbols:
    - runBlog

symbol-search:
  synonym-groups:
    - [article, post]
    - [author, writer]

mcp:
  enable-definition-knowledge-cache: true
  feedback-file: .lore-work/mcp-feedback.md
  tools:
    # Raw lore-mcp users only: enable this when the client summarizes or
    # compacts chats and the agent is instructed to call it after that reset.
    notifyKnowledgeReset: true
    executeCode: false
```

### Session settings

| Setting | Default | Description |
| --- | --- | --- |
| `session.project-root` | `.` | Haskell project root. In YAML, a relative path is resolved from the directory containing `lore.yaml`. |
| `session.ghc-work-dir` | `.lore-work` | Lore working directory, resolved relative to the final project root. |
| `session.custom-prelude` | unset | Module loaded as a custom interpreter prelude. |
| `session.parallel-workers-limit` | `auto` | Number of workers used for parallel module loading, or `auto` to use the processor count. |
| `session.log-level` | normal logging disabled | One of `debug`, `info`, `warning`, or `error`. |
| `session.default-test-args` | `[]` | Arguments appended to built-in test-suite runs. Use this to reduce noisy framework output; a YAML list preserves arguments exactly. |

Project provider detection uses this order:

1. `stack.yaml` -> Stack
2. `cabal.project` -> Cabal
3. `package.yaml` -> Cabal
4. exactly one root-level `*.cabal` file -> Cabal

When multiple root Cabal files exist, add a `cabal.project` to define package selection explicitly.

### MCP settings

| Setting | Default | Description |
| --- | --- | --- |
| `mcp.enable-definition-knowledge-cache` | `false` | Suppress unchanged definitions already returned by `getDefinitions`. Recommended for raw `lore-mcp` users; `pi-lore` enables and manages it automatically. |
| `mcp.feedback-file` | unset | Enables the `feedback` tool and writes entries to this project-relative or absolute file. |
| `mcp.tools.<toolName>` | enabled for most built-ins; `notifyKnowledgeReset` disabled by default | Enable or disable an individual built-in or configured custom tool. Raw `lore-mcp` users may enable `notifyKnowledgeReset` for summarized/compacted chat workflows. |
| `mcp.custom-tools` | `[]` | Define additional MCP tools backed by trusted shell commands. |

Disable the built-in test runner explicitly:

```yaml
mcp:
  tools:
    runTestSuite: false
```

Arguments passed by the MCP caller are added after `session.default-test-args`.

### Definition knowledge cache

This section matters for raw `lore-mcp` clients. `pi-lore` enables the definition cache and tracks reset points automatically, so Pi users should not configure `notifyKnowledgeReset` for normal workflows.

For raw `lore-mcp`, `mcp.enable-definition-knowledge-cache: true` is recommended because it prevents repeated unchanged definitions from being sent back to the client during repeated `getDefinitions` calls.

If the raw MCP client never summarizes or compacts chat context, no extra reset tool is needed. If the workflow does summarize or compact chats, choose one reset strategy:

- restart `lore-mcp` after summarization; or
- enable `notifyKnowledgeReset` and instruct the agent to call it only after the chat has been summarized, compacted, or otherwise reset.

`notifyKnowledgeReset` is disabled by default so agents do not clear duplicate-suppression memory accidentally during normal source edits.

### Tool-owned configuration

Detailed behavior for tool-owned settings lives with the corresponding tool docs:

| Setting area | Canonical doc |
| --- | --- |
| Dead-code roots and test/non-test reachability semantics | [`findDeadCode`](../docs/tools/findDeadCode.md) |
| Symbol-search synonym groups | [`searchSymbols`](../docs/tools/searchSymbols.md) |
| Definition duplicate-suppression cache and reset behavior | [`getDefinitions`](../docs/tools/getDefinitions.md), [`notifyKnowledgeReset`](../docs/tools/notifyKnowledgeReset.md) |
| Custom command tools and the `runTestSuite` override convention | [Custom command tools](../docs/tools/custom-command-tools.md) |
| Built-in test runner behavior | [`runTestSuite`](../docs/tools/runTestSuite.md) |
| Feedback file behavior | [`feedback`](../docs/tools/feedback.md) |

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

The YAML list form avoids shell parsing and is usually safer.

### Path behavior

- With no `LORE_PROJECT_ROOT`, Lore loads `./lore.yaml` from the startup working directory.
- With `LORE_PROJECT_ROOT`, Lore loads `<LORE_PROJECT_ROOT>/lore.yaml` and uses that value as the project-root environment override.
- `session.project-root` in YAML is relative to the directory containing `lore.yaml`.
- `session.ghc-work-dir` and a relative `mcp.feedback-file` are resolved from the final project root.
- Lore does not load a second configuration file after `session.project-root` redirects the project.

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

Rebuild `lore-mcp` with the exact reported compiler version when they differ.

### The server cannot detect a build provider

Start it from the project root, or set `LORE_PROJECT_ROOT`. Ensure the root contains `stack.yaml`, `cabal.project`, `package.yaml`, or exactly one `*.cabal` file. A directory with multiple root Cabal files needs a `cabal.project`.

### Changes are not reflected in symbol or interpreter tools

Run `reloadHomeModules`. It refreshes loaded modules and indexes, and it clears values previously introduced into the interpreter context.

### `runTestSuite` is missing

It may have been disabled in configuration. Remove a `mcp.tools.runTestSuite: false` override, or set:

```bash
export LORE_MCP_TOOL_ENABLED_RUN_TEST_SUITE=true
```

### Definitions are unexpectedly omitted

Definition knowledge caching may suppress unchanged definitions that were already returned to the client. For `pi-lore`, this is managed automatically. For raw `lore-mcp`, restart the server after chat summarization, or enable `notifyKnowledgeReset` and instruct the agent to call it only after the chat has been summarized, compacted, or otherwise reset.
