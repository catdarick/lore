# pi-lore

Use Lore's GHC-aware Haskell development tools directly from [Pi](https://github.com/badlogic/pi-mono).

`pi-lore` starts a compatible `lore-mcp` server for the current project, exposes its tools to the model, and adds Pi-specific context management for long debugging sessions.

## What it provides

`pi-lore` gives Pi agents access to Lore's compiler-aware Haskell tools. The [complete tool guide](../docs/Tools.md) explains what the agent can do with each tool and why those results are more reliable than plain text search or raw build logs for Haskell-specific questions.

The main value is smaller, more precise context:

- **Focused source retrieval:** The agent can request `publishArticle` alone, or add one or two dependency layers. Lore returns declarations, not whole files.
- **Resolved navigation:** The agent can find references and instances through GHC names, not text matches.
- **Structured validation:** Compilation and test results have machine-readable status, so Pi can track repair progress.
- **Definition memory:** Lore omits unchanged definitions that the current branch already knows.
- **Branch-aware restoration:** Pi forks restore the definition knowledge recorded for that branch.
- **Compact repair sessions:** Failed compilation and test work stays in a recovery section. After validation succeeds, Pi replaces that section with a concise summary.

Plain command output remains useful for project-specific tasks. `pi-lore` adds the compiler-aware layer that lets an agent use fewer tokens and make fewer guesses.

## Requirements

- Pi with pi-package support.
- Node.js 24 or newer.
- A Cabal or Stack project whose compiler can be detected locally.

Automatic binary downloads currently require Linux x86-64 with GNU libc and a GHC version listed in the package's `binaries.json`. On other platforms, or when the exact GHC version is not listed, a source-built `lore-mcp` can be configured when that platform and compiler are supported by Lore.

Lore uses the GHC API, so the server must be built with the **exact** GHC version used by the project. A binary built with GHC 9.6.5 cannot be used with a project on GHC 9.6.7.

## Install

Install the published package for the current user:

```bash
pi install npm:pi-lore
```

Or install it only for the current project:

```bash
pi install npm:pi-lore -l
```

Then start Pi from the root of a Haskell project. On first use, `pi-lore` will either start an already cached server or open a setup menu.

## Pi commands

| Command | Purpose |
| --- | --- |
| `/lore-settings` | Open the project settings menu. |
| `/lore-settings show` | Show the effective project-facing settings. |
| `/lore-status` | Show startup mode, command, GHC version, and server state. |
| `/lore-restart` | Restart the server, or reopen setup if it is not configured. |
| `/lore-stats` | Show estimated tool-context and recovery statistics. |
| `/lore-recovery-abandon` | Stop the active recovery and retain the original conversation entries. |

Changes that affect server startup or tool registration should be followed by `/reload` or a Pi restart.

## Configuration

There are two configuration files with different responsibilities:

- **`.pi/lore.config.json`** configures the Pi extension: server startup, timeouts, proxied Lore tools, and recovery behavior.
- **`lore.yaml`** configures the Haskell session and `lore-mcp`: project roots, compiler loading, dead-code roots, symbol-search synonyms, MCP tools, and custom command tools.

The interactive `/lore-settings` menu writes `.pi/lore.config.json`. See the [`lore-mcp` configuration guide](https://github.com/catdarick/lore/blob/main/lore-mcp/README.md#configuration) for `lore.yaml`.

### Common project settings

A typical `.pi/lore.config.json` is small:

```json
{
  "enabled": true,
  "tools": {
    "disabled": ["executeCode"]
  },
  "recovery": {
    "compilation": true,
    "tests": true
  }
}
```

| Setting | Default | Description |
| --- | --- | --- |
| `enabled` | `true` | Enables Lore for this project. |
| `command` | managed automatically | Explicit command name or executable path for `lore-mcp`. Setting it disables managed download selection. |
| `args` | `[]` | Arguments passed to the configured command. |
| `env` | see below | Environment variables added to the server process. |
| `cwd` | project directory | Working directory used to start the server. Relative paths are resolved from the Pi project directory. |
| `tools.disabled` | `[]` | Public Lore tools that should not be active in Pi. |
| `recovery.compilation` | `true` | Starts recovery after a failed `reloadHomeModules` result. |
| `recovery.tests` | `true` | Starts or extends recovery after a failed `runTestSuite` result. |

By default, the extension starts the server with definition caching enabled and the public `notifyKnowledgeReset` tool disabled:

```json
{
  "env": {
    "LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE": "true",
    "LORE_MCP_TOOL_ENABLED_NOTIFY_KNOWLEDGE_RESET": "false"
  }
}
```

Values from `.pi/lore.config.json` are merged with these defaults. Host-level Pi configuration, when supplied by the host, takes precedence over the project file. Project configuration is not loaded until the project is trusted.

### Advanced settings

| Setting | Default | Description |
| --- | --- | --- |
| `startupTimeoutMs` | `30000` | Timeout for project probing and server startup operations. |
| `defaultToolTimeoutMs` | `1000000` | Default timeout for a proxied MCP tool call. |
| `toolTimeoutMs` | `reloadHomeModules: 300000`, `runTestSuite: 900000` | Per-tool timeout overrides. |
| `summaryTimeoutMs` | `1000000` | Timeout for generating a completed recovery summary. |
| `maxInlineDiffBytes` | `50000` | Maximum recovery diff size included directly in summarization input. Larger diffs are stored in the state directory. |
| `allowToolOverride` | `false` | Allows a Lore tool to replace a Pi tool with the same name. Keep this disabled unless the collision is intentional. |
| `stateDir` | `.pi/extensions/lore/state` | Persistent branch, knowledge-cache, and recovery state directory. |

For non-interactive changes to tool and recovery settings, use a JSON patch:

```text
/lore-settings set {"recovery":{"tests":false}}
```

Other advanced values should be edited directly in `.pi/lore.config.json`.

## Managed server selection

When `command` is not configured, provider detection uses this order:

1. `stack.yaml` → Stack
2. `cabal.project` → Cabal
3. `package.yaml` → Cabal
4. exactly one root-level `*.cabal` file → Cabal

The GHC version is read with one of these commands:

```bash
stack exec -- ghc --numeric-version
cabal exec --write-ghc-environment-files=never -- ghc --numeric-version
```

Downloaded binaries are shared by all projects and stored under:

- `$XDG_CACHE_HOME/pi-lore`, when `XDG_CACHE_HOME` is set;
- otherwise `~/.cache/pi-lore`.

There is no nearest-version fallback. When the project changes compiler or resolver, reload Pi so the extension can select the corresponding server.

## Use a custom server build

A custom command is useful for local Lore development, unsupported platforms, or a GHC version without a published binary.

Open `/lore-settings`, choose **Set command to run Lore**, and enter either a command on `PATH` or an executable path. Before saving it, `pi-lore` verifies that the binary reports the expected Lore version, exact GHC version, and target platform.

You can also configure it directly:

```json
{
  "command": "/home/me/src/lore/dist/lore-mcp",
  "args": []
}
```

### Build `lore-mcp` with Cabal

Install the same GHC version used by the target project, then run from the Lore repository root:

```bash
git clone https://github.com/catdarick/lore.git
cd lore

cabal build exe:lore-mcp -w ghc-9.6.5
cabal list-bin exe:lore-mcp -w ghc-9.6.5
```

Replace `9.6.5` with the project's exact compiler version. Use the path printed by `cabal list-bin` in `/lore-settings`.

To verify the build identity:

```bash
"$(cabal list-bin exe:lore-mcp -w ghc-9.6.5)" --version-json
```

## Recovery behavior

Recovery is started by a failed structured result from `reloadHomeModules` or `runTestSuite`. The extension records a project baseline, keeps the repair work as a distinct section, and tracks which validation steps still need to pass.

After the required compilation and test checks succeed, `pi-lore` captures the resulting diff and asks Pi to summarize the failed approaches, useful findings, and applied fixes. Future model context uses that summary instead of the full repair transcript. This is especially helpful for long-running sessions to avoid context bloat.

Disable compilation and test recovery independently under `/lore-settings` → **Recovery**. Use `/lore-recovery-abandon` when you need to stop recovery without replacing the original entries.

## Troubleshooting

### Lore cannot detect the project

Start Pi at the project root and ensure one of `stack.yaml`, `cabal.project`, `package.yaml`, or a single root `*.cabal` file exists. A directory with multiple root Cabal files needs a `cabal.project`.

### No binary is available for this GHC version

Choose **Build instructions** in the setup menu, build `lore-mcp` with the exact project compiler, and configure the resulting executable as a custom command.

### The configured binary is rejected

Run:

```bash
/path/to/lore-mcp --version-json
```

Its `loreVersion`, `ghcVersion`, and `target` must match the extension release, project compiler, and current platform.

### Settings appear unchanged

Run `/reload` or restart Pi. Startup settings and MCP tool registration are established when the server starts.

## Developing the Pi package

`pi-lore` is a TypeScript Pi package and does not use Cabal itself. From `pi-lore/`:

```bash
npm test
npm run validate:package
```

The Haskell server it launches is built separately with Cabal as described above.
