# pi-lore

`pi-lore` is the Pi package for Lore's GHC-aware Haskell development tools. It starts a compatible `lore-mcp` server for the current project, exposes the shared Lore tools to the model, and adds Pi-specific context management for long debugging sessions.

The shared tool behavior is documented in the [tool guide](../docs/Tools.md). This README covers only the Pi integration layer: installation, managed server setup, Pi commands, Pi configuration, recovery behavior, and troubleshooting.

## What it provides

- **Managed server setup:** selects, downloads, validates, or prompts for a compatible `lore-mcp` binary.
- **Definition memory:** lets unchanged definitions already returned on the current branch be omitted from later `getDefinitions` responses.
- **Branch-aware restoration:** restores definition-memory state when Pi forks or resumes a branch.
- **Compact repair sessions:** keeps failed compile/test repair work in a recovery section, then replaces it with a summary after validation succeeds.
- **Interactive controls:** adds Pi commands for settings, status, restart, recovery, and usage statistics.

Plain command output is still useful for project-specific tasks. `pi-lore` adds the compiler-aware and branch-aware context-management layer that keeps large-codebase sessions smaller.

## Requirements

- Pi with package support.
- Node.js 24 or newer.
- A Cabal or Stack project whose compiler can be detected locally.
- A `lore-mcp` binary built for the exact full GHC version used by the project.

Automatic downloads currently require Linux x86-64 with GNU libc and a GHC version listed in the package's `binaries.json`. Other platforms, or unlisted GHC versions, need a manually configured `lore-mcp` binary when supported by Lore.

The root [GHC compatibility](../README.md#ghc-compatibility) section explains why exact compiler matching is required.

## Install

Install the published package for the current user:

```bash
pi install npm:pi-lore
```

Or install it only for the current project:

```bash
pi install npm:pi-lore -l
```

Start Pi from the root of a Haskell project. On first use, `pi-lore` either starts an already cached server or opens the setup menu.

## Pi commands

| Command | Purpose |
| --- | --- |
| `/lore-settings` | Open the project settings menu. |
| `/lore-settings show` | Show the effective project-facing settings. |
| `/lore-status` | Show startup mode, command, GHC version, and server state. |
| `/lore-restart` | Restart the server, or reopen setup if it is not configured. |
| `/lore-stats` | Show estimated tool-context and recovery statistics. |
| `/lore-recovery-abandon` | Stop the active recovery and retain the original conversation entries. |

Changes that affect server startup or MCP tool registration should be followed by `/reload` or a Pi restart.

## Configuration

There are two configuration files with different responsibilities:

- **`.pi/lore.config.json`** configures the Pi extension: server startup, timeouts, proxied Lore tools, recovery behavior, and Pi-side state paths.
- **`lore.yaml`** configures the Haskell session and `lore-mcp`: project root, GHC loading, MCP tool enablement, and tool-owned settings.

The interactive `/lore-settings` menu writes `.pi/lore.config.json`. Use the root [quick start](../README.md#quick-start-for-a-target-project) for a practical first `lore.yaml`, and see the [`lore-mcp` configuration guide](https://github.com/catdarick/lore/blob/main/lore-mcp/README.md#configuration) for configuration mechanics.

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

By default, the extension starts the server with definition caching enabled and hides the public reset tool from the model. `pi-lore` tracks summarization and branch changes itself, so agents do not need to call `notifyKnowledgeReset` in normal Pi workflows:

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

When `command` is not configured, `pi-lore` detects the project provider, reads the project GHC version, and selects a server binary with the exact same GHC version when one is available.

Downloaded binaries are shared by all projects and stored under:

- `$XDG_CACHE_HOME/pi-lore`, when `XDG_CACHE_HOME` is set;
- otherwise `~/.cache/pi-lore`.

There is no nearest-version fallback. When the project changes compiler or resolver, reload Pi so the extension can select the corresponding server.

Provider detection, project-root behavior, and the commands used to read the compiler version are documented in the [`lore-mcp` configuration guide](https://github.com/catdarick/lore/blob/main/lore-mcp/README.md#configuration).

## Custom server command

A custom command is useful for local Lore development, unsupported platforms, or a GHC version without a published binary.

Open `/lore-settings`, choose **Set command to run Lore**, and enter either a command on `PATH` or an executable path. Before saving it, `pi-lore` verifies the binary identity against the expected Lore version, exact GHC version, and target platform.

The same setting can be configured directly:

```json
{
  "command": "/home/me/src/lore/dist/lore-mcp",
  "args": []
}
```

Build instructions for `lore-mcp`, including exact-GHC Cabal commands and `--version-json` verification, are maintained in the [`lore-mcp` README](https://github.com/catdarick/lore/blob/main/lore-mcp/README.md#build-with-cabal).

## Recovery behavior

Recovery is a Pi-side context-management feature. It starts when `reloadHomeModules` or `runTestSuite` returns a failed structured result. The extension records a project baseline, keeps the repair work in a distinct section, and tracks which validation steps still need to pass.

After the required compilation and test checks succeed, `pi-lore` captures the resulting diff and asks Pi to summarize failed approaches, useful findings, and applied fixes. Future model context uses that summary instead of the full repair transcript.

Compilation and test recovery can be disabled independently under `/lore-settings` -> **Recovery**. `/lore-recovery-abandon` stops recovery without replacing the original entries.

Tool-level behavior for `reloadHomeModules` and `runTestSuite` remains documented in their shared tool pages.

## Troubleshooting

### Lore cannot detect the project

Start Pi at the project root. Provider detection and supported manifest order are documented in the [`lore-mcp` configuration guide](https://github.com/catdarick/lore/blob/main/lore-mcp/README.md#session-settings).

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

The Haskell server it launches is built separately with Cabal as described in the [`lore-mcp` README](https://github.com/catdarick/lore/blob/main/lore-mcp/README.md#build-with-cabal).
