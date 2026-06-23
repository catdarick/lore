# pi-lore

`pi-lore` gives Pi agents GHC-aware tools for Haskell codebases. In large projects, ordinary code discovery often means reading whole files, chasing imports by hand, and filling the model context with definitions that are unrelated or already known. Lore changes that interaction: the agent can ask about symbols directly.

Instead of treating the repository as plain text, `pi-lore` starts a compatible `lore-mcp` server and exposes tools for symbol search, definition lookup, reference search, exported-symbol browsing, instance lookup, expression typing, compilation, and test execution. The result is less redundant context and more precise edits, especially when a task spans many modules.

`pi-lore` also adds Pi-specific context management. It remembers definitions returned on the current branch, restores that memory across Pi forks and resumes, and compresses failed compile/test repair sessions after validation succeeds.

The shared Lore tool behavior is documented in the [tool guide](https://github.com/catdarick/lore/blob/main/docs/Tools.md). This README covers the Pi package: installation, managed server setup, commands, configuration, recovery, and troubleshooting.

## What You Get

- **Symbol-level discovery:** search symbols, inspect definitions, find references, list exports, resolve instances, and type expressions without forcing broad file reads.
- **Compiler-backed validation:** run `reloadHomeModules`, `executeCode`, and `runTestSuite` through the same Lore session the agent uses for analysis.
- **Definition memory:** omit unchanged definitions already returned in the current session.
- **Recovery summaries:** when a compile or test run fails, `pi-lore` tracks the "repair" actions and summarizes them after validation succeeds, so future context uses the summary instead of the full repair transcript.

## Requirements

- Pi with package support.
- Node.js 24 or newer.
- A Cabal or Stack project whose compiler can be detected locally.

`pi-lore` needs a `lore-mcp` server built with the exact full GHC version used by the target project. For Linux x86-64 with GNU libc, some matching servers are published on the [Lore releases page](https://github.com/catdarick/lore/releases). When a release asset matches the project compiler and current platform, `pi-lore` downloads it automatically.

If no matching prebuilt server is available, clone the Lore repository and build `lore-mcp` locally, then configure that executable in `/lore-settings`. Build instructions are in the [`lore-mcp` README](https://github.com/catdarick/lore/blob/main/lore-mcp/README.md#build-with-cabal).

The root [GHC compatibility](https://github.com/catdarick/lore/blob/main/README.md#ghc-compatibility) section explains why exact compiler matching is required.

## Install

Install for the current user:

```bash
pi install npm:pi-lore
```

Or install only for the current project:

```bash
pi install npm:pi-lore -l
```

Start Pi from the root of a Haskell project. On first use, `pi-lore` starts an already cached server or opens the setup menu.

## Commands

| Command | Purpose |
| --- | --- |
| `/lore-settings` | Open the project settings menu. |
| `/lore-settings show` | Show effective settings. |
| `/lore-status` | Show startup mode, command, GHC version, and server state. |
| `/lore-restart` | Restart the server, or reopen setup if it is not configured. |
| `/lore-stats` | Show estimated tool-context and recovery statistics. |
| `/lore-recovery-abandon` | Stop the active recovery and keep the original conversation entries. |

After changing startup or tool-registration settings, run `/reload` or restart Pi.

## Configuration

`pi-lore` has one Pi-side config file and one Lore server config file:

- **`.pi/lore.config.json`** configures the Pi extension: server startup, timeouts, proxied Lore tools, recovery behavior, and Pi-side state paths.
- **`lore.yaml`** configures the Haskell session and `lore-mcp`: project root, GHC loading, MCP tool enablement, and tool-owned settings.

The `/lore-settings` menu writes `.pi/lore.config.json`. Use the root [quick start](https://github.com/catdarick/lore/blob/main/README.md#quick-start-for-a-target-project) for a first `lore.yaml`, and the [`lore-mcp` configuration guide](https://github.com/catdarick/lore/blob/main/lore-mcp/README.md#configuration) for full configuration mechanics.

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

Common settings:

- `enabled`: enables or disables Lore for the project.
- `command` and `args`: run a custom `lore-mcp` binary instead of managed selection.
- `env`: adds environment variables to the server process.
- `cwd`: sets the server working directory; relative paths resolve from the Pi project directory.
- `tools.disabled`: disables selected public Lore tools in Pi.
- `recovery.compilation` and `recovery.tests`: control recovery after failed compile or test validation.

By default, `pi-lore` enables definition caching and hides the public reset tool, because it tracks summarization and branch changes itself:

```json
{
  "env": {
    "LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE": "true",
    "LORE_MCP_TOOL_ENABLED_NOTIFY_KNOWLEDGE_RESET": "false"
  }
}
```

Values from `.pi/lore.config.json` are merged with these defaults. Host-level Pi configuration, when supplied by the host, takes precedence. Project configuration is not loaded until the project is trusted.

Advanced settings include startup and tool timeouts, recovery summary timeout, maximum inline diff size, tool override behavior, and `stateDir` for persistent Pi-side state.

Use a JSON patch for non-interactive tool and recovery changes:

```text
/lore-settings set {"recovery":{"tests":false}}
```

Edit other advanced values directly in `.pi/lore.config.json`.

## Server Selection

When `command` is not configured, `pi-lore` detects the project provider, reads the project GHC version, and selects a server binary with the exact same GHC version when one is available.

Downloaded binaries are shared by all projects and stored under `$XDG_CACHE_HOME/pi-lore` when `XDG_CACHE_HOME` is set, otherwise `~/.cache/pi-lore`.

There is no nearest-version fallback. When the project changes compiler or resolver, reload Pi so the extension can select the corresponding server.

For local Lore development, unsupported platforms, or missing binary builds, set a custom command:

```json
{
  "command": "/home/me/src/lore/dist/lore-mcp",
  "args": []
}
```

Open `/lore-settings`, choose **Set command to run Lore**, and enter a command on `PATH` or an executable path. Before saving, `pi-lore` verifies the binary identity against the expected Lore version, exact GHC version, and target platform.

Build instructions and `--version-json` verification are maintained in the [`lore-mcp` README](https://github.com/catdarick/lore/blob/main/lore-mcp/README.md#build-with-cabal).

## Recovery

Recovery starts when `reloadHomeModules` or `runTestSuite` returns a failed structured result. `pi-lore` records a project baseline, keeps repair work in a distinct section, and tracks which validation steps still need to pass.

After compilation and test checks succeed, `pi-lore` captures the resulting diff and asks Pi to summarize failed approaches, useful findings, and applied fixes. Future context uses that summary instead of the full repair transcript.

Disable compilation and test recovery independently under `/lore-settings` -> **Recovery**. `/lore-recovery-abandon` stops recovery without replacing the original entries.

## Troubleshooting

### Lore Cannot Detect The Project

Start Pi at the project root. Provider detection and supported manifest order are documented in the [`lore-mcp` configuration guide](https://github.com/catdarick/lore/blob/main/lore-mcp/README.md#session-settings).

### No Binary Is Available For This GHC Version

Choose **Build instructions** in the setup menu, build `lore-mcp` with the exact project compiler, and configure the resulting executable as a custom command.

### The Configured Binary Is Rejected

Run:

```bash
/path/to/lore-mcp --version-json
```

Its `loreVersion`, `ghcVersion`, and `target` must match the extension release, project compiler, and current platform.

### Settings Appear Unchanged

Run `/reload` or restart Pi. Startup settings and MCP tool registration are established when the server starts.

## Development

`pi-lore` is a TypeScript Pi package and does not use Cabal. From `pi-lore/`:

```bash
npm test
npm run validate:package
```

The Haskell server it launches is built separately with Cabal as described in the [`lore-mcp` README](https://github.com/catdarick/lore/blob/main/lore-mcp/README.md#build-with-cabal).
