# Lore

Compiler-aware tools that help coding agents understand and modify Haskell projects while keeping large-codebase context pressure low.

Lore loads a project through the GHC API and exposes structured operations for project discovery, targeted source retrieval, symbol navigation, compilation diagnostics, typeclass instances, type inference, code evaluation, tests, and dead-code analysis. The tools let agents retrieve project facts and source slices instead of repeatedly filling context with whole files, broad text-search output, or raw build logs.

The [tool guide](docs/Tools.md) is the canonical tool reference. It lists every shared Lore tool, explains the context-pressure benefit, and links to per-tool input/output examples.

## Choose a frontend

| Frontend | Best fit | More details |
| --- | --- | --- |
| `pi-lore` | Pi users who want managed server setup, branch-aware definition memory, recovery summaries, settings, status, and usage statistics. | [`pi-lore/README.md`](pi-lore/README.md) |
| `lore-mcp` | MCP clients that can launch a local stdio server directly. | [`lore-mcp/README.md`](lore-mcp/README.md) |
| `lore-cli` | Shell, scripting, CI, or interactive terminal exploration. | [`lore-tools-cli/README.md`](lore-tools-cli/README.md) |

## Requirements

### Building Lore

- GHC 9.6 or newer within the package's supported bounds.
- Cabal.
- `hpack` when regenerating Cabal files from `package.yaml` or when a target project requires its generated Cabal file to be refreshed.
- Node.js 24 or newer only when developing or packaging `pi-lore`.

### Inspecting a project

The target project's build tool and compiler must be available where Lore runs. Lore supports Stack and Cabal projects; provider detection and path behavior are documented in the [`lore-mcp` configuration guide](lore-mcp/README.md#configuration).

## GHC compatibility

Lore links against the GHC API, so a `lore-mcp` or `lore-cli` binary must be built with the **same full GHC version** as the project it inspects.

For example, a binary built with GHC 9.6.5 must not be used for a project running GHC 9.6.7.

Check a project compiler with:

```bash
# Cabal project
cabal exec --write-ghc-environment-files=never -- ghc --numeric-version

# Stack project
stack exec -- ghc --numeric-version
```

Check a Lore server binary with:

```bash
lore-mcp --version-json
```

`pi-lore` validates downloaded and manually configured servers before starting them.

## Configuration overview

`lore-mcp` and `lore-cli` read optional project configuration from `lore.yaml`. A frontend such as `pi-lore` may provide environment overrides when it starts the server.

For `lore-mcp` and `lore-cli`, configuration precedence is:

```text
built-in defaults < lore.yaml < environment variables
```

| Configuration area | Canonical doc |
| --- | --- |
| Haskell session, project root, GHC work directory, dead-code roots, symbol-search synonyms, MCP tool enablement, feedback, and custom command tools. | [`lore-mcp` configuration](lore-mcp/README.md#configuration) |
| Pi-specific startup, managed binary selection, tool proxying, recovery, timeout, and state settings. | [`pi-lore` configuration](pi-lore/README.md#configuration) |
| Per-tool input, output, examples, and tool-owned configuration semantics. | [Tool guide](docs/Tools.md) |

## Build the repository

Clone the repository and build all Cabal packages:

```bash
git clone https://github.com/catdarick/lore.git
cd lore

cabal build all
```

Build only the end-user executables:

```bash
cabal build exe:lore-mcp exe:lore-cli
```

Find the resulting executable paths:

```bash
cabal list-bin exe:lore-mcp
cabal list-bin exe:lore-cli
```

Run the Haskell test suites:

```bash
cabal test all
```

Build an optimized server binary:

```bash
cabal build exe:lore-mcp --enable-optimization=2
```

The repository also contains Stack configuration for supported development workflows, but Cabal is the primary build path documented here.

## Repository layout

| Path | Purpose |
| --- | --- |
| [`lore/`](lore/) | Core GHC session, loading, analysis, interpreter, configuration, and project support. |
| [`lore-tools/`](lore-tools/) | Shared tool operations, structured results, and rendering used by frontends. |
| [`lore-mcp/`](lore-mcp/) | MCP protocol server, tool schemas, MCP configuration, and custom command tools. |
| [`lore-tools-cli/`](lore-tools-cli/) | Interactive and single-command terminal frontend exposed as `lore-cli`. |
| [`pi-lore/`](pi-lore/) | Pi package that manages `lore-mcp` and adds Pi-specific context and recovery behavior. |
| [`docs/Tools.md`](docs/Tools.md) | Canonical shared tool guide and links to per-tool pages. |
| [`scripts/`](scripts/) | Repository maintenance and release scripts. |
| [`.github/workflows/`](.github/workflows/) | Build, release, binary packaging, and npm publication workflows. |

The dependency direction is approximately:

```text
lore
  ↓
lore-tools
  ├─→ lore-mcp
  └─→ lore-tools-cli

lore-mcp ← managed by → pi-lore
```

## Development workflow

After changing Haskell code:

```bash
cabal build all
cabal test all
```

After changing the Pi package:

```bash
cd pi-lore
npm test
npm run validate:package
```

When modifying `package.yaml`, regenerate the corresponding Cabal file with the repository's supported `hpack` version before committing both source-of-truth and generated manifest changes.

Useful repository tasks are also available through `Taskfile.yml`, including formatting, building, release version updates, and MCP Inspector startup.

## Security

Lore can compile project code, evaluate expressions, run test suites, and execute configured shell-command tools. These capabilities are appropriate only for projects and configuration that the developer trusts.

For direct MCP integrations, review which tools are enabled before exposing the server to an agent. `runTestSuite` is disabled by default; custom tools and evaluation features can also be disabled in `lore.yaml`.

## License

BSD-3-Clause.
