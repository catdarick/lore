# Lore

Compiler-aware tools that help coding agents understand and modify Haskell projects.

Lore loads a project through the GHC API and exposes structured operations for compilation, project discovery, symbol navigation, source definitions, references, typeclass instances, type inference, code evaluation, tests, and dead-code analysis.

Choose the frontend that matches the intended workflow:

- **[`pi-lore`](pi-lore/README.md)** — the easiest way to use Lore in Pi, with automatic server setup and context-management features.
- **[`lore-mcp`](lore-mcp/README.md)** — a stdio Model Context Protocol server for Pi, IDEs, and other MCP clients.
- **[`lore-cli`](lore-tools-cli/README.md)** — an interactive and single-command terminal frontend, useful for exploration and CI.

## What Lore provides

Lore helps coding agents work with Haskell through compiler facts instead of raw text. This matters for teams that want an agent to edit real projects without filling the context with text-search output, full file dumps, or long build logs.

With Lore, an agent can:

- learn the Cabal or Stack workspace shape before reading files;
- discover symbols by names, modules, and type signatures;
- inspect a module's public API without reading its implementation;
- fetch exact source definitions, with controlled dependency expansion;
- find resolved references instead of same-text matches;
- ask GHC which instance applies to a concrete type;
- compile and test changes with structured diagnostics;
- run small type or runtime checks in the loaded project context.

Terminal tools are still useful for arbitrary text search and project-specific commands. Lore adds the Haskell-specific layer that terminal tools do not have: GHC name resolution, type information, module exports, instance selection, and compact diagnostic rendering.

See the [tool guide](docs/Tools.md) for the full tool list and the benefits over raw terminal commands.

## Choose how to use Lore

### Pi integration

Use `pi-lore` for the most complete experience. It starts `lore-mcp`, registers its tools in Pi, and adds:

- automatic matching-binary setup;
- branch-aware definition memory;
- suppression of duplicate source definitions;
- recovery summaries for compilation and test-fixing sessions;
- interactive settings, status, restart, and usage-statistics commands.

Install it for the current user:

```bash
pi install npm:pi-lore
```

Or install it only for the current project:

```bash
pi install npm:pi-lore -l
```

Then start Pi from the Haskell project root. See the [pi-lore README](pi-lore/README.md) for requirements, managed binary support, configuration, and troubleshooting.

### Direct MCP integration

Use `lore-mcp` when your client supports local stdio MCP servers.

Build it with the exact GHC version used by the target project:

```bash
git clone https://github.com/catdarick/lore.git
cd lore

cabal build exe:lore-mcp -w ghc-9.6.5
cabal list-bin exe:lore-mcp -w ghc-9.6.5
```

Replace `9.6.5` with the project's full compiler version. Start the resulting executable with the target project as its working directory.

A typical client configuration looks like this:

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

See the [tool guide](docs/Tools.md) for tool behavior and the [lore-mcp README](lore-mcp/README.md) for `lore.yaml`, environment variables, and security considerations.

### Command-line interface

Build the CLI:

```bash
cabal build exe:lore-cli
cabal list-bin exe:lore-cli
```

Run it without a subcommand to open the interactive prompt:

```bash
cabal run lore-cli
```

Or execute one operation directly:

```bash
cabal run lore-cli -- discover-project
cabal run lore-cli -- reload
cabal run lore-cli -- search-symbols publishArticle
cabal run lore-cli -- get-definition Blog.Article.publishArticle --recursive
cabal run lore-cli -- find-dead-code --limit 20
```

Markdown is the default output format. Use JSON for scripts and CI:

```bash
cabal run lore-cli -- --format json discover-project
```

Inside the interactive prompt, use `:help`, `:help COMMAND`, `:write`, and `:tee` for command help and output forwarding.

## Requirements

### Building Lore

- GHC 9.6 or newer within the package's supported bounds.
- Cabal.
- `hpack` when regenerating Cabal files from `package.yaml` or when a target project requires its generated Cabal file to be refreshed.
- Node.js 24 or newer only when developing or packaging `pi-lore`.

### Inspecting a project

The target project's build tool and compiler must be available in the environment where Lore runs.

Lore detects project providers in this order:

1. `stack.yaml` → Stack
2. `cabal.project` → Cabal
3. `package.yaml` → Cabal
4. exactly one root-level `*.cabal` file → Cabal

A directory containing multiple root-level Cabal files should provide a `cabal.project`.

## GHC compatibility

Lore links against the GHC API, so a `lore-mcp` or `lore-cli` binary must be built with the **same full GHC version** as the project it inspects.

For example, a binary built with GHC 9.6.5 must not be used for a project running GHC 9.6.7.

Check the project compiler with:

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

`pi-lore` performs this validation automatically before starting a downloaded or manually configured server.

## Configuration overview

`lore-mcp` and `lore-cli` read optional project configuration from `lore.yaml`.

A minimal example:

```yaml
session:
  project-root: .
  ghc-work-dir: .lore-work
  parallel-workers-limit: auto
  log-level: info

dead-code:
  alive-modules:
    - Blog.Public
  alive-symbols:
    - runBlog

symbol-search:
  synonym-groups:
    - [article, post]
    - [author, writer]

mcp:
  tools:
    runTestSuite: true
```

For `lore-mcp` and `lore-cli`, configuration precedence is:

```text
built-in defaults < lore.yaml < environment variables
```

A frontend such as `pi-lore` may provide environment overrides when it starts the server.

The major sections are:

| Section | Purpose |
| --- | --- |
| `session` | Project root, working directory, custom prelude, parallelism, logging, and default test arguments. |
| `dead-code` | Modules and symbols that should be treated as reachable roots. |
| `symbol-search` | Project-specific synonym groups added to Lore's built-in search vocabulary. |
| `mcp` | MCP tool enablement, definition caching, feedback output, and custom shell-command tools. |

For the full configuration reference, including environment variables and path resolution, see [lore-mcp configuration](lore-mcp/README.md#configuration).

`pi-lore` has a separate `.pi/lore.config.json` file for Pi-specific startup, timeout, tool-selection, and recovery settings. See [pi-lore configuration](pi-lore/README.md#configuration).

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

Lore can compile project code, evaluate expressions, run test suites, and execute configured shell-command tools. Use these capabilities only with projects and configuration you trust.

For direct MCP integrations, review which tools are enabled before exposing the server to an agent. `runTestSuite` is disabled by default; custom tools and evaluation features can also be disabled in `lore.yaml`.

## License

BSD-3-Clause.
