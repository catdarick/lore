# lore-mcp

Executable package for exposing `lore` through an MCP server.

## Runtime configuration

`lore-mcp` reads optional environment variables when constructing `SessionConfig`:

- `LORE_MCP_PROJECT_ROOT`: overrides `projectRoot` (default `"."`)
- `LORE_MCP_GHC_WORK_DIR`: overrides `ghcWorkDir` (default `".lore-work"`)
- `LORE_MCP_CUSTOM_PRELUDE`: overrides `customPrelude`
  - unset: use base `Prelude`
  - non-empty module name (for example `CustomPrelude`): import that module instead of `Prelude`
- `LORE_MCP_PARALLEL_WORKERS_LIMIT`: overrides parallel worker limit
  - `auto`: `WorkersAsNumProcessors` (default)
  - positive integer (for example `4`): `ThisWorkersCount 4`
- `LORE_MCP_LOG_LEVEL`: enables server logging and sets the minimum emitted level
  - unset: disable logging
  - `debug`: emit debug, info, warning, and error logs
  - `info`: emit info, warning, and error logs
  - `warn` or `warning`: emit warning and error logs
  - `error`: emit only error logs

When enabled, logs are written to `stderr`. This keeps MCP protocol traffic on `stdout`.

Invalid values fail fast during server startup with a descriptive error message.
