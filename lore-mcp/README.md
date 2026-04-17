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
- `LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE`: enables `getDefinition` duplicate-suppression memory
  - unset: disabled (default)
  - truthy values: `1`, `true`, `yes`, `on`
  - falsy values: `0`, `false`, `no`, `off`

## Definition knowledge cache (optional)

When `LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE` is enabled:

- `getDefinition` remembers previously returned definition bodies and omits repeats in later calls.
- `getDefinition` accepts an optional `force` flag; when `force=true`, the knowledge check is ignored and all requested symbol definitions are returned (including recursive ones).
- The server exposes `notifyKnowledgeReset`, which clears that memory and makes previously omitted definitions returnable again.

When the flag is disabled (default), `getDefinition` behaves as before and `notifyKnowledgeReset` is not registered.

When enabled, logs are written to `stderr`. This keeps MCP protocol traffic on `stdout`.

Invalid values fail fast during server startup with a descriptive error message.
