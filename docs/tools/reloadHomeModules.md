# `reloadHomeModules`

`reloadHomeModules` reloads project home modules into GHC. After source changes, it is the validation and refresh point for symbol lookup, reference lookup, type inference, evaluation, and test execution.

The tool refreshes Lore's loaded-module state and indexes, applies safe import fixes when available, and returns paginated GHC diagnostics instead of raw build output.

## Typical MCP input

```json
{ "skip": 0 }
```

`skip` asks for the next diagnostic page. The default page contains up to five diagnostics.

## What the tool returns

The text result contains the load summary, safe fixes applied, and focused diagnostics. Diagnostics include source locations and snippets when available, and pagination keeps repeated repair attempts from filling context with the same full build log.

The reload may remove redundant imports. It also clears interactive bindings created by `executeCode` while preserving the project modules and configured temporal modules.

## Example

```text
Successfully loaded all 12 modules. No errors found.
```
