# `reloadHomeModules`

`reloadHomeModules` reloads project home modules into GHC. The agent can call it after source changes, before symbol lookup, reference lookup, type inference, or evaluation.

**Benefit over build commands:** Build output can be long and noisy. Lore returns a structured status, module counts, grouped diagnostics, focused snippets, and safe automatic import fixes.

## Typical MCP input

```json
{ "skip": 0 }
```

`skip` asks for the next diagnostic page. The default page contains up to five diagnostics.

## What the agent receives

The text result contains the load summary, safe fixes, and focused diagnostics. The structured result reports `success`, `compilation-failure`, `environment-failure`, or `restart-required`.

The reload may remove redundant imports. It also clears interactive bindings created by `executeCode`.

## Example

```text
Successfully loaded all 12 modules. No errors found.
```
