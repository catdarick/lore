# `createTemporalModule`

Create a temporary Haskell module for multi-line debugging code.


## Typical MCP input

```json
{}
```

## What the tool returns

The tool returns the new module path and the required workflow:

1. Write imports, declarations, types, or instances to the file.
2. Run `reloadHomeModules`.
3. Call the new definitions with `executeCode`.

The module remains attached across reloads. Delete the file to detach it. A session restart removes it.

## Example

```text
Temporal module initialized at: .lore-work/temporal/LoreTemporal1.hs
```
