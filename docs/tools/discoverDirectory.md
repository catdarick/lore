# `discoverDirectory`

`discoverDirectory` returns a bounded tree for one project directory. It provides layout context before any file, module, or symbol-specific inspection.


## Typical MCP input

```json
{
  "path": "src",
  "depth": 2
}
```

`depth: 0` shows the requested directory and its immediate entries. Larger values open more levels.

## What the tool returns

The result is a compact tree. Closed directories show a file count, and trimmed sections show how many entries were omitted.

## Example

```text
src/
├── Blog/
│   ├── Article.hs
│   └── Store.hs
└── Main.hs
```
