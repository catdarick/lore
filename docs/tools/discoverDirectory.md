# `discoverDirectory`

`discoverDirectory` returns a bounded tree for one project directory. The agent can understand layout before choosing files or modules.

**Benefit over file listing:** A plain file tree can dump thousands of paths. Lore stays inside the project, skips ignored directories, collapses simple paths, and trims noisy folders.

## Typical MCP input

```json
{
  "path": "src",
  "depth": 2
}
```

`depth: 0` shows the requested directory and its immediate entries. Larger values open more levels.

## What the agent receives

The result is a compact tree. Closed directories show a file count, and trimmed sections show how many entries were omitted.

## Example

```text
src/
├── Blog/
│   ├── Article.hs
│   └── Store.hs
└── Main.hs
```
