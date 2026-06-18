# `executeCode`

`executeCode` evaluates a small one-line Haskell expression or IO action in the current project context. The agent can run a focused runtime check without creating project files.

**Benefit over a temporary script or full test run:** The agent can check one expression without creating source files or running the whole suite. Failures return GHC diagnostics and common-fix hints.

## Typical MCP input

```json
{
  "code": "map articleTitle publishedArticles"
}
```

The result must have a `Show` instance, unless the expression performs its own IO output. Imports and multi-line declarations are not supported.

## What the agent receives

The result contains captured output and the shown value. Failures include GHC diagnostics and short hints for common interpreter mistakes.

## Example

```text
["First post","Release notes"]
```

This tool executes project code, so it belongs only in trusted workflows.
