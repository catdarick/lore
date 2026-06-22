# `executeCode`

`executeCode` evaluates a small one-line Haskell expression or IO action in the current project context. It provides a focused runtime check without adding source files or running a full test suite.


## Typical MCP input

```json
{
  "code": "map articleTitle publishedArticles"
}
```

The result must have a `Show` instance, unless the expression performs its own IO output. Imports and multi-line declarations are not supported.

## What the tool returns

The result contains captured output and the shown value. Failures include GHC diagnostics and short hints for common interpreter mistakes.

## Example

```text
["First post","Release notes"]
```

This tool executes project code, so it belongs only in trusted workflows.
