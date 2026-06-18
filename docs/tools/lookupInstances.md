# `lookupInstances`

Search loaded class and type-family instance heads that mention all requested names.

**Benefit over plain command output:** The agent can use it for broad instance discovery. It returns compact instance heads instead of source files.

## Typical MCP input

```json
{
  "names": ["Render", "Article"],
  "skip": 0
}
```

Provide at least two exact symbol names. Qualify ambiguous names with their modules.

## What the agent receives

The result contains matching class and family instance declarations from the current index. It does not decide which instance GHC will select for a concrete expression.

## Example

```text
- instance Render Article
- instance Render a => Render (Preview a)
```
