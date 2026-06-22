# `lookupInstances`

Search loaded class and type-family instance heads that mention all requested names. It provides broad instance discovery with compact instance heads instead of source-file reads or fragile `instance` text-search output.

## Typical MCP input

```json
{
  "names": ["Render", "Article"],
  "skip": 0
}
```

Provide at least two exact symbol names. Qualify ambiguous names with their modules.

## What the tool returns

The result contains matching class and family instance declarations from the current index. It does not decide which instance GHC will select for a concrete expression.

## Example

```text
- instance Render Article
- instance Render a => Render (Preview a)
```
