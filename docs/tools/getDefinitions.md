# `getDefinitions`

Return source definitions for one or more known symbols in project modules.

**Benefit over broad file output:** The agent can request only the declarations it needs instead of reading complete files. Dependency expansion adds a controlled amount of nearby logic.

## Typical MCP input

```json
{
  "symbols": ["Blog.Article.publishArticle"],
  "expansion": "Direct",
  "skip": 0
}
```

Expansion levels:

- `None` returns only the requested declarations.
- `Direct` adds one dependency layer.
- `Recursive` adds two dependency layers.

Lore returns at most 30 definition results per page. Source is available only for home modules.

## What the agent receives

The result groups declarations by source file and includes signatures. When definition caching is enabled, unchanged definitions already sent to the client may be listed as omitted.

## Example

A `Direct` request for `publishArticle` can return `publishArticle` together with helpers such as `validateDraft` and `saveArticle`, without returning unrelated code from those modules.
