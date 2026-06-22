# `getDefinitions`

Return source definitions for one or more known symbols in project modules.

This is Lore's main source-retrieval tool: it returns only the requested declarations instead of complete files. Dependency expansion adds a controlled amount of nearby logic when direct definitions are not enough.

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

`None` is the smallest expansion and is appropriate when the requested declaration is likely enough. `Direct` and `Recursive` add helper logic when needed; they are useful, but deliberately spend more context.

Lore returns at most 30 definition results per page. Source is available only for home modules.

## What the tool returns

The result groups declarations by source file and includes signatures. When definition caching is enabled, unchanged definitions already sent to the client may be listed as omitted instead of repeated. That keeps source retrieval aligned with the client's current context.

After client-side compaction or context reset, [`notifyKnowledgeReset`](notifyKnowledgeReset.md) clears duplicate-suppression memory so previously omitted definitions can be returned again.

## Example

A `Direct` request for `publishArticle` can return `publishArticle` together with helpers such as `validateDraft` and `saveArticle`, without returning unrelated code from those modules.
