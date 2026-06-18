# `findReferences`

Find resolved uses of a known symbol across loaded project modules.

**Benefit over plain command output:** Lore returns focused snippets around real GHC-resolved references. This avoids broad text-search matches for unrelated names.

## Typical MCP input

```json
{
  "symbol": "Blog.Article.publishArticle",
  "verbosity": "Low",
  "maxResults": 10,
  "skip": 0
}
```

`Low` shows the usage itself. `Medium` adds its enclosing top-level definition. `High` adds broader control-flow context.

## What the agent receives

The result groups references by file and enclosing definition. Each entry includes a source location and a snippet. Results are paginated.

## Example

```text
src/Blog/Routes.hs

lines 28-34
  article <- publishArticle draft
```
