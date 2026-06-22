# `findReferences`

Find resolved uses of a known symbol across loaded project modules. Lore returns focused snippets around real GHC-resolved references, giving usage context without broad grep output or unrelated same-text matches.

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

## What the tool returns

The result groups references by file and enclosing definition. Each entry includes a source location and a snippet. Results are paginated.

## Example

```text
src/Blog/Routes.hs

lines 28-34
  article <- publishArticle draft
```
