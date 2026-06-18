# `searchSymbols`

Find Haskell symbols when the exact name is unknown.

**Benefit over text search:** The tool returns a small ranked list of names and modules. It does not return source bodies, comments, or arbitrary text matches.

## Typical MCP input

```json
{
  "query": "publish article",
  "modulePatterns": ["Blog.*"]
}
```

Use a short phrase that resembles a Haskell name. Module patterns are optional; `*` matches any part of a module name.

## What the agent receives

Lore ranks up to ten grouped suggestions. It compares the query with symbol names, aliases, module names, and top-level argument and result types. It also handles common word variants, synonyms, and small spelling mistakes.

Capitalization is a ranking hint: `Article` favors types, while `article` favors values and functions.

## Example

```text
1. Blog.Article.publishArticle
2. Blog.Store.saveArticle
3. Blog.Article.PublishedArticle
```
