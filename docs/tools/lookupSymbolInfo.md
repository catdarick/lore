# `lookupSymbolInfo`

`lookupSymbolInfo` resolves a known symbol and returns interface information. It provides metadata before implementation source is retrieved. This is analogous to `:info` in GHCi, but it can search all loaded top-level symbols rather than only symbols exported from currently scoped modules.

## Typical MCP input

```json
{
  "symbol": "Blog.Article.publishArticle",
  "skip": 0
}
```

The module qualifier is optional. Add it only when an unqualified name is ambiguous.

## What the tool returns

The result can include a function type, a type or class declaration header, constructors, the defining location, export modules, and direct class instances. If no exact symbol exists, Lore suggests similar names.

This tool is sufficient when interface metadata answers the question. [`getDefinitions`](getDefinitions.md) is the source-retrieval tool for cases where implementation details matter.

## Example

```text
publishArticle :: Draft -> IO Article
  Defined at: src/Blog/Article.hs:42
  Exported from: Blog.Article
```
