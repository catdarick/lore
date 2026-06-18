# `lookupSymbolInfo`

`lookupSymbolInfo` resolves a known symbol and returns interface information. The agent can inspect metadata before deciding whether source code is needed.

**Benefit over text search:** Text search may find calls, definitions, comments, and tests together. Lore returns the actual resolved symbol, its type or declaration header, where it is defined, and where it is exported.

## Typical MCP input

```json
{
  "symbol": "Blog.Article.publishArticle",
  "skip": 0
}
```

The module qualifier is optional. Add it only when an unqualified name is ambiguous.

## What the agent receives

The result can include a function type, a type or class declaration header, constructors, the defining location, export modules, and direct class instances. If no exact symbol exists, Lore suggests similar names.

## Example

```text
publishArticle :: Draft -> IO Article
  Defined at: src/Blog/Article.hs:42
  Exported from: Blog.Article
```
