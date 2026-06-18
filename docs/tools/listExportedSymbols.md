# `listExportedSymbols`

`listExportedSymbols` lists symbols exported by one loaded module, including re-exports. The agent can inspect an API without reading the implementation.

**Benefit over text search:** Text search can find an export list, but it cannot reliably include re-exports or show constructor and method structure. Lore asks the loaded GHC session.

## Typical MCP input

```json
{
  "moduleName": "Blog.Article",
  "packageName": null,
  "typeHint": "Article",
  "skip": 0
}
```

`packageName` disambiguates duplicate module names. `typeHint` keeps symbols whose own type directly mentions that name.

## What the agent receives

The result lists symbol names and categories. Types can include their constructors, and classes can include methods. Results are paginated.

## Example

```text
1. data Article (Article)
2. publishArticle
3. articleTitle
```
