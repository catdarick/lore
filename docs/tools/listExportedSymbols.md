# `listExportedSymbols`

`listExportedSymbols` lists symbols exported by one loaded module, including re-exports. It provides the public API before implementation source is retrieved.


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

## What the tool returns

The result lists symbol names and categories. Types can include their constructors, and classes can include methods. Results are paginated.

## Example

```text
1. data Article (Article)
2. publishArticle
3. articleTitle
```
