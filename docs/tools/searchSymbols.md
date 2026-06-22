# `searchSymbols`

Find Haskell symbols when the exact name is unknown.
The tool returns a small ranked list of names and modules using fuzzy and semantic matching. It does not return source bodies, comments, or arbitrary text matches, so symbol discovery happens before source retrieval.


## Typical MCP input

```json
{
  "query": "persistArticle",
  "modulePatterns": ["Blog.*"]
}
```

Queries work best as short phrases that resemble Haskell names. Module patterns are optional; `*` matches any part of a module name.

## What the tool returns

Lore ranks up to ten grouped suggestions. It compares the query with symbol names, aliases, module names, and top-level argument and result types. It also handles common word variants, synonyms, and small spelling mistakes.

Capitalization is a ranking hint: `Article` favors types, while `article` favors values and functions.

## Example

```text
1. Blog.Store.saveArticle
2. Blog.Article.publishArticle
3. Blog.Article.PublishedArticle
```

## Symbol-search synonyms

Lore includes general programming synonyms and abbreviations. Add project-specific vocabulary with groups of equivalent terms:

```yaml
symbol-search:
  synonym-groups:
    - [account, profile]
    - [author, writer]
    - ["pull request", PR]
```

Project groups are merged with Lore's built-in vocabulary; they do not replace it. Each group must contain at least two distinct normalized terms, and every term in a group is a direct, bidirectional synonym of the other terms in that group. Overlapping groups do not create transitive matches.

Multi-word terms should be quoted as one YAML string. They match only when their normalized tokens occur contiguously and in order within one indexed name, module, argument type, or result type.