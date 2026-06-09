# lore monorepo

This repository contains two Haskell packages:

- `lore/`: the core library
- `lore-mcp/`: an executable package intended to host the MCP server layer

Use the repository root `stack.yaml` or `cabal.project` to build the workspace.

## Project Configuration

Projects can add symbol-search synonym groups in the repository-root `lore.yaml`:

```yaml
dead-code:
  alive-modules:
    - "Main"
  alive-symbols:
    - "runApplication"

symbol-search:
  synonym-groups:
    - ["customer", "client"]
    - ["enqueue", "schedule", "submit"]
```

Project groups extend the built-in synonym groups. Relationships are symmetric but direct and non-transitive: if `"alpha"` is grouped with `"beta"` and `"beta"` is grouped with `"gamma"`, `"alpha"` is not automatically a synonym of `"gamma"`.

Each entry must represent exactly one search token after normal symbol-search tokenization and canonicalization. Quote all terms, especially operators and YAML-like words such as `"null"`. Phrase synonyms such as `"customer account"` are not supported.

Config edits affect the next search. The symbol index does not need to be rebuilt because synonyms only affect query-token matching against the existing indexed vocabulary.
