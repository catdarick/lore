# `discoverProject`

`discoverProject` reads workspace manifests and describes packages and components. It provides the Cabal/Stack shape before source-file reads, and it does not require home modules to compile.


## Typical MCP input

```json
{}
```

## What the tool returns

The result lists package roots and manifests, then libraries, programs, tests, and benchmarks. Shared dependencies, extensions, and GHC options appear once instead of repeating in every component.

## Example

```text
# Workspace

- shared dependencies: aeson, base, containers
- shared GHC options: -Wall, -Wcompat, -Werror, -Widentities
- shared extensions: BangPatterns, BlockArguments

## Package: lore-mcp

- package root: lore-mcp/
- package manifest: lore-mcp/lore-mcp.cabal
- package shared dependencies: bytestring, directory, ghc

### Component: executable:lore-mcp

- source dirs: lore-mcp/app/
- main module: lore-mcp/app/Main.hs
- component specific dependencies: lore-mcp
- component specific GHC options: -rtsopts, -threaded, -with-rtsopts=-N

### Component: library

- source dirs: lore-mcp/src/
- main module: (none)

### Component: test:lore-mcp-test

- source dirs: lore-mcp/test/
- main module: lore-mcp/test/Spec.hs
- component specific dependencies: hspec, lore-mcp
- component specific GHC options: -Wno-incomplete-patterns, -rtsopts, -threaded, -with-rtsopts=-N
```
