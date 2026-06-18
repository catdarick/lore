# `discoverProject`

`discoverProject` reads the workspace manifests and describes packages and components. The agent can learn the project shape before reading source files, and it does not need home modules to compile.

**Benefit over file listing:** File listing shows paths. It does not explain which directories belong to each library, program, test suite, or benchmark. Lore reads the build configuration and shows the shape that GHC uses.

## Typical MCP input

```json
{}
```

## What the agent receives

The result lists package roots and manifests, then libraries, programs, tests, and benchmarks. Shared dependencies, extensions, and GHC options appear once instead of repeating in every component.

## Example

```text
Package: blog
  package root: blog/
  package manifest: blog/blog.cabal

Component: library
  source dirs: blog/src/
```
