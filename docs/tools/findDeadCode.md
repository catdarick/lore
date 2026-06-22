# `findDeadCode`

Find loaded top-level declarations that are not reachable from executable entry points or roots configured in `lore.yaml`.

## Dead-code roots

`findDeadCode` evaluates reachability separately for non-test components and test components.

A non-test executable `main` is a root for the non-test reachability graph. A test-suite `main` is a root only within that test component. References from tests do not make library declarations alive in the non-test graph, so a library symbol used only by tests is still reported as dead. This is intentional: test usage should not hide library code that is otherwise unreachable from non-test components.

Add public library modules and other externally called entry points explicitly when they are not reachable from a non-test executable:

```yaml
dead-code:
  alive-modules:
    - MyLibrary
    - MyLibrary.Public
    - MyLibrary.Plugin.*
  alive-symbols:
    - MyLibrary.startServer
```

Configured `alive-modules` and `alive-symbols` are added as roots to the non-test reachability graph. `alive-modules` entries use the same case-sensitive `*` module-pattern syntax as `searchSymbols.modulePatterns`; exact module names still work. They are useful for public APIs, plugin entry points, GHCi helpers, framework callbacks, and other code invoked outside the statically visible call graph.


## Typical MCP input

```json
{
  "modules": ["Blog.Article", "Blog.Store"],
  "skip": 0
}
```

Omit `modules` to inspect all loaded home modules. The filter changes what Lore reports, not the reachability graph.


## What the tool returns

The result reports scanned, alive, and dead counts, then groups dead names by module. Lore analyzes test and non-test components separately, so test-only usage does not keep library code alive in the non-test graph.

## Example

```text
Scanned 84 definitions: 80 alive, 4 dead.

Blog.Article:
- legacyArticleSlug
- renderOldSummary
```
