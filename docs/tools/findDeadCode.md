# `findDeadCode`

Find loaded top-level declarations that are not reachable from executable entry points or roots configured in `lore.yaml`.

**Benefit over plain command output:** The result contains a summary and compact module-qualified names. It does not dump the source of every candidate.

## Typical MCP input

```json
{
  "modules": ["Blog.Article", "Blog.Store"],
  "skip": 0
}
```

Omit `modules` to inspect all loaded home modules. The filter changes what Lore reports, not the reachability graph.

In `lore.yaml`, `dead-code.alive-modules` accepts exact module names and `*` module patterns, for example `Blog.Plugin.*`. Matching modules are treated as non-test reachability roots.

## What the agent receives

The result reports scanned, alive, and dead counts, then groups dead names by module. Lore analyzes test and non-test components separately, so test-only usage does not keep library code alive in the non-test graph.

## Example

```text
Scanned 84 definitions: 80 alive, 4 dead.

Blog.Article:
- legacyArticleSlug
- renderOldSummary
```
