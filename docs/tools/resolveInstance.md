# `resolveInstance`

Ask GHC which typeclass instance applies to one concrete class application.

**Benefit over plain command output:** This replaces manual instance-chain reading with one selected result and only the constraints needed to understand it.

## Typical MCP input

```json
{
  "query": "Render Article"
}
```

The optional `instance` prefix is accepted.

## What the agent receives

The result shows the selected instance head. When project source exists, it also shows the declaration. For polymorphic instances, Lore can show type-variable substitutions and required constraints.

## Example

```text
Selected instance:

instance Render Article where
  render = renderArticle
```
