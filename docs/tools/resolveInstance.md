# `resolveInstance`

Ask GHC which typeclass instance applies to one concrete class application.
This replaces manual instance-chain reading with one selected result and only the constraints needed to understand it, avoiding broad source reads around candidate instances.

## Typical MCP input

```json
{
  "query": "Render Article"
}
```

The optional `instance` prefix is accepted.

## What the tool returns

The result shows the selected instance head. When project source exists, it also shows the declaration. For polymorphic instances, Lore can show type-variable substitutions and required constraints.

## Example

```text
Selected instance:

instance Render Article where
  render = renderArticle
```
