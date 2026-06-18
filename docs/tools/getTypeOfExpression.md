# `getTypeOfExpression`

Infer the type of a Haskell expression in the current interpreter context without evaluating it.

**Benefit over plain command output:** The agent can answer a type question without retrieving source or running code.

## Typical MCP input

```json
{
  "expression": "map articleTitle publishedArticles"
}
```

The input must be an expression, not an import, declaration, or statement.

## What the agent receives

The result contains the inferred type or focused GHC diagnostics.

## Example

```text
Type

[Text]
```
