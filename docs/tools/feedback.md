# `feedback`

Append a concise report to the feedback file configured by the project.

This tool exists only when `mcp.feedback-file` is set.

## Typical MCP input

```json
{
  "title": "duplicate symbol result",
  "content": "searchSymbols returned the same qualified name twice after reload."
}
```

## What the tool returns

Lore appends a Markdown entry with the title and body, then returns the destination path.

## Example

```text
Feedback appended to .lore-work/mcp-feedback.md.
```
