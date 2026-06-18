# `notifyKnowledgeReset`

Clear Lore's memory of definitions already sent to the client.

This tool exists only when definition caching is enabled.

## Typical MCP input

```json
{}
```

## What the agent receives

The result reports how many cached definition fingerprints were cleared. Later `getDefinitions` calls can return those definitions again.

## Example

```text
Knowledge reset acknowledged. Cleared 18 cached definition fingerprints.
```

Call this only after the client loses its earlier conversation context. Normal source edits do not require it because changed definitions receive new fingerprints.
