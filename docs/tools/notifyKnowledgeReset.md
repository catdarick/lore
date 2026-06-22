# `notifyKnowledgeReset`

Clear Lore's memory of definitions already sent to the client.

This tool exists only when definition caching is enabled. It keeps targeted source retrieval aligned with the client's actual conversation context.

## Typical MCP input

```json
{}
```

## What the tool returns

The result reports how many cached definition fingerprints were cleared. Later `getDefinitions` calls can return those definitions again.

## Example

```text
Knowledge reset acknowledged. Cleared 18 cached definition fingerprints.
```

This tool is relevant after compaction, summarization, branch-context loss, or any other client-side context reset that removes previously returned definitions from the model's usable context.

Normal source edits do not require it because changed definitions receive new fingerprints. If the workflow does not use client-side context reset, consider disabling this tool through `lore.yaml` or `LORE_MCP_TOOL_ENABLED_NOTIFY_KNOWLEDGE_RESET`.
