# `notifyKnowledgeReset`

Clear Lore's memory of definitions already sent to the client.

This tool is disabled by default. It makes sense only for raw `lore-mcp` users whose client summarizes, compacts, or otherwise resets chat context while the same server keeps running.

`pi-lore` tracks summarization and branch changes automatically, so Pi users should normally leave this public tool disabled. For raw `lore-mcp`, enable it only when the agent has clear instructions to call it after summarization or compaction. Restarting `lore-mcp` after summarization is the simpler alternative.

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

For raw `lore-mcp`, this tool is relevant after compaction, summarization, or any other client-side context reset that removes previously returned definitions from the model's usable context while the server keeps running.

Normal source edits do not require it because changed definitions receive new fingerprints. Do not call it as a generic refresh step; doing so discards useful duplicate-suppression memory and can increase repeated source output.
