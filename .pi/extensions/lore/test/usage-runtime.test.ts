import assert from "node:assert/strict";
import { test } from "node:test";
import { resolve } from "node:path";
import { createLoreExtension } from "../src/index.ts";
import { FakePiHost } from "./test-support.ts";

const projectRoot = resolve(process.cwd(), "../../..");

test("runtime usage stats read active branch entries", async () => {
  const host = new FakePiHost(projectRoot);
  host.entries = [
    {
      id: "t1",
      role: "toolResult",
      toolCallId: "call-1",
      toolName: "legacyLoreTool",
      content: [{ type: "text", text: "branch result" }],
      details: { lore: {} },
    },
  ];
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    const stats = await runtime.getUsageStats();
    assert.equal(stats.totals.main.calls, 1);
    assert.equal(stats.tools[0]?.toolName, "legacyLoreTool");
  } finally {
    await runtime.stop();
  }
});
