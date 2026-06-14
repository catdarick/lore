import assert from "node:assert/strict";
import { resolve } from "node:path";
import { test } from "node:test";
import { createLoreExtension } from "../src/index.ts";
import { FakePiHost } from "./test-support.ts";

const projectRoot = resolve(process.cwd(), "../");

function stripAnsi(text: string): string {
  return text.replace(/\x1b\[[0-9;]*m/g, "");
}

test("registers Lore tools dynamically and normalizes required-nullable schema fields", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    assert.deepEqual([...host.tools.keys()].sort(), ["echo", "getDefinition", "reloadHomeModules", "runTestSuite"]);
    assert.deepEqual(host.tools.get("getDefinition")?.parameters, {
      type: "object",
      properties: { symbols: { type: "array" } },
    });
    assert.deepEqual(host.tools.get("runTestSuite")?.parameters, {
      type: "object",
      properties: {
        success: { type: "boolean" },
        testArgs: { type: "array", items: { type: "string" } },
      },
    });
    const guidelines = host.tools.get("reloadHomeModules")?.promptGuidelines?.join("\n") ?? "";
    assert.match(guidelines, /structured semantic failures/);
    assert.doesNotMatch(guidelines, /Pi may inject system context messages/);
    assert.doesNotMatch(guidelines, /\[\[LORE_SECTION_STARTED:<id>\]\]/);
    assert.doesNotMatch(guidelines, /\[\[LORE_FIXES_APPLIED\]\]/);
  } finally {
    await runtime.stop();
  }
});

test("captures changed knowledge snapshots after getDefinition", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    const result = (await host.tools.get("getDefinition")?.execute("call-1", { symbols: ["Foo"] })) as {
      details: { lore: { knowledgeSnapshot?: { hashes: string[] } } };
    };
    assert.deepEqual(result.details.lore.knowledgeSnapshot?.hashes, ["hash:Foo"]);
    host.appendEntry({
      role: "toolResult",
      toolCallId: "call-1",
      toolName: "getDefinition",
      details: result.details,
    });
    const second = (await host.tools.get("getDefinition")?.execute("call-2", { symbols: ["Foo"] })) as {
      details: { lore: { knowledgeSnapshot?: { hashes: string[] } } };
    };
    assert.equal(second.details.lore.knowledgeSnapshot, undefined);
  } finally {
    await runtime.stop();
  }
});

test("consecutive definition calls preserve pending knowledge before Pi appends tool result", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    const first = (await host.tools.get("getDefinition")?.execute("call-1", { symbols: ["Foo"] })) as {
      details: { lore: { knowledgeSnapshot?: { hashes: string[] } } };
    };
    const second = (await host.tools.get("getDefinition")?.execute("call-2", { symbols: ["Bar"] })) as {
      details: { lore: { knowledgeSnapshot?: { hashes: string[] } } };
    };
    assert.deepEqual(first.details.lore.knowledgeSnapshot?.hashes, ["hash:Foo"]);
    assert.deepEqual(second.details.lore.knowledgeSnapshot?.hashes, ["hash:Bar", "hash:Foo"]);
  } finally {
    await runtime.stop();
  }
});


test("timeout kills Lore and next operation restores branch knowledge without retrying", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    const result = (await host.tools.get("getDefinition")?.execute("call-1", { symbols: ["Foo"] })) as {
      details: { lore: { knowledgeSnapshot?: { hashes: string[] } } };
    };
    host.appendEntry({
      role: "toolResult",
      toolCallId: "call-1",
      toolName: "getDefinition",
      details: result.details,
    });
    await assert.rejects(() => host.tools.get("reloadHomeModules")!.execute("call-2", { sleepMs: 500 }), /timed out/i);
    const echo = (await host.tools.get("echo")?.execute("call-3", {})) as {
      details: { lore: { structuredContent: { cache: string[] } } };
    };
    assert.deepEqual(echo.details.lore.structuredContent.cache, ["hash:Foo"]);
  } finally {
    await runtime.stop();
  }
});

test("synthesizes visible tool text when structured result has empty content", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    const result = (await host.tools
      .get("reloadHomeModules")
      ?.execute("call-1", { success: false, emptyContent: true })) as {
      content: Array<{ type: string; text?: string }>;
    };
    const rendered = result.content
      .filter((part) => part.type === "text")
      .map((part) => part.text ?? "")
      .join("\n");
    assert.match(rendered, /returned no text content/i);
    assert.match(rendered, /"success":false/);
  } finally {
    await runtime.stop();
  }
});

test("registers custom renderers that keep result sections collapsed by default", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    const tool = host.tools.get("echo");
    assert.equal(typeof tool?.renderCall, "function");
    assert.equal(typeof tool?.renderResult, "function");

    const state: Record<string, unknown> = {};
    let invalidations = 0;

    const beforeCall = tool?.renderCall?.({ foo: "bar" }, {}, { args: { foo: "bar" }, state });
    assert.ok(beforeCall);
    const beforeCallText = stripAnsi(beforeCall?.render(200).join("\n") ?? "");
    assert.match(beforeCallText, /^echo \{"foo":"bar"\}$/m);

    const collapsedFirst = tool?.renderResult?.(
      {
        content: [{ type: "text", text: "hello world" }],
        details: { lore: { structuredContent: { ok: true } } },
        isError: false,
      },
      { expanded: false, isPartial: false },
      {},
      { args: { foo: "bar" }, state, invalidate: () => void (invalidations += 1) },
    );
    assert.ok(collapsedFirst);
    const collapsed = tool?.renderResult?.(
      {
        content: [{ type: "text", text: "hello world" }],
        details: { lore: { structuredContent: { ok: true } } },
        isError: false,
      },
      { expanded: false, isPartial: false },
      {},
      { args: { foo: "bar" }, state, invalidate: () => void (invalidations += 1) },
    );
    assert.ok(collapsed);
    const collapsedText = stripAnsi(collapsed?.render(200).join("\n") ?? "");
    assert.doesNotMatch(collapsedText, /^echo /m);
    assert.match(collapsedText, /hello world/);

    const afterCall = tool?.renderCall?.({ foo: "bar" }, {}, { args: { foo: "bar" }, state });
    assert.ok(afterCall);
    const afterCallText = stripAnsi(afterCall?.render(200).join("\n") ?? "");
    assert.match(afterCallText, /^echo \{"foo":"bar"\} – completed \(~\d+ tokens\)$/m);
    assert.ok(invalidations >= 1);

    const expanded = tool?.renderResult?.(
      {
        content: [{ type: "text", text: "hello world" }],
        details: { lore: { structuredContent: { ok: true } } },
        isError: false,
      },
      { expanded: true, isPartial: false },
      {},
      { args: { foo: "bar" }, state },
    );
    assert.ok(expanded);
    const expandedText = stripAnsi(expanded?.render(200).join("\n") ?? "");
    assert.match(expandedText, /Output:/);
    assert.match(expandedText, /Structured Content:/);

    const dedupedFirst = tool?.renderResult?.(
      {
        content: [
          { type: "text", text: "Failed to load 1 of 2 modules.\n## A" },
          { type: "text", text: "Failed to load 1 of 2 modules.\n## A" },
        ],
        details: { lore: { structuredContent: { ok: false } } },
        isError: false,
      },
      { expanded: true, isPartial: false },
      {},
      { args: { skip: 0 }, state },
    );
    assert.ok(dedupedFirst);
    const deduped = tool?.renderResult?.(
      {
        content: [
          { type: "text", text: "Failed to load 1 of 2 modules.\n## A" },
          { type: "text", text: "Failed to load 1 of 2 modules.\n## A" },
        ],
        details: { lore: { structuredContent: { ok: false } } },
        isError: false,
      },
      { expanded: true, isPartial: false },
      {},
      { args: { skip: 0 }, state },
    );
    const dedupedText = stripAnsi(deduped?.render(200).join("\n") ?? "");
    const matches = dedupedText.match(/Failed to load 1 of 2 modules\./g) ?? [];
    assert.equal(matches.length, 1);
  } finally {
    await runtime.stop();
  }
});
