import assert from "node:assert/strict";
import { test } from "node:test";
import { loadLoreConfig } from "../src/config.ts";
import { createLoreExtension } from "../src/index.ts";
import { LoreClient } from "../src/mcp-client.ts";
import { FakePiHost } from "./test-support.ts";
import { join, resolve } from "node:path";

test("default config enables Lore definition knowledge RPC at the producer", () => {
  const config = loadLoreConfig({ projectDir: process.cwd() });
  assert.equal(config.env.LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE, "true");
  assert.equal(config.env.LORE_MCP_TOOL_ENABLED_NOTIFY_KNOWLEDGE_RESET, "false");
  assert.deepEqual(config.tools, { disabled: [] });
  assert.deepEqual(config.recovery, { compilation: true, tests: true });
});

test("config supports tool and recovery feature gates", () => {
  const config = loadLoreConfig({
    projectDir: process.cwd(),
    getConfig: () => ({
      tools: {
        disabled: ["getDefinition"],
      },
      recovery: {
        compilation: false,
        tests: true,
      },
    }),
  });

  assert.deepEqual(config.tools, {
    disabled: ["getDefinition"],
  });
  assert.deepEqual(config.recovery, { compilation: false, tests: true });
});

test("startup reports unavailable Lore without throwing through Pi", async () => {
  const host = new FakePiHost(process.cwd());
  host.getConfig = () => ({
    command: "python3",
    args: ["-c", "import sys; sys.exit(2)"],
    cwd: process.cwd(),
    startupTimeoutMs: 500,
    defaultToolTimeoutMs: 500,
    toolTimeoutMs: {},
    summaryTimeoutMs: 500,
    maxInlineDiffBytes: 1000,
  });
  const runtime = await createLoreExtension(host);
  await runtime.start();
  assert.match(host.statuses.get("lore-extension") ?? "", /Lore extension unavailable:/);
});

test("runtime startup does not require an attached Pi session branch", async () => {
  const projectRoot = resolve(process.cwd(), "../");
  const host = new FakePiHost(projectRoot);
  host.getActiveBranchEntries = () => {
    throw new Error("no session context");
  };
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    assert.equal(runtime.getState().startupError, undefined);
    assert.equal(runtime.getState().registeredToolNames.length > 0, true);
  } finally {
    await runtime.stop();
  }
});

test("concurrent restarts serialize the full Lore process lifecycle", async () => {
  const projectRoot = resolve(process.cwd(), "../");
  const client = new LoreClient({
    command: "python3",
    args: [join(projectRoot, "pi-lore/test/fake-lore-mcp.py")],
    env: {},
    cwd: projectRoot,
    startupTimeoutMs: 5_000,
    defaultToolTimeoutMs: 5_000,
    toolTimeoutMs: {},
    summaryTimeoutMs: 5_000,
    maxInlineDiffBytes: 50_000,
    allowToolOverride: false,
    stateDir: join(projectRoot, ".pi/extensions/lore/state-test"),
  });
  await client.start();
  try {
    await Promise.all([client.restart(), client.restart(), client.restart(), client.restart()]);
    const tools = await client.listTools();
    assert.equal(tools.some((tool) => tool.name === "reloadHomeModules"), true);
  } finally {
    await client.stop();
  }
});

test("stop prevents queued calls from restarting Lore", async () => {
  const projectRoot = resolve(process.cwd(), "../");
  const client = new LoreClient({
    command: "python3",
    args: [join(projectRoot, "pi-lore/test/fake-lore-mcp.py")],
    env: {},
    cwd: projectRoot,
    startupTimeoutMs: 5_000,
    defaultToolTimeoutMs: 5_000,
    toolTimeoutMs: { reloadHomeModules: 5_000, echo: 5_000 },
    summaryTimeoutMs: 5_000,
    maxInlineDiffBytes: 50_000,
    allowToolOverride: false,
    stateDir: join(projectRoot, ".pi/extensions/lore/state-test"),
  });
  await client.start();
  const first = client.callStructured("reloadHomeModules", { sleepMs: 1_000 }).then(
    () => "resolved" as const,
    (error: unknown) => error,
  );
  const second = client.callStructured("echo", {}).then(
    () => "resolved" as const,
    (error: unknown) => error,
  );
  await new Promise((resolve) => setTimeout(resolve, 50));
  await client.stop();
  assert.match(String(await first), /stopped|process/i);
  assert.match(String(await second), /stopped|process/i);
});

test("stop wins over queued automatic stale restart", async () => {
  const projectRoot = resolve(process.cwd(), "../");
  const client = new LoreClient({
    command: "python3",
    args: [join(projectRoot, "pi-lore/test/fake-lore-mcp.py")],
    env: {},
    cwd: projectRoot,
    startupTimeoutMs: 5_000,
    defaultToolTimeoutMs: 5_000,
    toolTimeoutMs: {},
    summaryTimeoutMs: 5_000,
    maxInlineDiffBytes: 50_000,
    allowToolOverride: false,
    stateDir: join(projectRoot, ".pi/extensions/lore/state-test"),
  });
  await client.start();
  (client as unknown as { staleAfterFailure: boolean }).staleAfterFailure = true;
  const call = client.listTools().then(
    () => "resolved" as const,
    (error: unknown) => error,
  );
  const stopped = client.stop().then(() => "stopped" as const);
  assert.equal(await stopped, "stopped");
  assert.match(String(await call), /stopped|process/i);
  await assert.rejects(() => client.listTools(), /stopped/i);
});
