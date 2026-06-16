import assert from "node:assert/strict";
import { test } from "node:test";
import { loadLoreConfig } from "../src/config.ts";
import { createLoreExtension, setManagedLoreBinaryResolverForTests } from "../src/index.ts";
import { LoreClient } from "../src/mcp-client.ts";
import { FakePiHost } from "./test-support.ts";
import { join, resolve } from "node:path";

test("default config enables Lore definition knowledge RPC at the producer", () => {
  const config = loadLoreConfig({ projectDir: process.cwd() });
  assert.equal(config.command, undefined);
  assert.deepEqual(config.args, []);
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


test("explicit command bypasses managed binary resolution", async () => {
  let resolved = false;
  const restore = setManagedLoreBinaryResolverForTests(async () => { resolved = true; throw new Error("should not resolve"); });
  try {
    const host = new FakePiHost(resolve(process.cwd(), ".."));
    const runtime = await createLoreExtension(host);
    await runtime.start();
    try {
      assert.equal(runtime.getState().startupError, undefined);
      assert.equal(resolved, false);
    } finally {
      await runtime.stop();
    }
  } finally {
    restore();
  }
});

test("missing command invokes managed binary resolution and restart reuses it", async () => {
  let calls = 0;
  const restore = setManagedLoreBinaryResolverForTests(async () => { calls++; return "python3"; });
  try {
    const host = new FakePiHost(process.cwd());
    host.getConfig = () => ({
      args: [join(process.cwd(), "test/fake-lore-mcp.py")],
      cwd: process.cwd(),
      startupTimeoutMs: 5_000,
      defaultToolTimeoutMs: 5_000,
      toolTimeoutMs: { reloadHomeModules: 100, runTestSuite: 5_000, echo: 5_000 },
      summaryTimeoutMs: 5_000,
      maxInlineDiffBytes: 50_000,
      stateDir: ".pi/extensions/lore/state-test",
    });
    const runtime = await createLoreExtension(host);
    await runtime.start();
    await runtime.restartLore();
    try {
      assert.equal(runtime.getState().startupError, undefined);
      assert.equal(calls, 1);
    } finally {
      await runtime.stop();
    }
  } finally {
    restore();
  }
});

test("managed resolution failure becomes startup status and stop remains safe", async () => {
  const restore = setManagedLoreBinaryResolverForTests(async () => { throw new Error("no binary"); });
  try {
    const host = new FakePiHost(process.cwd());
    host.getConfig = () => ({ args: [], cwd: process.cwd(), startupTimeoutMs: 500, defaultToolTimeoutMs: 500, toolTimeoutMs: {}, summaryTimeoutMs: 500, maxInlineDiffBytes: 1000 });
    const runtime = await createLoreExtension(host);
    await runtime.start();
    await runtime.stop();
    assert.match(host.statuses.get("lore-extension") ?? "", /no binary/);
  } finally {
    restore();
  }
});


test("managed resolution receives configured environment and LORE_PROJECT_ROOT", async () => {
  let seen: { projectDir: string; env: NodeJS.ProcessEnv; timeoutMs: number } | undefined;
  const restore = setManagedLoreBinaryResolverForTests(async (input) => { seen = { projectDir: input.projectDir, env: input.env, timeoutMs: input.timeoutMs }; throw new Error("stop after resolve input capture"); });
  try {
    const host = new FakePiHost(process.cwd());
    host.getConfig = () => ({
      args: [],
      env: { LORE_PROJECT_ROOT: "../lore-root", PATH: "/tmp/lore-bin" },
      cwd: process.cwd(),
      startupTimeoutMs: 500,
      defaultToolTimeoutMs: 500,
      toolTimeoutMs: {},
      summaryTimeoutMs: 500,
      maxInlineDiffBytes: 1000,
    });
    const runtime = await createLoreExtension(host);
    await runtime.start();
    assert.equal(seen?.projectDir, resolve(process.cwd(), "..", "lore-root"));
    assert.equal(seen?.env.PATH, "/tmp/lore-bin");
    assert.equal(seen?.timeoutMs, 500);
  } finally {
    restore();
  }
});

test("recovery lifecycle methods are available before Lore process resolution", async () => {
  const restore = setManagedLoreBinaryResolverForTests(async () => { throw new Error("not reached"); });
  try {
    const host = new FakePiHost(process.cwd());
    host.getConfig = () => ({ args: [], cwd: process.cwd(), startupTimeoutMs: 500, defaultToolTimeoutMs: 500, toolTimeoutMs: {}, summaryTimeoutMs: 500, maxInlineDiffBytes: 1000 });
    const runtime = await createLoreExtension(host);
    await runtime.abandonRecovery();
    const entries = [{ id: "one", role: "user", content: "hello" }];
    assert.deepEqual(await runtime.processContext({ rawMessages: [], normalizedEntries: entries }), entries);
  } finally {
    restore();
  }
});

test("startup reports unavailable Lore without throwing through Pi", async () => {
  const host = new FakePiHost(process.cwd());
  host.getConfig = () => ({
    command: "python3",
    args: ["-c", "import sys; print('lore-mcp failed before initialize', file=sys.stderr); sys.exit(2)"],
    cwd: process.cwd(),
    startupTimeoutMs: 500,
    defaultToolTimeoutMs: 500,
    toolTimeoutMs: {},
    summaryTimeoutMs: 500,
    maxInlineDiffBytes: 1000,
  });
  const runtime = await createLoreExtension(host);
  await runtime.start();
  await host.emit("session_start");
  const status = host.statuses.get("lore-extension") ?? "";
  assert.match(status, /Lore extension unavailable:/);
  assert.match(status, /Lore process output:/);
  assert.match(status, /lore-mcp failed before initialize/);
  assert.equal(host.notices.some((notice) => notice.includes("Lore client is stopped")), false);
});

test("startup timeout reports captured Lore output", async () => {
  const host = new FakePiHost(process.cwd());
  host.getConfig = () => ({
    command: "python3",
    args: ["-c", "import sys, time; print('lore-mcp stuck after error', file=sys.stderr, flush=True); time.sleep(2)"],
    cwd: process.cwd(),
    startupTimeoutMs: 100,
    defaultToolTimeoutMs: 500,
    toolTimeoutMs: {},
    summaryTimeoutMs: 500,
    maxInlineDiffBytes: 1000,
  });
  const runtime = await createLoreExtension(host);
  await runtime.start();
  const status = host.statuses.get("lore-extension") ?? "";
  assert.match(status, /Lore request initialize timed out after 100ms/);
  assert.match(status, /Lore process output:/);
  assert.match(status, /lore-mcp stuck after error/);
});

test("session events before Lore startup settles do not touch the stopped client", async () => {
  const host = new FakePiHost(process.cwd());
  const runtime = await createLoreExtension(host);

  await host.emit("session_start");
  await runtime.processContext({ rawMessages: [], normalizedEntries: [] });

  assert.equal(host.notices.some((notice) => notice.includes("Lore client is stopped")), false);
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
