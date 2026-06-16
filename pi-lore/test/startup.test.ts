import assert from "node:assert/strict";
import { test } from "node:test";
import { loadLoreConfig } from "../src/config.ts";
import { createLoreExtension } from "../src/index.ts";
import { createStartupCoordinator } from "../src/setup.ts";
import { LoreClient } from "../src/mcp-client.ts";
import { FakePiHost, fakeKeybindings } from "./test-support.ts";
import { join, resolve } from "node:path";
import { mkdtemp, mkdir, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { createHash } from "node:crypto";
import { loadBundledBinaryManifest } from "../src/binary-manifest.ts";
import { loreExtensionStatusText } from "../src/extension-status.ts";
import { setLoreCommand } from "../src/project-config.ts";

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

test("project settings enable Lore and continue into automatic setup", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-project-enable-"));
  await mkdir(join(projectDir, ".pi"), { recursive: true });
  await writeFile(join(projectDir, ".pi/lore.config.json"), JSON.stringify({ enabled: false }), "utf8");
  const choices = ["Not now"];
  const statuses: string[] = [];
  const coordinator = createStartupCoordinator({
    host: { projectDir, setStatus(_key, text) { statuses.push(text); } },
    projectDir,
    runtime: fakeRuntime(),
    binaryOptions: {
      target: { platform: "linux", arch: "x64", libc: "gnu" },
      manifest: { schemaVersion: 1, loreVersion: "0.1.0.0", assets: [] },
      probe: async () => ({ provider: "stack", ghcVersion: "9.12.3" }),
    },
  });
  await coordinator.setProjectEnabled(true, { ui: { select: async () => choices.shift() } });
  assert.equal(coordinator.getState().kind, "skippedForSession");
  assert.equal(statuses.at(-1), loreExtensionStatusText("paused"));
});

test("initial setup can disable Lore for the project", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-initial-disable-"));
  const target = { platform: "linux", arch: "x64", libc: "gnu" } as const;
  const asset = Buffer.from("unused");
  const seenOptions: string[][] = [];
  const coordinator = createStartupCoordinator({
    host: { projectDir },
    projectDir,
    runtime: fakeRuntime(),
    binaryOptions: {
      target,
      manifest: {
        schemaVersion: 1,
        loreVersion: "0.1.0.0",
        assets: [{
          ...target,
          ghcVersion: "9.6.5",
          fileName: "lore-mcp.gz",
          sha256: createHash("sha256").update(asset).digest("hex"),
        }],
      },
      probe: async () => ({ provider: "stack", ghcVersion: "9.6.5" }),
    },
  });

  await coordinator.startAutomatically();
  await coordinator.openSetup({
    ui: {
      select: async (_title, options) => {
        seenOptions.push(options);
        return "Disable Lore for this project";
      },
    },
  });

  assert.equal(seenOptions[0].includes("Disable Lore for this project"), true);
  assert.equal(seenOptions[0].includes("Set command to run Lore"), true);
  assert.equal(coordinator.getState().kind, "disabled");
  assert.equal(loadLoreConfig({ projectDir }).enabled, false);
});

test("setup download failure transitions out of installing", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-download-fail-"));
  const gz = Buffer.from("not used");
  const target = { platform: "linux", arch: "x64", libc: "gnu" } as const;
  const statuses: string[] = [];
  const coordinator = createStartupCoordinator({
    host: { projectDir, setStatus(_key, text) { statuses.push(text); } },
    projectDir,
    runtime: fakeRuntime(),
    binaryOptions: {
      target,
      manifest: { schemaVersion: 1, loreVersion: "0.1.0.0", assets: [{ ...target, ghcVersion: "9.6.5", fileName: "asset.gz", sha256: createHash("sha256").update(gz).digest("hex") }] },
      probe: async () => ({ provider: "stack", ghcVersion: "9.6.5" }),
      download: async () => { throw new Error("network down"); },
    },
  });
  await coordinator.startAutomatically();
  const choices = ["Download", undefined];
  const titles: string[] = [];
  await coordinator.openSetup({ ui: { select: async (title) => { titles.push(title); return choices.shift(); }, notify() {} } });
  assert.equal(coordinator.getState().kind, "failed");
  assert.equal(statuses.includes(loreExtensionStatusText("preparing")), true);
  assert.equal(statuses.includes(loreExtensionStatusText("setupRequired")), true);
  assert.equal(statuses.includes(loreExtensionStatusText("downloading")), true);
  assert.equal(statuses.at(-1), loreExtensionStatusText("unavailable"));
  assert.match(coordinator.statusText(), /network down/);
  assert.match(titles[1] ?? "", /matching Lore binary could not be downloaded or installed/);
});

test("retry after planning failure reruns project planning", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-planning-retry-"));
  let probes = 0;
  const coordinator = createStartupCoordinator({
    host: { projectDir },
    projectDir,
    runtime: fakeRuntime(),
    binaryOptions: {
      target: { platform: "linux", arch: "x64", libc: "gnu" },
      manifest: { schemaVersion: 1, loreVersion: "0.1.0.0", assets: [] },
      probe: async () => {
        probes += 1;
        if (probes === 1) throw new Error("compiler probe failed");
        return { provider: "stack", ghcVersion: "9.12.3" };
      },
    },
  });
  await coordinator.startAutomatically();
  const choices = ["Retry", "Not now"];
  await coordinator.openSetup({ ui: { select: async () => choices.shift(), notify() {} } });
  assert.equal(probes, 2);
  assert.equal(coordinator.getState().kind, "skippedForSession");
});

test("automatic setup keeps setup-required state in the status bar without notifications", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-headless-"));
  const statuses: string[] = [];
  const notices: string[] = [];
  const coordinator = createStartupCoordinator({
    host: { projectDir, setStatus(_key, text) { statuses.push(text); } },
    projectDir,
    runtime: fakeRuntime(),
    binaryOptions: {
      target: { platform: "linux", arch: "x64", libc: "gnu" },
      manifest: { schemaVersion: 1, loreVersion: "0.1.0.0", assets: [] },
      probe: async () => ({ provider: "stack", ghcVersion: "9.12.3" }),
    },
  });
  await coordinator.startAutomatically();
  coordinator.attachContext({ ui: { notify(message: string) { notices.push(message); } } });
  coordinator.attachContext({ ui: { notify(message: string) { notices.push(message); } } });
  await new Promise((resolve) => setTimeout(resolve, 0));
  assert.equal(coordinator.getState().kind, "waitingForSetup");
  assert.equal(statuses.at(-1), loreExtensionStatusText("setupRequired"));
  assert.equal(statuses.includes(loreExtensionStatusText("preparing")), true);
  assert.deepEqual(notices, []);
});

test("startup coordinator keeps active and disabled states in the status bar", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-status-bar-"));
  const activeHost = new FakePiHost(projectDir);
  activeHost.getConfig = () => ({
    command: "lore-mcp",
    args: [],
    cwd: projectDir,
    startupTimeoutMs: 5_000,
    defaultToolTimeoutMs: 5_000,
    toolTimeoutMs: {},
    summaryTimeoutMs: 5_000,
    maxInlineDiffBytes: 50_000,
    stateDir: ".pi/state",
  });
  const activeStatuses: string[] = [];
  activeHost.setStatus = (key, text) => {
    activeHost.statuses.set(key, text);
    activeStatuses.push(text);
  };
  const active = createStartupCoordinator({ host: activeHost, projectDir, runtime: fakeRuntime() });
  await active.startAutomatically();
  assert.deepEqual(activeStatuses, [loreExtensionStatusText("starting"), loreExtensionStatusText("active")]);
  assert.equal(activeHost.statuses.get("lore-extension"), loreExtensionStatusText("active"));
  assert.equal(activeHost.notices.some((notice) => notice.includes("Lore is ready")), false);

  const disabledHost = new FakePiHost(projectDir);
  disabledHost.getConfig = () => ({ enabled: false });
  const disabled = createStartupCoordinator({ host: disabledHost, projectDir, runtime: fakeRuntime() });
  await disabled.startAutomatically();
  assert.equal(disabledHost.statuses.get("lore-extension"), loreExtensionStatusText("disabled"));
});

test("automatic setup opens only for waiting and failed states", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-auto-prompt-"));
  let selects = 0;
  const ctx = { ui: { select: async () => { selects += 1; return "Not now"; }, notify() {} } };

  const ready = createStartupCoordinator({
    host: { projectDir, getConfig: () => ({ command: "python3", args: [], cwd: projectDir, startupTimeoutMs: 5_000, defaultToolTimeoutMs: 5_000, toolTimeoutMs: {}, summaryTimeoutMs: 5_000, maxInlineDiffBytes: 50_000, stateDir: ".pi/state" }) },
    projectDir,
    runtime: fakeRuntime(),
  });
  await ready.startAutomatically();
  ready.attachContext(ctx);

  const disabled = createStartupCoordinator({ host: { projectDir, getConfig: () => ({ enabled: false }) }, projectDir, runtime: fakeRuntime() });
  await disabled.startAutomatically();
  disabled.attachContext(ctx);
  await new Promise((resolve) => setTimeout(resolve, 0));
  assert.equal(selects, 0);

  const waiting = createStartupCoordinator({
    host: { projectDir },
    projectDir,
    runtime: fakeRuntime(),
    binaryOptions: { target: { platform: "linux", arch: "x64", libc: "gnu" }, manifest: { schemaVersion: 1, loreVersion: "0.1.0.0", assets: [] }, probe: async () => ({ provider: "stack", ghcVersion: "9.12.3" }) },
  });
  await waiting.startAutomatically();
  waiting.attachContext(ctx);
  await new Promise((resolve) => setTimeout(resolve, 0));

  const failed = createStartupCoordinator({
    host: { projectDir, getConfig: () => ({ command: "python3", args: [], cwd: projectDir, startupTimeoutMs: 5_000, defaultToolTimeoutMs: 5_000, toolTimeoutMs: {}, summaryTimeoutMs: 5_000, maxInlineDiffBytes: 50_000, stateDir: ".pi/state" }) },
    projectDir,
    runtime: { ...fakeRuntime(), async startResolved() { return { ok: false as const, error: new Error("Exit code 2") }; } },
  });
  await failed.startAutomatically();
  failed.attachContext(ctx);
  await new Promise((resolve) => setTimeout(resolve, 0));
  assert.equal(selects, 2);

  const contextFirstWaiting = createStartupCoordinator({
    host: { projectDir },
    projectDir,
    runtime: fakeRuntime(),
    binaryOptions: { target: { platform: "linux", arch: "x64", libc: "gnu" }, manifest: { schemaVersion: 1, loreVersion: "0.1.0.0", assets: [] }, probe: async () => ({ provider: "stack", ghcVersion: "9.12.3" }) },
  });
  contextFirstWaiting.attachContext(ctx);
  await contextFirstWaiting.startAutomatically();
  assert.equal(selects, 3);

  const contextFirstFailed = createStartupCoordinator({
    host: { projectDir, getConfig: () => ({ command: "python3", args: [], cwd: projectDir, startupTimeoutMs: 5_000, defaultToolTimeoutMs: 5_000, toolTimeoutMs: {}, summaryTimeoutMs: 5_000, maxInlineDiffBytes: 50_000, stateDir: ".pi/state" }) },
    projectDir,
    runtime: { ...fakeRuntime(), async startResolved() { return { ok: false as const, error: new Error("Exit code 2") }; } },
  });
  contextFirstFailed.attachContext(ctx);
  await contextFirstFailed.startAutomatically();
  assert.equal(selects, 4);
});

test("setup deactivates active Lore tools on disable and failed restart", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-deactivate-"));
  let active = ["bash", "getDefinition"];
  let starts = 0;
  const hostConfig = {
    command: "python3",
    args: [],
    cwd: projectDir,
    startupTimeoutMs: 5_000,
    defaultToolTimeoutMs: 5_000,
    toolTimeoutMs: {},
    summaryTimeoutMs: 5_000,
    maxInlineDiffBytes: 50_000,
    stateDir: ".pi/state",
  };
  const coordinator = createStartupCoordinator({
    host: { projectDir, getConfig: () => hostConfig },
    projectDir,
    runtime: {
      ...fakeRuntime(),
      async startResolved() {
        starts += 1;
        return starts === 1 ? { ok: true as const, registeredToolNames: ["getDefinition"] } : { ok: false as const, error: new Error("Exit code 2") };
      },
    },
    activate() { active = [...new Set([...active, "getDefinition"])] ; },
    deactivate() { active = active.filter((name) => name !== "getDefinition"); },
  });
  await coordinator.startAutomatically();
  assert.deepEqual(active, ["bash", "getDefinition"]);
  const failed = await coordinator.restartOrSetup({ ui: { select: async () => undefined } });
  assert.equal(failed.kind, "failed");
  assert.deepEqual(active, ["bash"]);

  active = ["bash", "getDefinition"];
  starts = 0;
  const disableCoordinator = createStartupCoordinator({
    host: { projectDir, getConfig: () => hostConfig },
    projectDir,
    runtime: {
      ...fakeRuntime(),
      async startResolved() { return { ok: true as const, registeredToolNames: ["getDefinition"] }; },
    },
    activate() { active = [...new Set([...active, "getDefinition"])] ; },
    deactivate() { active = active.filter((name) => name !== "getDefinition"); },
  });
  await disableCoordinator.startAutomatically();
  await disableCoordinator.setProjectEnabled(false);
  assert.deepEqual(active, ["bash"]);
});

test("changing the Lore command preserves a disabled project", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-disabled-command-"));
  await mkdir(join(projectDir, ".pi"), { recursive: true });
  await writeFile(join(projectDir, ".pi/lore.config.json"), JSON.stringify({ enabled: false }), "utf8");

  setLoreCommand(projectDir, "lore-mcp");

  const written = JSON.parse(await readFile(join(projectDir, ".pi/lore.config.json"), "utf8"));
  assert.equal(written.enabled, false);
  assert.equal(written.command, "lore-mcp");
  assert.deepEqual(written.args, []);
});

test("manual command persistence preserves PATH commands and resolves executable paths", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-command-args-"));
  await mkdir(join(projectDir, ".pi"), { recursive: true });
  await writeFile(join(projectDir, ".pi/lore.config.json"), JSON.stringify({ command: "python3", args: ["wrapper.py"], tools: { disabled: ["echo"] } }), "utf8");

  setLoreCommand(projectDir, "./bin/lore-mcp");
  let written = JSON.parse(await readFile(join(projectDir, ".pi/lore.config.json"), "utf8"));
  assert.deepEqual(written.args, []);
  assert.equal(written.command, resolve(projectDir, "./bin/lore-mcp"));
  assert.deepEqual(written.tools, { disabled: ["echo"] });

  setLoreCommand(projectDir, " lore-mcp ");
  written = JSON.parse(await readFile(join(projectDir, ".pi/lore.config.json"), "utf8"));
  assert.equal(written.command, "lore-mcp");
  assert.deepEqual(written.args, []);
});

test("setup accepts a lore-mcp command available on PATH", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-path-command-"));
  const target = { platform: "linux", arch: "x64", libc: "gnu" } as const;
  const ghcVersion = "9.12.3";
  let validatedCommand: string | undefined;
  let validationCwd: string | undefined;
  let startedCommand: string | undefined;
  let inputTitle: string | undefined;
  let inputLines: string[] = [];
  const coordinator = createStartupCoordinator({
    host: { projectDir },
    projectDir,
    runtime: {
      ...fakeRuntime(),
      async startResolved(config) {
        startedCommand = config.command;
        return { ok: true as const, registeredToolNames: [] };
      },
    },
    binaryOptions: {
      target,
      manifest: { schemaVersion: 1, loreVersion: loadBundledBinaryManifest().loreVersion, assets: [] },
      probe: async () => ({ provider: "stack", ghcVersion }),
      run: async (command, args, options) => {
        validatedCommand = command;
        validationCwd = options.cwd;
        return {
          command,
          args,
          cwd: options.cwd,
          exitCode: 0,
          signal: null,
          stdout: JSON.stringify({ loreVersion: loadBundledBinaryManifest().loreVersion, ghcVersion, target: "linux-x64-gnu" }),
          stderr: "",
        };
      },
    },
  });
  await coordinator.startAutomatically();
  let customCalls = 0;
  let setupMenuLines: string[] = [];
  await coordinator.openSetup({
    ui: {
      select: async () => { throw new Error("native select should not be used when custom UI is available"); },
      custom: async (factory: (...args: unknown[]) => unknown) => await new Promise<string | undefined>((resolveInput) => {
        customCalls += 1;
        const component = factory(undefined, {
          fg: (role: string, text: string) => `<${role}>${text}</${role}>`,
          bold: (text: string) => `<bold>${text}</bold>`,
        }, fakeKeybindings(), resolveInput) as { render(width: number): string[]; handleInput(data: string): void };
        const lines = component.render(120);
        if (customCalls === 1) {
          setupMenuLines = lines;
          component.handleInput("down");
          component.handleInput("enter");
        } else {
          inputLines = lines;
          inputTitle = inputLines.find((line) => line.includes("Lore command"));
          for (const character of "lore-mcp") component.handleInput(character);
          component.handleInput("enter");
        }
      }),
      notify() {},
    },
  });

  assert.equal(customCalls, 2);
  assert.equal(setupMenuLines.some((line) => line.includes("<accent><bold>") && line.includes("</bold></accent>")), true);
  assert.equal(inputLines.some((line) => line.includes("<accent><bold>Lore command</bold></accent>")), true);
  assert.equal(inputLines.some((line) => line.includes("Enter a command name or executable path")), true);
  assert.equal(setupMenuLines[0].startsWith("<border>"), true);
  assert.equal(inputLines[0].startsWith("<border>"), true);
  assert.match(inputTitle ?? "", /<accent><bold>Lore command<\/bold><\/accent>/);
  assert.equal(validatedCommand, "lore-mcp");
  assert.equal(validationCwd, projectDir);
  assert.equal(startedCommand, "lore-mcp");
  assert.equal(coordinator.getState().kind, "ready");
  const written = JSON.parse(await readFile(join(projectDir, ".pi/lore.config.json"), "utf8"));
  assert.equal(written.command, "lore-mcp");
});

test("startResolved replaces a running binary without duplicate tool registration", async () => {
  const host = new FakePiHost(resolve(process.cwd(), ".."));
  const runtime = await createLoreExtension(host);
  const config = loadLoreConfig(host);
  assert.ok(runtime.startResolved);
  const first = await runtime.startResolved({ ...config, command: "python3" });
  const second = await runtime.startResolved({ ...config, command: "python3" });
  try {
    assert.equal(first.ok, true);
    assert.equal(second.ok, true);
    assert.ok(host.tools.has("getDefinition"));
  } finally {
    await runtime.stop();
  }
});

function fakeRuntime() {
  return {
    async start() {},
    async stop() {},
    async restartLore() {},
    async listAvailableToolNames() { return []; },
    async abandonRecovery() {},
    async processContext(input: { normalizedEntries: unknown[] }) { return input.normalizedEntries as never; },
    async getUsageStats() { throw new Error("unused"); },
    getState() { return { registeredToolNames: [], knowledge: { hashes: [] }, recovery: { phase: "inactive" }, completedRecoveries: [] } as never; },
    async startResolved() { return { ok: true as const, registeredToolNames: [] }; },
  };
}


test("runtime starts an explicitly configured command without managed resolution", async () => {
  const host = new FakePiHost(resolve(process.cwd(), ".."));
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    assert.equal(runtime.getState().startupError, undefined);
    assert.equal(host.statuses.get("lore-extension"), loreExtensionStatusText("active"));
  } finally {
    await runtime.stop();
  }
});

test("runtime without a resolved command does not download automatically", async () => {
  const host = new FakePiHost(process.cwd());
  host.getConfig = () => ({ args: [], cwd: process.cwd(), startupTimeoutMs: 500, defaultToolTimeoutMs: 500, toolTimeoutMs: {}, summaryTimeoutMs: 500, maxInlineDiffBytes: 1000 });
  const runtime = await createLoreExtension(host);
  await runtime.start();
  assert.match(runtime.getState().startupError ?? "", /process configuration is unresolved/);
  await runtime.stop();
});

test("startup coordinator probes the effective Lore project root", async () => {
  const configProjectDir = await mkdtemp(join(tmpdir(), "pi-lore-project-root-"));
  const startupCwd = join(configProjectDir, "workspace");
  const expectedProjectDir = resolve(startupCwd, "../actual-project");
  let seenProjectDir: string | undefined;
  let seenPath: string | undefined;
  const coordinator = createStartupCoordinator({
    host: {
      projectDir: configProjectDir,
      getConfig: () => ({
        cwd: startupCwd,
        env: { LORE_PROJECT_ROOT: "../actual-project", PATH: "/tmp/lore-bin" },
      }),
    },
    projectDir: configProjectDir,
    runtime: fakeRuntime(),
    binaryOptions: {
      target: { platform: "linux", arch: "x64", libc: "gnu" },
      manifest: { schemaVersion: 1, loreVersion: "0.1.0.0", assets: [] },
      probe: async (projectDir, options) => {
        seenProjectDir = projectDir;
        seenPath = options?.env?.PATH;
        return { provider: "stack", ghcVersion: "9.12.3" };
      },
    },
  });
  await coordinator.startAutomatically();
  assert.equal(seenProjectDir, expectedProjectDir);
  assert.equal(seenPath, "/tmp/lore-bin");
});

test("recovery lifecycle methods are available before a process is started", async () => {
  const host = new FakePiHost(process.cwd());
  host.getConfig = () => ({ args: [], cwd: process.cwd(), startupTimeoutMs: 500, defaultToolTimeoutMs: 500, toolTimeoutMs: {}, summaryTimeoutMs: 500, maxInlineDiffBytes: 1000 });
  const runtime = await createLoreExtension(host);
  await runtime.abandonRecovery();
  const entries = [{ id: "one", role: "user", content: "hello" }];
  assert.deepEqual(await runtime.processContext({ rawMessages: [], normalizedEntries: entries }), entries);
});

test("stopped runtime no longer performs stopped-client branch restoration", async () => {
  const host = new FakePiHost(resolve(process.cwd(), ".."));
  const runtime = await createLoreExtension(host);
  await runtime.start();
  await runtime.stop();
  const entries = [{ id: "one", role: "user", content: "hello" }];
  assert.deepEqual(await runtime.processContext({ rawMessages: [], normalizedEntries: entries }), entries);
  await host.emit("session_tree");
  await host.emit("session_compact");
  assert.equal(host.notices.some((notice) => notice.includes("Lore client is stopped")), false);
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
  const startupError = runtime.getState().startupError ?? "";
  assert.equal(status, loreExtensionStatusText("unavailable"));
  assert.match(startupError, /Lore process output:/);
  assert.match(startupError, /lore-mcp failed before initialize/);
  assert.equal(host.notices.some((notice) => notice.includes("Lore extension unavailable")), false);
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
  const startupError = runtime.getState().startupError ?? "";
  assert.equal(status, loreExtensionStatusText("unavailable"));
  assert.match(startupError, /Lore request initialize timed out after 100ms/);
  assert.match(startupError, /Lore process output:/);
  assert.match(startupError, /lore-mcp stuck after error/);
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

for (const cancelKey of ["kitty-escape", "kitty-ctrl+c"]) {
  test(`custom command input returns to setup when cancelled with ${cancelKey}`, async () => {
    const projectDir = await mkdtemp(join(tmpdir(), "pi-lore-command-cancel-"));
    let validationRuns = 0;
    const coordinator = createStartupCoordinator({
      host: { projectDir },
      projectDir,
      runtime: fakeRuntime(),
      binaryOptions: {
        target: { platform: "linux", arch: "x64", libc: "gnu" },
        manifest: { schemaVersion: 1, loreVersion: "0.1.0.0", assets: [] },
        probe: async () => ({ provider: "stack", ghcVersion: "9.12.3" }),
        run: async () => {
          validationRuns += 1;
          throw new Error("validation must not run after cancellation");
        },
      },
    });
    await coordinator.startAutomatically();

    let customCalls = 0;
    const screens: string[][] = [];
    await coordinator.openSetup({
      ui: {
        select: async () => { throw new Error("native select should not be used when custom UI is available"); },
        custom: async <T>(factory: (...args: unknown[]) => unknown) =>
          new Promise<T | undefined>((done) => {
            customCalls += 1;
            const component = factory(
              undefined,
              {
                fg: (role: string, text: string) => `<${role}>${text}</${role}>`,
                bold: (text: string) => `<bold>${text}</bold>`,
              },
              fakeKeybindings({ "tui.select.cancel": [cancelKey] }),
              done,
            ) as { render(width: number): string[]; handleInput(data: string): void };
            screens.push(component.render(100));
            if (customCalls === 1) {
              component.handleInput("down");
              component.handleInput("enter");
            } else {
              component.handleInput(cancelKey);
            }
          }),
        notify() {},
      },
    });

    assert.equal(customCalls, 3);
    assert.equal(screens[0].some((line) => line.includes("Lore needs a custom build")), true);
    assert.equal(screens[1].some((line) => line.includes("Lore command")), true);
    assert.equal(screens[1].some((line) => line.includes("Enter a command name or executable path")), true);
    assert.equal(screens[2].some((line) => line.includes("Lore needs a custom build")), true);
    assert.equal(coordinator.getState().kind, "waitingForSetup");
    assert.equal(validationRuns, 0);
    assert.equal(loadLoreConfig({ projectDir }).command, undefined);
  });
}
