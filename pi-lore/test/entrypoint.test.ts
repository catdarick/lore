import assert from "node:assert/strict";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import createLoreExtension, { __test, createLoreExtension as namedCreateLoreExtension } from "../index.ts";

test("root extension entrypoint exports the Pi extension factory", async () => {
  assert.equal(typeof createLoreExtension, "function");
  assert.equal(typeof namedCreateLoreExtension, "function");
});

test("root adapter rejects missing required Pi capabilities", async () => {
  await assert.rejects(
    () => createLoreExtension({ on() {} } as never),
    /requires Pi capabilities: registerTool, appendEntry, sendMessage/,
  );
});

test("root adapter appends Lore marker guidance to the system prompt", () => {
  let beforeAgentStart: ((event: unknown) => unknown) | undefined;
  __test.registerLoreSystemPromptGuidance({
    on(event, handler) {
      if (event === "before_agent_start") {
        beforeAgentStart = handler;
      }
    },
  });

  assert.equal(typeof beforeAgentStart, "function");
  const result = beforeAgentStart?.({ systemPrompt: "base prompt" }) as { systemPrompt?: string };
  assert.ok(result.systemPrompt);
  assert.equal(result.systemPrompt.startsWith("base prompt\n\n"), true);
  assert.equal(countOccurrences(result.systemPrompt, "base prompt"), 1);
  assert.equal(countOccurrences(result.systemPrompt, "[[LORE_SECTION_STARTED:<id>]]"), 1);
  assert.equal(countOccurrences(result.systemPrompt, "[[LORE_FIXES_APPLIED]]"), 1);
  assert.equal(__test.appendLoreContextMarkerGuidance({})?.systemPrompt, undefined);
});

test("root adapter computes active Lore status from Pi active tools", () => {
  assert.deepEqual(
    __test.currentActiveLoreToolNames(
      { getActiveTools: () => ["bash", "getDefinition"] },
      ["getDefinition", "runTestSuite"],
    ),
    ["getDefinition"],
  );
  assert.deepEqual(
    __test.currentActiveLoreToolNames({}, ["getDefinition", "runTestSuite"]),
    ["getDefinition", "runTestSuite"],
  );
});

test("root adapter lore-stats command emits hidden immediate display without waiting for idle", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  let waited = false;
  let statsReadBeforeWait = false;
  let displayMessage: unknown;
  let currentContextSeen = false;

  __test.registerUsageStatsCommand(
    {
      registerCommand(name, options) {
        if (name === "lore-stats") {
          command = options as typeof command;
        }
      },
    },
    {
      async getUsageStats() {
        statsReadBeforeWait = !waited;
        return {
          tools: [],
          totals: {
            main: { calls: 0, tokens: 0 },
            summarizedRecovery: { calls: 0, tokens: 0 },
          },
          recovery: {
            completedRecoveries: 0,
            originalTokensSummarized: 0,
            summaryReplacementTokens: 0,
            estimatedReductionTokens: 0,
            missingMetricsRecoveries: 0,
          },
          warnings: [],
          estimated: true,
        };
      },
    },
    {
      async appendDisplayMessageImmediately(message) {
        displayMessage = message;
        return "appended";
      },
    },
    (ctx) => {
      currentContextSeen = Boolean(ctx);
    },
  );

  assert.equal(typeof command?.handler, "function");
  await command?.handler?.(undefined, {
    waitForIdle() {
      waited = true;
    },
    sessionManager: {
      getBranch: () => [],
      getLeafId: () => null,
    },
    ui: {
      notify() {},
      setStatus() {},
    },
  });

  assert.equal(waited, false);
  assert.equal(statsReadBeforeWait, true);
  assert.equal(currentContextSeen, true);
  assert.equal((displayMessage as { customType?: unknown }).customType, "lore-usage-stats");
  assert.equal((displayMessage as { display?: unknown }).display, true);
  assert.equal(((displayMessage as { details?: { hiddenFromModel?: unknown } }).details)?.hiddenFromModel, true);
});

test("root adapter lore-stats command requires immediate display support", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  __test.registerUsageStatsCommand(
    {
      registerCommand(_name, options) {
        command = options as typeof command;
      },
    },
    {
      async getUsageStats() {
        return {
          tools: [],
          totals: {
            main: { calls: 0, tokens: 0 },
            summarizedRecovery: { calls: 0, tokens: 0 },
          },
          recovery: {
            completedRecoveries: 0,
            originalTokensSummarized: 0,
            summaryReplacementTokens: 0,
            estimatedReductionTokens: 0,
            missingMetricsRecoveries: 0,
          },
          warnings: [],
          estimated: true,
        };
      },
    },
    {},
    () => {},
  );

  await assert.rejects(
    () => command?.handler?.(undefined, {}) ?? Promise.resolve(),
    /Lore statistics require immediate display support/,
  );
});

test("root adapter lore-settings command shows effective settings", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  const notices: Array<{ message: string; type?: string }> = [];

  __test.registerLoreSettingsCommand(
    {
      registerCommand(name, options) {
        if (name === "lore-settings") {
          command = options as typeof command;
        }
      },
    },
    { projectDir: process.cwd() },
    { listAvailableToolNames: async () => [] },
    () => {},
  );

  await command?.handler?.("show", {
    ui: {
      notify(message: string, type?: string) {
        notices.push({ message, type });
      },
    },
  });

  assert.equal(notices.length, 1);
  assert.equal(notices[0].type, "info");
  assert.match(notices[0].message, /Effective tool\/recovery settings/);
  assert.match(notices[0].message, /"recovery"/);
});

test("root adapter lore-settings command writes project config patches", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  const projectDir = await mkdtemp(join(tmpdir(), "lore-settings-command-"));
  const notices: Array<{ message: string; type?: string }> = [];
  const configPath = join(projectDir, ".pi", "lore.config.json");
  mkdirSync(join(projectDir, ".pi"), { recursive: true });
  writeFileSync(configPath, JSON.stringify({ tools: { enabled: ["stale"] } }), "utf8");

  __test.registerLoreSettingsCommand(
    {
      registerCommand(_name, options) {
        command = options as typeof command;
      },
    },
    { projectDir },
    { listAvailableToolNames: async () => [] },
    () => {},
  );

  await command?.handler?.('set {"recovery":{"tests":false},"tools":{"disabled":["runTestSuite"]}}', {
    ui: {
      notify(message: string, type?: string) {
        notices.push({ message, type });
      },
    },
  });

  assert.equal(existsSync(configPath), true);
  assert.deepEqual(JSON.parse(readFileSync(configPath, "utf8")), {
    recovery: { tests: false },
    tools: { disabled: ["runTestSuite"] },
  });
  assert.match(notices[0].message, /Run \/reload to apply tool registration changes/);
});

test("root adapter lore-settings opens a two-item menu and tool checkboxes from Lore MCP", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  const projectDir = await mkdtemp(join(tmpdir(), "lore-settings-tools-"));
  let listedFromRuntime = false;
  const menuOptionsSeen: string[][] = [];
  let activeTools = ["bash", "getDefinition", "runTestSuite"];

  __test.registerLoreSettingsCommand(
    {
      getActiveTools: () => activeTools,
      setActiveTools: (next: string[]) => {
        activeTools = next;
      },
      registerCommand(_name, options) {
        command = options as typeof command;
      },
    },
    { projectDir },
    {
      async listAvailableToolNames() {
        listedFromRuntime = true;
        return ["getDefinition", "runTestSuite"];
      },
    },
    () => {},
  );

  const selections = ["Tools", undefined];
  await command?.handler?.("", {
    ui: {
      select: async (_title: string, options: string[]) => {
        menuOptionsSeen.push(options);
        return selections.shift();
      },
      custom: async <T>(factory: (...args: unknown[]) => unknown) =>
        new Promise<T | undefined>((resolve) => {
          const component = factory(undefined, undefined, undefined, resolve) as { handleInput: (data: string) => void };
          component.handleInput("\u001b[B");
          component.handleInput(" ");
          component.handleInput("\u001b");
        }),
      notify() {},
    },
  });

  assert.equal(listedFromRuntime, true);
  assert.deepEqual(menuOptionsSeen[0], ["Tools", "Recovery"]);
  assert.deepEqual(JSON.parse(readFileSync(join(projectDir, ".pi", "lore.config.json"), "utf8")), {
    tools: { disabled: ["runTestSuite"] },
  });
  assert.deepEqual(activeTools.sort(), ["bash", "getDefinition"]);
});

test("root adapter lore-settings recovery checkboxes disable test recovery", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  const projectDir = await mkdtemp(join(tmpdir(), "lore-settings-recovery-"));

  __test.registerLoreSettingsCommand(
    {
      registerCommand(_name, options) {
        command = options as typeof command;
      },
    },
    { projectDir },
    { listAvailableToolNames: async () => [] },
    () => {},
  );

  const selections = ["Recovery", undefined];
  await command?.handler?.("", {
    ui: {
      select: async () => selections.shift(),
      custom: async <T>(factory: (...args: unknown[]) => unknown) =>
        new Promise<T | undefined>((resolve) => {
          const component = factory(undefined, undefined, undefined, resolve) as { handleInput: (data: string) => void };
          component.handleInput("\u001b[B");
          component.handleInput("\n");
          component.handleInput("\u001b");
        }),
      notify() {},
    },
  });

  assert.deepEqual(JSON.parse(readFileSync(join(projectDir, ".pi", "lore.config.json"), "utf8")), {
    recovery: { compilation: true, tests: false },
  });
});

test("root adapter lore-settings rejects unsupported config keys", () => {
  assert.throws(() => __test.parseLoreSettingsPatch('{"command":"evil"}'), /Unsupported \/lore-settings keys: command/);
  assert.throws(
    () => __test.parseLoreSettingsPatch('{"tools":{"enabled":["getDefinition"]}}'),
    /Unsupported \/lore-settings tools keys: enabled/,
  );
});

test("root adapter lore-settings validates patches before writing", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  const projectDir = await mkdtemp(join(tmpdir(), "lore-settings-invalid-"));

  __test.registerLoreSettingsCommand(
    {
      registerCommand(_name, options) {
        command = options as typeof command;
      },
    },
    { projectDir },
    { listAvailableToolNames: async () => [] },
    () => {},
  );

  await assert.rejects(
    () => command?.handler?.('set {"recovery":{"tests":"no"}}', {}) ?? Promise.resolve(),
    /recovery\.tests must be a boolean/,
  );
  assert.equal(existsSync(join(projectDir, ".pi", "lore.config.json")), false);
});

test("root adapter lore-restart command restarts the Lore binary", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  let restarted = false;
  let currentContextSeen = false;
  const notices: Array<{ message: string; type?: string }> = [];

  __test.registerLoreRestartCommand(
    {
      registerCommand(name, options) {
        if (name === "lore-restart") {
          command = options as typeof command;
        }
      },
    },
    {
      async restartLore() {
        restarted = true;
      },
    },
    (ctx) => {
      currentContextSeen = Boolean(ctx);
    },
  );

  assert.equal(typeof command?.handler, "function");
  await command?.handler?.(undefined, {
    sessionManager: {
      getBranch: () => [],
      getLeafId: () => null,
    },
    ui: {
      notify(message: string, type?: string) {
        notices.push({ message, type });
      },
      setStatus() {},
    },
  });

  assert.equal(restarted, true);
  assert.equal(currentContextSeen, true);
  assert.deepEqual(notices, [
    { message: "Restarting Lore MCP binary...", type: "info" },
    { message: "Lore MCP binary restarted.", type: "info" },
  ]);
});

test("root adapter lore-restart command reports restart failures", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  const notices: Array<{ message: string; type?: string }> = [];

  __test.registerLoreRestartCommand(
    {
      registerCommand(_name, options) {
        command = options as typeof command;
      },
    },
    {
      async restartLore() {
        throw new Error("boom");
      },
    },
    () => {},
  );

  await assert.rejects(
    () =>
      command?.handler?.(undefined, {
        ui: {
          notify(message: string, type?: string) {
            notices.push({ message, type });
          },
        },
      }) ?? Promise.resolve(),
    /boom/,
  );

  assert.deepEqual(notices, [
    { message: "Restarting Lore MCP binary...", type: "info" },
    { message: "Lore MCP binary restart failed: boom", type: "error" },
  ]);
});

function countOccurrences(text: string, pattern: string): number {
  return text.split(pattern).length - 1;
}
