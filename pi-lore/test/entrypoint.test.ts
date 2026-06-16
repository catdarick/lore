import assert from "node:assert/strict";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import createLoreExtension, { __test, createLoreExtension as namedCreateLoreExtension } from "../index.ts";
import { fakeKeybindings } from "./test-support.ts";

function fakeSettingsController(overrides: Partial<{
  setProjectEnabled: (enabled: boolean, ctx?: unknown) => Promise<void>;
  configureCommand: (ctx?: unknown) => Promise<"applied" | "cancelled">;
}> = {}) {
  return {
    async setProjectEnabled() {},
    async configureCommand() { return "cancelled" as const; },
    ...overrides,
  };
}

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

test("root adapter starts Lore MCP outside Pi extension startup", async () => {
  let startCalled = false;
  let settled = false;
  const started = new Promise<void>((resolve) => {
    __test.startLoreRuntimeInBackground(
      {
        async start() {
          startCalled = true;
          await new Promise((resume) => setTimeout(resume, 50));
        },
        getState() {
          return { registeredToolNames: [], startupError: undefined } as never;
        },
      },
      {
        onSettled() {
          settled = true;
          resolve();
        },
      },
    );
  });

  assert.equal(startCalled, false);
  assert.equal(settled, false);
  await started;
  assert.equal(startCalled, true);
  assert.equal(settled, true);
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

test("root adapter syncs Lore active tools to coordinator readiness", () => {
  let active = ["bash", "getDefinition"];
  const pi = {
    getActiveTools: () => active,
    setActiveTools(next: string[]) { active = next; },
  };
  __test.syncLoreToolActivation(pi, { getState: () => ({ kind: "disabled" }) }, ["getDefinition"]);
  assert.deepEqual(active, ["bash"]);
  __test.syncLoreToolActivation(pi, { getState: () => ({ kind: "ready" }) }, ["getDefinition"]);
  assert.deepEqual(active, ["bash", "getDefinition"]);
});

test("root adapter derives Lore status tone from coordinator state", () => {
  assert.equal(__test.loreStatusTone({ kind: "ready", command: "lore-mcp", mode: "managed" }, "old error", 0), "info");
  assert.equal(__test.loreStatusTone({ kind: "failed", failure: { kind: "planning", summary: "boom", details: "boom" } }, undefined, 1), "error");
  assert.equal(__test.loreStatusTone({ kind: "disabled" }, undefined, 1), "info");
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
    fakeSettingsController(),
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
  assert.match(notices[0].message, /Effective settings/);
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
    fakeSettingsController(),
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

test("root adapter lore-settings opens project action, command, tools, and recovery entries", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  const projectDir = await mkdtemp(join(tmpdir(), "lore-settings-tools-"));
  let listedFromRuntime = false;
  let nativeSelectCalled = false;
  const screens: string[][] = [];
  let customCalls = 0;
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
    fakeSettingsController(),
    () => {},
  );

  await command?.handler?.("", {
    ui: {
      select: async () => { nativeSelectCalled = true; return undefined; },
      custom: async <T>(factory: (...args: unknown[]) => unknown) =>
        new Promise<T | undefined>((resolve) => {
          customCalls += 1;
          const component = factory(undefined, {
            fg: (role: string, text: string) => `<${role}>${text}</${role}>`,
            bold: (text: string) => `<bold>${text}</bold>`,
          }, fakeKeybindings(), resolve) as { render: (width: number) => string[]; handleInput: (data: string) => void };
          screens.push(component.render(80));
          if (customCalls === 1) {
            component.handleInput("down");
            component.handleInput("down");
            component.handleInput("enter");
          } else if (customCalls === 2) {
            component.handleInput("down");
            component.handleInput(" ");
            component.handleInput("escape");
          } else component.handleInput("escape");
        }),
      notify() {},
    },
  });

  assert.equal(listedFromRuntime, true);
  assert.equal(nativeSelectCalled, false);
  assert.equal(customCalls, 3);
  assert.equal(screens[0].some((line) => line.includes("<accent><bold>Lore settings</bold></accent>")), true);
  assert.equal(screens[0].some((line) => line.includes("Disable Lore for this project")), true);
  assert.equal(screens[0].some((line) => line.includes("Set command to run Lore")), true);
  assert.equal(screens[1].some((line) => line.includes("<accent><bold>Lore tools</bold></accent>")), true);
  assert.equal(screens[0][0].startsWith("<border>"), true);
  assert.equal(screens[1][0].startsWith("<border>"), true);
  assert.equal(screens[1].some((line) => line.includes("<accent>→ [X] getDefinition</accent>")), true);
  assert.equal(screens[1].some((line) => line.includes("<dim> ↑↓ navigate")), true);
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
    fakeSettingsController(),
    () => {},
  );

  let customCalls = 0;
  await command?.handler?.("", {
    ui: {
      select: async () => { throw new Error("native select should not be used when custom UI is available"); },
      custom: async <T>(factory: (...args: unknown[]) => unknown) =>
        new Promise<T | undefined>((resolve) => {
          customCalls += 1;
          const component = factory(undefined, {
            fg: (role: string, text: string) => `<${role}>${text}</${role}>`,
            bold: (text: string) => `<bold>${text}</bold>`,
          }, fakeKeybindings(), resolve) as { handleInput: (data: string) => void };
          if (customCalls === 1) {
            component.handleInput("down");
            component.handleInput("down");
            component.handleInput("down");
            component.handleInput("enter");
          } else if (customCalls === 2) {
            component.handleInput("down");
            component.handleInput("enter");
            component.handleInput("escape");
          } else component.handleInput("escape");
        }),
      notify() {},
    },
  });
  assert.equal(customCalls, 3);

  assert.deepEqual(JSON.parse(readFileSync(join(projectDir, ".pi", "lore.config.json"), "utf8")), {
    recovery: { compilation: true, tests: false },
  });
});



test("root adapter lore-settings disables Lore from the root menu", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  const projectDir = await mkdtemp(join(tmpdir(), "lore-settings-project-"));
  const enabledValues: boolean[] = [];
  __test.registerLoreSettingsCommand(
    {
      registerCommand(_name, options) {
        command = options as typeof command;
      },
    },
    { projectDir },
    { listAvailableToolNames: async () => [] },
    fakeSettingsController({
      async setProjectEnabled(enabled) {
        enabledValues.push(enabled);
      },
    }),
    () => {},
  );

  let customCalls = 0;
  await command?.handler?.("", {
    ui: {
      custom: async <T>(factory: (...args: unknown[]) => unknown) =>
        new Promise<T | undefined>((resolve) => {
          customCalls += 1;
          const component = factory(undefined, {
            fg: (_role: string, text: string) => text,
            bold: (text: string) => text,
          }, fakeKeybindings(), resolve) as { handleInput: (data: string) => void };
          if (customCalls === 1) component.handleInput("enter");
          else component.handleInput("escape");
        }),
    },
  });

  assert.deepEqual(enabledValues, [false]);
  assert.equal(customCalls, 2);
});



test("root adapter lore-settings enables Lore from the root menu", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  const projectDir = await mkdtemp(join(tmpdir(), "lore-settings-enable-"));
  mkdirSync(join(projectDir, ".pi"), { recursive: true });
  writeFileSync(join(projectDir, ".pi", "lore.config.json"), JSON.stringify({ enabled: false }), "utf8");
  const enabledValues: boolean[] = [];
  const screens: string[][] = [];
  __test.registerLoreSettingsCommand(
    {
      registerCommand(_name, options) {
        command = options as typeof command;
      },
    },
    { projectDir },
    { listAvailableToolNames: async () => [] },
    fakeSettingsController({
      async setProjectEnabled(enabled) {
        enabledValues.push(enabled);
      },
    }),
    () => {},
  );

  let customCalls = 0;
  await command?.handler?.("", {
    ui: {
      custom: async <T>(factory: (...args: unknown[]) => unknown) =>
        new Promise<T | undefined>((resolve) => {
          customCalls += 1;
          const component = factory(undefined, {
            fg: (_role: string, text: string) => text,
            bold: (text: string) => text,
          }, fakeKeybindings(), resolve) as { render: (width: number) => string[]; handleInput: (data: string) => void };
          screens.push(component.render(80));
          if (customCalls === 1) component.handleInput("enter");
          else component.handleInput("escape");
        }),
    },
  });

  assert.equal(screens[0].some((line) => line.includes("Enable Lore for this project")), true);
  assert.deepEqual(enabledValues, [true]);
  assert.equal(customCalls, 2);
});

test("root adapter lore-settings opens command configuration from the root menu", async () => {
  let command: { handler?: (args: unknown, ctx: unknown) => Promise<void> } | undefined;
  let commandConfigurations = 0;
  __test.registerLoreSettingsCommand(
    {
      registerCommand(_name, options) {
        command = options as typeof command;
      },
    },
    { projectDir: process.cwd() },
    { listAvailableToolNames: async () => [] },
    fakeSettingsController({
      async configureCommand() {
        commandConfigurations += 1;
        return "cancelled";
      },
    }),
    () => {},
  );

  let customCalls = 0;
  await command?.handler?.("", {
    ui: {
      custom: async <T>(factory: (...args: unknown[]) => unknown) =>
        new Promise<T | undefined>((resolve) => {
          customCalls += 1;
          const component = factory(undefined, {
            fg: (_role: string, text: string) => text,
            bold: (text: string) => text,
          }, fakeKeybindings(), resolve) as { handleInput: (data: string) => void };
          if (customCalls === 1) {
            component.handleInput("down");
            component.handleInput("enter");
          } else component.handleInput("escape");
        }),
    },
  });

  assert.equal(commandConfigurations, 1);
  assert.equal(customCalls, 2);
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
    fakeSettingsController(),
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
