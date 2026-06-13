import assert from "node:assert/strict";
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

function countOccurrences(text: string, pattern: string): number {
  return text.split(pattern).length - 1;
}
