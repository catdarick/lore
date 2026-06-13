import assert from "node:assert/strict";
import { test } from "node:test";
import { analyzeLoreUsage, formatLoreUsageStats } from "../src/usage-stats.ts";
import {
  estimateLoreToolResultTokens,
  loreToolResultContextText,
  loreToolResultDisplayText,
} from "../src/tool-result-text.ts";
import type { CompletedRecovery, PiEntry } from "../src/types.ts";

test("analyzes Lore tool results by main and recovery-summarized buckets", () => {
  const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000101";
  const entries: PiEntry[] = [
    { id: "u1", role: "user", content: "request" },
    assistantCall("a2", "call-failed", "reloadHomeModules"),
    toolResult("t3", "call-failed", "reloadHomeModules", "failed reload"),
    assistantCall("a4", "call-def", "getDefinition"),
    toolResult("t5", "call-def", "getDefinition", "definition text"),
    assistantCall("a6", "call-other", "notLore"),
    toolResult("t7", "call-other", "notLore", "ignored"),
    assistantCall("a8", "call-fixed", "reloadHomeModules"),
    toolResult("t9", "call-fixed", "reloadHomeModules", "reload ok"),
    toolResult("t10", "legacy-call", "historicalTool", "old lore", { lore: { structuredContent: {} } }),
  ];
  const completed = completedRecovery(recoveryId, "call-failed", "call-fixed", {
    originalRecoveryTokens: 100,
    summaryReplacementTokens: 10,
    estimated: true,
  });

  const stats = analyzeLoreUsage({
    entries,
    completedRecoveries: [completed],
    registeredToolNames: ["reloadHomeModules", "getDefinition"],
  });

  const reload = stats.tools.find((tool) => tool.toolName === "reloadHomeModules");
  assert.equal(reload?.summarizedRecovery.calls, 1);
  assert.equal(reload?.main.calls, 1);
  const definition = stats.tools.find((tool) => tool.toolName === "getDefinition");
  assert.equal(definition?.summarizedRecovery.calls, 1);
  assert.equal(definition?.main.calls, 0);
  const historical = stats.tools.find((tool) => tool.toolName === "historicalTool");
  assert.equal(historical?.main.calls, 1);
  assert.equal(stats.tools.some((tool) => tool.toolName === "notLore"), false);
  assert.equal(stats.recovery.originalTokensSummarized, 100);
  assert.equal(stats.recovery.summaryReplacementTokens, 10);
  assert.equal(stats.recovery.estimatedReductionTokens, 90);
});

test("final successful validation remains in main bucket", () => {
  const entries: PiEntry[] = [
    assistantCall("a1", "call-failed", "reloadHomeModules"),
    toolResult("t2", "call-failed", "reloadHomeModules", "failed"),
    assistantCall("a3", "call-fixed", "reloadHomeModules"),
    toolResult("t4", "call-fixed", "reloadHomeModules", "ok"),
  ];
  const stats = analyzeLoreUsage({
    entries,
    completedRecoveries: [completedRecovery("r1", "call-failed", "call-fixed")],
    registeredToolNames: ["reloadHomeModules"],
  });
  const reload = stats.tools.find((tool) => tool.toolName === "reloadHomeModules");
  assert.equal(reload?.summarizedRecovery.calls, 1);
  assert.equal(reload?.main.calls, 1);
});

test("active or fork-before-completion branches keep calls in main bucket", () => {
  const entries: PiEntry[] = [
    assistantCall("a1", "call-failed", "reloadHomeModules"),
    toolResult("t2", "call-failed", "reloadHomeModules", "failed"),
  ];
  const stats = analyzeLoreUsage({
    entries,
    completedRecoveries: [],
    registeredToolNames: ["reloadHomeModules"],
  });
  assert.equal(stats.totals.main.calls, 1);
  assert.equal(stats.totals.summarizedRecovery.calls, 0);
});

test("malformed recovery ranges warn and missing legacy metrics are not counted as zero", () => {
  const stats = analyzeLoreUsage({
    entries: [toolResult("t1", "missing-call", "reloadHomeModules", "failed")],
    completedRecoveries: [completedRecovery("bad-recovery", "missing-start", "missing-end")],
    registeredToolNames: ["reloadHomeModules"],
  });
  assert.equal(stats.recovery.completedRecoveries, 1);
  assert.equal(stats.recovery.missingMetricsRecoveries, 1);
  assert.match(stats.warnings.join("\n"), /could not be resolved/);
  assert.match(stats.warnings.join("\n"), /Token metrics unavailable/);
});

test("unmatched non-Lore tool results do not warn", () => {
  const stats = analyzeLoreUsage({
    entries: [{ id: "t1", role: "toolResult", toolCallId: "unknown", content: [{ type: "text", text: "noise" }] }],
    completedRecoveries: [],
    registeredToolNames: ["reloadHomeModules"],
  });
  assert.equal(stats.warnings.length, 0);
  assert.equal(stats.totals.main.calls, 0);
});

test("unmatched Lore metadata tool results warn", () => {
  const stats = analyzeLoreUsage({
    entries: [
      {
        id: "t1",
        role: "toolResult",
        toolCallId: "unknown",
        content: [{ type: "text", text: "legacy lore" }],
        details: { lore: {} },
      },
    ],
    completedRecoveries: [],
    registeredToolNames: ["reloadHomeModules"],
  });
  const unknown = stats.tools.find((tool) => tool.toolName === "unknown Lore tool");
  assert.equal(unknown?.main.calls, 1);
  assert.match(stats.warnings.join("\n"), /Lore tool result could not be matched/);
});

test("result text helpers count string content and context blocks while deduplicating display text", () => {
  assert.equal(estimateLoreToolResultTokens("plain result"), Math.ceil("plain result".length / 4));
  const content = [
    { type: "text", text: "alpha" },
    { type: "text", text: "alpha" },
    { type: "text", text: "beta gamma" },
  ];
  assert.equal(loreToolResultContextText(content), "alpha\n\nalpha\n\nbeta gamma");
  assert.equal(loreToolResultDisplayText(content), "alpha\n\nbeta gamma");
  assert.equal(estimateLoreToolResultTokens(content), Math.ceil("alpha\n\nalpha\n\nbeta gamma".length / 4));
});

test("formats a stats report with limitations", () => {
  const stats = analyzeLoreUsage({
    entries: [toolResult("t1", "call-1", "getDefinition", "abcdefghi")],
    completedRecoveries: [],
    registeredToolNames: ["getDefinition"],
  });
  const report = formatLoreUsageStats(stats);
  assert.match(report, /Lore tool-result context statistics \(estimated, ~4 characters\/token\)/);
  assert.match(report, /Main \/ not Lore-summarized/);
  assert.match(report, /active branch only/);
  assert.match(report, /provider billing/);
});

function assistantCall(id: string, callId: string, name: string): PiEntry {
  return { id, role: "assistant", content: [{ type: "toolCall", id: callId, name, arguments: {} }] };
}

function toolResult(id: string, callId: string, name: string, text: string, details?: unknown): PiEntry {
  return {
    id,
    role: "toolResult",
    toolCallId: callId,
    toolName: name,
    content: [{ type: "text", text }],
    details,
  };
}

function completedRecovery(
  recoveryId: string,
  startValidationToolCallId: string,
  finalValidationToolCallId: string,
  tokenMetrics?: CompletedRecovery["tokenMetrics"],
): CompletedRecovery {
  return {
    recoveryId,
    startValidationToolCallId,
    finalValidationToolCallId,
    summary: "summary",
    contextReplacement: "summary",
    diff: {
      reliable: true,
      changedPaths: [],
      stats: { filesChanged: 0, additions: 0, deletions: 0 },
      truncated: false,
    },
    tokenMetrics,
    completedAt: 1,
  };
}
