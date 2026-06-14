import assert from "node:assert/strict";
import { test } from "node:test";
import { __test as rootCodec } from "../index.ts";
import { analyzeLoreUsage, projectCompletedRecoveries, updateRecoveryObligations } from "../src/index.ts";
import { planCompletedRecoveryRanges } from "../src/context-projection.ts";
import { renderDiffForContext } from "../src/recovery-summary.ts";
import type { CompletedRecovery, RecoveryState } from "../src/types.ts";

test("recovery obligations remain independent", () => {
  const active: Extract<RecoveryState, { phase: "active" }> = {
    phase: "active",
    recoveryId: "lore-recovery-00000000-0000-4000-8000-000000000001",
    contextMarker: "[[LORE_SECTION_STARTED:lore-recovery-00000000-0000-4000-8000-000000000001]]",
    startValidationToolName: "reloadHomeModules",
    startValidationToolCallId: "call-1",
    startedAt: 1,
    reason: "reloadHomeModules failed",
    baselineId: "lore-recovery-00000000-0000-4000-8000-000000000001",
    compilationPending: true,
    testsPending: true,
  };
  const afterTestSuccess = updateRecoveryObligations(active, {
    kind: "semantic",
    toolName: "runTestSuite",
    success: true,
    structuredContent: { success: true },
  });
  assert.equal(afterTestSuccess.compilationPending, false);
  assert.equal(afterTestSuccess.testsPending, false);

  const afterReloadSuccess = updateRecoveryObligations(afterTestSuccess, {
    kind: "semantic",
    toolName: "reloadHomeModules",
    success: true,
    structuredContent: { success: true },
  });
  assert.equal(afterReloadSuccess.compilationPending, false);
  assert.equal(afterReloadSuccess.testsPending, false);
});

test("completed recovery projection preserves prefix and later messages", () => {
  const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000002";
  const completed: CompletedRecovery = {
    recoveryId,
    startEntryId: "a2",
    startValidationToolCallId: "call-failed",
    finalValidationToolCallId: "call-fixed",
    summary: "summary",
    contextReplacement: "summary\nbounded diff",
    diff: {
      reliable: true,
      changedPaths: ["A.hs"],
      stats: { filesChanged: 1, additions: 1, deletions: 0 },
      inlinePatch: "patch",
      truncated: false,
    },
    completedAt: 10,
  };
  const entries = [
    { id: "u1", role: "user", content: "request" },
    { id: "a2", role: "assistant", content: [{ type: "toolCall", id: "call-failed", name: "reloadHomeModules", arguments: {} }] },
    {
      id: "m3",
      role: "system",
      content: "──────── LORE RECOVERY STARTED ────────",
      details: { loreExtension: { kind: "uiMarker", marker: "recovery-start", recoveryId } },
    },
    { id: "a4", role: "assistant", content: [{ type: "toolCall", id: "call-fixed", name: "reloadHomeModules", arguments: {} }] },
    { id: "t5", role: "toolResult", toolCallId: "call-fixed", content: [{ type: "text", text: "reload ok" }] },
    { id: "u5", role: "user", content: "later" },
  ];
  const projected = projectCompletedRecoveries(entries, [completed]);
  assert.deepEqual(
    projected.map((entry) => entry.id),
    ["u1", `lore-projection-${recoveryId}`, "a4", "t5", "u5"],
  );
  assert.equal(projected[1].content, "summary\nbounded diff");
});

test("completed recovery projection falls back to validation toolCallIds", () => {
  const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000003";
  const completed: CompletedRecovery = {
    recoveryId,
    startValidationToolCallId: "call-failed",
    finalValidationToolCallId: "call-fixed",
    summary: "summary",
    contextReplacement: "compressed",
    diff: {
      reliable: true,
      changedPaths: [],
      stats: { filesChanged: 0, additions: 0, deletions: 0 },
      truncated: false,
    },
    completedAt: 11,
  };
  const entries = [
    { role: "user", content: "request" },
    {
      role: "assistant",
      content: [{ type: "toolCall", id: "call-failed", name: "reloadHomeModules", arguments: {} }],
    },
    { role: "toolResult", toolCallId: "call-failed", content: [{ type: "text", text: "reload failed" }] },
    { role: "assistant", content: "investigating" },
    {
      role: "assistant",
      content: [{ type: "toolCall", id: "call-fixed", name: "reloadHomeModules", arguments: {} }],
    },
    { role: "toolResult", toolCallId: "call-fixed", content: [{ type: "text", text: "reload ok" }] },
    {
      role: "custom",
      content: "──────── LORE RECOVERY COMPLETED ────────",
      details: { loreExtension: { kind: "uiMarker", marker: "recovery-complete", recoveryId } },
    },
    { role: "user", content: "later" },
  ];
  const projected = projectCompletedRecoveries(entries, [completed]);
  assert.equal(projected.length, 5);
  assert.equal(projected[1].content, "compressed");
  assert.equal(projected[2].role, "assistant");
  assert.equal(projected[3].toolCallId, "call-fixed");
});

test("completed recovery with missing start boundary does not replace unrelated prefix", () => {
  const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000014";
  const completed: CompletedRecovery = {
    recoveryId,
    startValidationToolCallId: "missing-failed-call",
    finalValidationToolCallId: "call-fixed",
    summary: "summary",
    contextReplacement: "compressed",
    diff: {
      reliable: true,
      changedPaths: [],
      stats: { filesChanged: 0, additions: 0, deletions: 0 },
      truncated: false,
    },
    completedAt: 14,
  };
  const entries = [
    { id: "u1", role: "user", content: "unrelated old user message" },
    { id: "a2", role: "assistant", content: "unrelated old assistant response" },
    { id: "a3", role: "assistant", content: [{ type: "toolCall", id: "call-fixed", name: "reloadHomeModules" }] },
    {
      id: "t4",
      role: "toolResult",
      toolCallId: "call-fixed",
      toolName: "reloadHomeModules",
      content: [{ type: "text", text: "reload ok" }],
    },
  ];

  const plan = planCompletedRecoveryRanges(entries, [completed]);
  assert.equal(plan.ranges.length, 0);
  assert.deepEqual(plan.unresolvedRecoveryIds, [recoveryId]);
  assert.match(plan.warnings.join("\n"), /could not be resolved/);

  const projected = projectCompletedRecoveries(entries, [completed]);
  assert.deepEqual(projected, entries);

  const stats = analyzeLoreUsage({
    entries,
    completedRecoveries: [completed],
    registeredToolNames: ["reloadHomeModules"],
  });
  assert.equal(stats.totals.main.calls, 1);
  assert.equal(stats.totals.summarizedRecovery.calls, 0);
  assert.match(stats.warnings.join("\n"), /could not be resolved/);
});

test("hidden visible recovery summary is excluded from projected model context", () => {
  const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000004";
  const contextReplacement = "compressed recovery context";
  const completed: CompletedRecovery = {
    recoveryId,
    startValidationToolCallId: "call-failed",
    finalValidationToolCallId: "call-fixed",
    summary: "summary",
    contextReplacement,
    diff: {
      reliable: true,
      changedPaths: [],
      stats: { filesChanged: 0, additions: 0, deletions: 0 },
      truncated: false,
    },
    completedAt: 12,
  };
  const rawMessages = [
    { role: "user", content: "request" },
    { role: "assistant", content: [{ type: "toolCall", id: "call-failed" }] },
    { role: "toolResult", toolCallId: "call-failed", content: "failed" },
    { role: "assistant", content: [{ type: "toolCall", id: "call-fixed" }] },
    { role: "toolResult", toolCallId: "call-fixed", content: "ok" },
    {
      role: "custom",
      customType: "lore-recovery-summary",
      content: contextReplacement,
      display: true,
      details: { recoveryId, hiddenFromModel: true },
    },
    { role: "user", content: "later" },
  ];
  const projected = projectCompletedRecoveries(rootCodec.normalizePiMessages(rawMessages), [completed]);
  const occurrences = projected.filter((entry) => entry.content === contextReplacement);
  assert.equal(occurrences.length, 1);
  assert.equal(projected.some((entry) => entry.customType === "lore-recovery-summary"), false);
  assert.equal(rawMessages.some((message) => (message as { customType?: string }).customType === "lore-recovery-summary"), true);
  assert.equal(
    rootCodec.toLlmMessages(rawMessages).some((message) => JSON.stringify(message).includes(contextReplacement)),
    false,
  );
});

test("diff context bounds changed path list by characters", async () => {
  const text = await renderDiffForContext({
    reliable: true,
    changedPaths: Array.from({ length: 2_000 }, (_, index) => `generated/path/${index.toString().padStart(4, "0")}.txt`),
    stats: { filesChanged: 2_000, additions: 1, deletions: 1 },
    inlinePatch: "patch",
    truncated: false,
  });
  assert.match(text, /additional paths omitted/);
  assert.ok(text.length < 12_000);
});

test("truncated diff without patch artifact does not claim full patch retention", async () => {
  const text = await renderDiffForContext({
    reliable: false,
    reason: "diff command failed",
    changedPaths: ["A.txt"],
    stats: { filesChanged: 1, additions: 0, deletions: 0 },
    inlinePatch: "partial",
    truncated: true,
  });
  assert.doesNotMatch(text, /Full patch was retained/);
  assert.match(text, /artifact is unavailable/);
});
