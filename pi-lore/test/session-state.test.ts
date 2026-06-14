import assert from "node:assert/strict";
import { test } from "node:test";
import { SessionStateStore, foldSessionEntries, type SessionLog } from "../src/session-state.ts";
import type { LoreSessionEvent, PiEntry } from "../src/types.ts";

const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000010";

test("append failure leaves in-memory state unchanged", async () => {
  const log: SessionLog = {
    async append() {
      throw new Error("append failed");
    },
    async readActiveBranch() {
      return [];
    },
  };
  const store = new SessionStateStore(log);
  await assert.rejects(() => store.persist(activeRecoveryEvent()), /append failed/);
  assert.equal(store.current().recovery.phase, "inactive");
});

test("concurrent persists apply in append order", async () => {
  const events: LoreSessionEvent[] = [];
  const log: SessionLog = {
    async append(event) {
      events.push(event);
    },
    async readActiveBranch() {
      return [];
    },
  };
  const store = new SessionStateStore(log);
  await Promise.all([
    store.persist(activeRecoveryEvent()),
    store.persist({ kind: "recoveryAbandoned", recoveryId, state: { phase: "inactive" }, abandonedAt: 2 }),
  ]);
  assert.deepEqual(events.map((event) => event.kind), ["recoveryState", "recoveryAbandoned"]);
  assert.equal(store.current().recovery.phase, "inactive");
});

test("reload cannot overwrite a concurrently committed event", async () => {
  const entries: PiEntry[] = [];
  const log: SessionLog = {
    async append(event) {
      entries.push(entryFor(event));
    },
    async readActiveBranch() {
      await new Promise((resolve) => setTimeout(resolve, 10));
      return entries;
    },
  };
  const store = new SessionStateStore(log);
  await Promise.all([store.reloadFromBranch(), store.persist(activeRecoveryEvent())]);
  assert.equal(store.current().recovery.phase, "active");
});

test("malformed known session events are ignored and legacy mode state is readable", () => {
  const state = foldSessionEntries([
    { details: { loreExtension: { kind: "recoveryState", state: { phase: "active", recoveryId: "../bad" } } } },
    {
      details: {
        loreExtension: {
          kind: "recoveryState",
          state: {
            mode: "active",
            recoveryId,
            contextMarker: `[[LORE_SECTION_STARTED:${recoveryId}]]`,
            startValidationToolName: "reloadHomeModules",
            startValidationToolCallId: "call-1",
            startedAt: 1,
            reason: "reloadHomeModules failed",
            baselineId: recoveryId,
            compilationPending: true,
            testsPending: false,
          },
        },
      },
    },
  ]);
  assert.equal(state.recovery.phase, "active");
});

test("ready-to-finalize recovery with empty legacy validation text remains readable", () => {
  const state = foldSessionEntries([
    {
      details: {
        loreExtension: {
          kind: "recoveryState",
          state: {
            phase: "readyToFinalize",
            recoveryId,
            contextMarker: `[[LORE_SECTION_STARTED:${recoveryId}]]`,
            startValidationToolName: "reloadHomeModules",
            startValidationToolCallId: "call-1",
            finalValidationToolCallId: "call-2",
            finalValidationText: "",
            startedAt: 1,
            reason: "reloadHomeModules failed",
            baselineId: recoveryId,
            compilationPending: false,
            testsPending: false,
          },
        },
      },
    },
  ]);
  assert.equal(state.recovery.phase, "readyToFinalize");
});

test("legacy recovery start marker remains readable from persisted state", () => {
  const state = foldSessionEntries([
    {
      details: {
        loreExtension: {
          kind: "recoveryState",
          state: {
            phase: "active",
            recoveryId,
            contextMarker: `[[LORE_RECOVERY_START:${recoveryId}]]`,
            startValidationToolName: "reloadHomeModules",
            startValidationToolCallId: "call-1",
            startedAt: 1,
            reason: "reloadHomeModules failed",
            baselineId: recoveryId,
            compilationPending: true,
            testsPending: false,
          },
        },
      },
    },
  ]);
  assert.equal(state.recovery.phase, "active");
  assert.equal(
    state.recovery.phase === "active" ? state.recovery.contextMarker : "",
    `[[LORE_RECOVERY_START:${recoveryId}]]`,
  );
});

test("completed recovery decoder replaces malformed diff with safe fallback", () => {
  const state = foldSessionEntries([
    {
      details: {
        loreExtension: {
          kind: "completedRecovery",
          completed: {
            recoveryId,
            startValidationToolCallId: "call-1",
            finalValidationToolCallId: "call-2",
            summary: "summary",
            contextReplacement: "replacement",
            diff: { stats: null, changedPaths: "not-an-array", truncated: "yes" },
            completedAt: 1,
          },
        },
      },
    },
  ]);
  assert.equal(state.completedRecoveries.length, 1);
  assert.equal(state.completedRecoveries[0]?.diff.reliable, false);
  assert.deepEqual(state.completedRecoveries[0]?.diff.changedPaths, []);
});

test("current returns a deep clone of nested state", async () => {
  const events: LoreSessionEvent[] = [];
  const store = new SessionStateStore({
    async append(event) {
      events.push(event);
    },
    async readActiveBranch() {
      return events.map(entryFor);
    },
  });
  await store.persist({
    kind: "completedRecovery",
    completed: {
      recoveryId,
      startValidationToolCallId: "call-1",
      finalValidationToolCallId: "call-2",
      summary: "summary",
      contextReplacement: "replacement",
      diff: {
        reliable: true,
        changedPaths: ["A.hs"],
        stats: { filesChanged: 1, additions: 1, deletions: 0 },
        truncated: false,
      },
      completedAt: 1,
    },
  });
  const snapshot = store.current();
  snapshot.completedRecoveries[0]!.diff.changedPaths.push("B.hs");
  snapshot.completedRecoveries[0]!.diff.stats.additions = 99;
  assert.deepEqual(store.current().completedRecoveries[0]?.diff.changedPaths, ["A.hs"]);
  assert.equal(store.current().completedRecoveries[0]?.diff.stats.additions, 1);
});

function activeRecoveryEvent(): LoreSessionEvent {
  return {
    kind: "recoveryState",
    state: {
      phase: "active",
      recoveryId,
      contextMarker: `[[LORE_SECTION_STARTED:${recoveryId}]]`,
      startValidationToolName: "reloadHomeModules",
      startValidationToolCallId: "call-1",
      startedAt: 1,
      reason: "reloadHomeModules failed",
      baselineId: recoveryId,
      compilationPending: true,
      testsPending: false,
    },
  };
}

function entryFor(event: LoreSessionEvent): PiEntry {
  return { details: { loreExtension: event } };
}
