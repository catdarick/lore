import assert from "node:assert/strict";
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { test } from "node:test";
import { RecoveryManager } from "../src/recovery.ts";
import { SessionStateStore } from "../src/session-state.ts";
import type { LoreConfig } from "../src/types.ts";
import { LoreRecoveryUi } from "../src/ui.ts";
import { FakePiHost } from "./test-support.ts";

const projectRoot = resolve(process.cwd(), "../");

test("cache sync failure after completion does not undo completedRecovery", async () => {
  const host = new FakePiHost(projectRoot);
  const stateDir = await mkdtemp(join(tmpdir(), "lore-recovery-reset-fail-"));
  const config: LoreConfig = {
    command: "python3",
    args: [],
    env: {},
    cwd: projectRoot,
    startupTimeoutMs: 1_000,
    defaultToolTimeoutMs: 1_000,
    toolTimeoutMs: {},
    summaryTimeoutMs: 1_000,
    maxInlineDiffBytes: 50_000,
    allowToolOverride: false,
    stateDir,
  };
  const store = new SessionStateStore(host);
  const ui = new LoreRecoveryUi(host);
  const recovery = new RecoveryManager({
    host,
    config,
    store,
    ui,
    synchronizeEmptyKnowledge: async () => {
      throw new Error("reset failed");
    },
  });

  host.currentAssistantStartId = "assistant-start";
  host.currentEntryIdValue = "assistant-start";

  await recovery.handleValidation(
    { kind: "semantic", toolName: "reloadHomeModules", success: false, structuredContent: { success: false } },
    "reload failed",
    "call-failed",
  );
  const marker =
    store.current().recovery.phase === "active" || store.current().recovery.phase === "readyToFinalize"
      ? store.current().recovery.contextMarker
      : "";
  const rawMessages = [
    { role: "user", content: [{ type: "text", text: "reload again" }], timestamp: Date.now() - 2 },
    {
      role: "custom",
      customType: "lore-recovery-context-marker",
      content: marker,
      display: false,
      timestamp: Date.now() - 1,
    },
  ];
  host.appendEntry({
    id: "tool-result-failed",
    role: "toolResult",
    toolCallId: "call-failed",
    toolName: "reloadHomeModules",
    content: [{ type: "text", text: "reload failed" }],
    details: { lore: { structuredContent: { success: false } } },
  });

  await recovery.handleValidation(
    { kind: "semantic", toolName: "reloadHomeModules", success: true, structuredContent: { success: true } },
    "reload ok",
    "call-fixed",
  );
  host.appendEntry({
    id: "tool-result-fixed",
    role: "toolResult",
    toolCallId: "call-fixed",
    toolName: "reloadHomeModules",
    content: [{ type: "text", text: "reload ok" }],
    details: { lore: { structuredContent: { success: true } } },
  });

  await recovery.processContext({ rawMessages, normalizedEntries: host.getActiveBranchEntries() });

  const state = store.current();
  assert.equal(state.completedRecoveries.length, 1);
  assert.equal(state.recovery.phase, "inactive");
  assert.equal(host.notices.some((notice) => /reset failed/i.test(notice)), true);
  assert.equal(state.knowledge.hashes.length, 0);
});
