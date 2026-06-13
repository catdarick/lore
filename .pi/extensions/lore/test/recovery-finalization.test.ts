import assert from "node:assert/strict";
import { existsSync } from "node:fs";
import { join, resolve } from "node:path";
import { test } from "node:test";
import { createLoreExtension } from "../src/index.ts";
import { FakePiHost } from "./test-support.ts";

const projectRoot = resolve(process.cwd(), "../../..");

test("recovery finalization waits for a context event with the persisted toolResult", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    host.currentEntryIdValue = "assistant-1";
    host.currentAssistantStartId = "assistant-1";

    await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
    const markerMessage = host.sentMessages.find((event) => event.message.customType === "lore-recovery-context-marker");
    assert.ok(markerMessage);
    assert.equal(markerMessage?.message.display, false);
    assert.match(String(markerMessage?.message.content ?? ""), /\[\[LORE_SECTION_STARTED:/);
    assert.equal(String(markerMessage?.message.content ?? "").trim(), runtime.getState().recovery.phase === "active" ? runtime.getState().recovery.contextMarker : "");
    assert.equal(
      (markerMessage?.message.details as { loreExtension?: { kind?: string } } | undefined)?.loreExtension?.kind,
      "recoveryState",
    );
    host.appendEntry({
      type: "message",
      message: {
        role: "toolResult",
        toolCallId: "call-1",
        toolName: "reloadHomeModules",
        content: [{ type: "text", text: "reload failed" }],
        details: { lore: { structuredContent: { success: false } } },
        isError: false,
        timestamp: Date.now(),
      },
    });
    assert.equal(runtime.getState().recovery.phase, "active");

    await host.tools
      .get("reloadHomeModules")
      ?.execute("call-2", { success: true }, undefined, undefined, undefined);
    assert.equal(runtime.getState().recovery.phase, "readyToFinalize");
    assert.equal(runtime.getState().completedRecoveries.length, 0);

    const marker = runtime.getState().recovery.phase === "readyToFinalize" ? runtime.getState().recovery.contextMarker : "";
    const rawMessages = [
      { role: "user", content: [{ type: "text", text: "reload again and fix" }], timestamp: Date.now() - 2 },
      {
        role: "custom",
        customType: "lore-recovery-context-marker",
        content: marker,
        display: false,
        timestamp: Date.now() - 1,
      },
    ];

    await runtime.processContext({ rawMessages, normalizedEntries: host.getActiveBranchEntries() });
    assert.equal(runtime.getState().completedRecoveries.length, 0);

    host.appendEntry({
      type: "message",
      message: {
        role: "toolResult",
        toolCallId: "call-2",
        toolName: "reloadHomeModules",
        content: [{ type: "text", text: "reload ok" }],
        details: { lore: { structuredContent: { success: true } } },
        isError: false,
        timestamp: Date.now(),
      },
    });

    await runtime.processContext({ rawMessages, normalizedEntries: host.getActiveBranchEntries() });

    await waitFor(() => runtime.getState().completedRecoveries.length === 1);

    assert.equal(runtime.getState().recovery.phase, "inactive");
    assert.equal(runtime.getState().completedRecoveries.length, 1);
    const completed = runtime.getState().completedRecoveries[0];
    assert.ok(completed);
    const artifactsDir = join(projectRoot, ".pi/extensions/lore/state-test/recoveries", completed.recoveryId);
    assert.equal(existsSync(artifactsDir), false);
    const replacement = runtime.getState().completedRecoveries[0]?.contextReplacement ?? "";
    assert.match(replacement, /Diff:/);
    assert.equal(host.notices.some((notice) => notice.startsWith("Recovery started: ")), true);
    assert.equal(host.notices.some((notice) => notice.startsWith("Recovery completed: ")), true);
    const summaryMessage = host.entries.find((entry) => entry.customType === "lore-recovery-summary");
    assert.ok(summaryMessage);
    assert.equal(summaryMessage?.display, true);
    assert.match(String(summaryMessage?.content ?? ""), /\[\[LORE_FIXES_APPLIED\]\]/);
    assert.match(String(summaryMessage?.content ?? ""), /Diff:/);
    assert.match(
      host.notices.find((notice) => notice.startsWith("Recovery completed:")) ?? "",
      /Recovery context: ~\d+ tokens -> summary: ~\d+ tokens/,
    );
    assert.equal(typeof completed.tokenMetrics?.originalRecoveryTokens, "number");
    assert.equal(typeof completed.tokenMetrics?.summaryReplacementTokens, "number");
  } finally {
    await runtime.stop();
  }
});

test("recovery finalization remains retryable when restart lacks summary context", async () => {
  const host = new FakePiHost(projectRoot);

  const firstRuntime = await createLoreExtension(host);
  await firstRuntime.start();
  host.currentEntryIdValue = "assistant-1";
  host.currentAssistantStartId = "assistant-1";
  await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
  await host.tools
    .get("reloadHomeModules")
    ?.execute("call-2", { success: true }, undefined, undefined, undefined);
  host.appendEntry({
    type: "message",
    message: {
      role: "toolResult",
      toolCallId: "call-2",
      toolName: "reloadHomeModules",
      content: [{ type: "text", text: "reload ok" }],
      details: { lore: { structuredContent: { success: true } } },
      isError: false,
      timestamp: Date.now(),
    },
  });
  await firstRuntime.stop();
  host.tools.clear();

  const secondRuntime = await createLoreExtension(host);
  await secondRuntime.start();
  try {
    await host.emit("session_start");
    await waitFor(() => secondRuntime.getState().recovery.phase === "readyToFinalize");
    assert.equal(secondRuntime.getState().recovery.phase, "readyToFinalize");
    assert.equal(secondRuntime.getState().completedRecoveries.length, 0);
  } finally {
    await secondRuntime.stop();
  }
});

test("context projection waits for recovery finalization before continuing the same turn", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    host.currentEntryIdValue = "assistant-1";
    host.currentAssistantStartId = "assistant-1";

    await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
    await host.tools
      .get("reloadHomeModules")
      ?.execute("call-2", { success: true }, undefined, undefined, undefined);
    host.entries.unshift({
      id: "assistant-1",
      role: "assistant",
      content: [{ type: "toolCall", id: "call-1", name: "reloadHomeModules" }],
    });
    host.appendEntry({
      id: "assistant-2",
      role: "assistant",
      content: [{ type: "toolCall", id: "call-2", name: "reloadHomeModules" }],
    });

    const marker = (runtime.getState().recovery.phase === "readyToFinalize" || runtime.getState().recovery.phase === "active")
      ? runtime.getState().recovery.contextMarker
      : "[[LORE_SECTION_STARTED:unknown]]";

    const rawMessages = [
      { role: "user", content: [{ type: "text", text: "reload again and fix" }], timestamp: Date.now() - 3 },
      {
        role: "custom",
        customType: "lore-recovery-context-marker",
        content: marker,
        display: false,
        timestamp: Date.now() - 2,
      },
      { role: "assistant", content: [{ type: "text", text: "Investigating failure" }], timestamp: Date.now() - 1 },
    ];

    host.appendEntry({
      type: "message",
      message: {
        role: "toolResult",
        toolCallId: "call-2",
        toolName: "reloadHomeModules",
        content: [{ type: "text", text: "reload ok" }],
        details: { lore: { structuredContent: { success: true } } },
        isError: false,
        timestamp: Date.now(),
      },
    });

    await runtime.processContext({ rawMessages, normalizedEntries: host.getActiveBranchEntries() });

    assert.equal(runtime.getState().recovery.phase, "inactive");
    assert.equal(runtime.getState().completedRecoveries.length, 1);
    assert.equal(host.generatedTextFromMessagesRequests.length, 1);
    assert.equal(host.generatedTextFromMessagesRequests[0]?.messages, rawMessages);
    assert.match(host.generatedTextFromMessagesRequests[0]?.prompt ?? "", /### Errors and fixes/);
    assert.doesNotMatch(host.generatedTextFromMessagesRequests[0]?.prompt ?? "", /### Failed tests, causes, and fixes/);
    assert.match(runtime.getState().completedRecoveries[0]?.summary ?? "", /from messages/i);
  } finally {
    await runtime.stop();
  }
});

test("recovery completion events are ordered before the main continuation", async () => {
  const host = new FakePiHost(projectRoot);
  const events: string[] = [];
  const originalAppendEntry = host.appendEntry.bind(host);
  host.appendEntry = (entry) => {
    const event = entry.details && typeof entry.details === "object"
      ? (entry.details as { loreExtension?: { kind?: string } }).loreExtension
      : undefined;
    const appended = originalAppendEntry(entry);
    if (event?.kind === "completedRecovery") {
      events.push("completedRecovery persisted");
    }
    return appended;
  };
  host.notify = (message) => {
    if (message.startsWith("Recovery completed:")) {
      events.push("completion notification");
    }
    host.notices.push(message);
  };
  const originalAppendDisplay = host.appendDisplayMessageImmediately.bind(host);
  host.appendDisplayMessageImmediately = (message) => {
    const result = originalAppendDisplay(message);
    if (message.customType === "lore-recovery-summary" && result === "appended") {
      events.push("visible summary appended");
    }
    return result;
  };

  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
    host.appendEntry({
      role: "toolResult",
      toolCallId: "call-1",
      toolName: "reloadHomeModules",
      content: [{ type: "text", text: "reload failed" }],
      details: { lore: { structuredContent: { success: false } } },
    });
    await host.tools.get("reloadHomeModules")?.execute("call-2", { success: true }, undefined, undefined, undefined);
    const marker = runtime.getState().recovery.phase === "readyToFinalize"
      ? runtime.getState().recovery.contextMarker
      : "[[LORE_SECTION_STARTED:unknown]]";
    host.appendEntry({
      role: "toolResult",
      toolCallId: "call-2",
      toolName: "reloadHomeModules",
      content: [{ type: "text", text: "reload ok" }],
      details: { lore: { structuredContent: { success: true } } },
    });

    await runtime.processContext({
      rawMessages: [{ role: "custom", customType: "lore-recovery-context-marker", content: marker, display: false }],
      normalizedEntries: host.getActiveBranchEntries(),
    });
    events.push("context handler returned");
    events.push("main continuation started");

    assert.deepEqual(events, [
      "completedRecovery persisted",
      "completion notification",
      "visible summary appended",
      "context handler returned",
      "main continuation started",
    ]);
  } finally {
    await runtime.stop();
  }
});

test("test-recovery summarization prompt requires test sections only", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    host.currentEntryIdValue = "assistant-1";
    host.currentAssistantStartId = "assistant-1";

    await host.tools.get("runTestSuite")?.execute("test-call-1", { success: false }, undefined, undefined, undefined);
    await host.tools.get("runTestSuite")?.execute("test-call-2", { success: true }, undefined, undefined, undefined);

    const marker = (runtime.getState().recovery.phase === "readyToFinalize" || runtime.getState().recovery.phase === "active")
      ? runtime.getState().recovery.contextMarker
      : "[[LORE_SECTION_STARTED:unknown]]";

    const rawMessages = [
      { role: "user", content: [{ type: "text", text: "fix failing tests" }], timestamp: Date.now() - 3 },
      {
        role: "custom",
        customType: "lore-recovery-context-marker",
        content: marker,
        display: false,
        timestamp: Date.now() - 2,
      },
      { role: "assistant", content: [{ type: "text", text: "Investigating test failures" }], timestamp: Date.now() - 1 },
    ];

    host.appendEntry({
      type: "message",
      message: {
        role: "toolResult",
        toolCallId: "test-call-1",
        toolName: "runTestSuite",
        content: [{ type: "text", text: "tests failed" }],
        details: { lore: { structuredContent: { success: false } } },
        isError: false,
        timestamp: Date.now(),
      },
    });
    host.appendEntry({
      type: "message",
      message: {
        role: "toolResult",
        toolCallId: "test-call-2",
        toolName: "runTestSuite",
        content: [{ type: "text", text: "tests ok" }],
        details: { lore: { structuredContent: { success: true } } },
        isError: false,
        timestamp: Date.now(),
      },
    });

    await runtime.processContext({ rawMessages, normalizedEntries: host.getActiveBranchEntries() });

    assert.equal(host.generatedTextFromMessagesRequests.length, 1);
    const prompt = host.generatedTextFromMessagesRequests[0]?.prompt ?? "";
    assert.match(prompt, /### Failed tests, causes, and fixes/);
    assert.doesNotMatch(prompt, /^### Errors and fixes$/m);
  } finally {
    await runtime.stop();
  }
});

test("failed validation after readyToFinalize reopens recovery", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
    host.appendEntry({
      role: "toolResult",
      toolCallId: "call-1",
      toolName: "reloadHomeModules",
      content: [{ type: "text", text: "reload failed" }],
      details: { lore: { structuredContent: { success: false } } },
    });
    await host.tools.get("reloadHomeModules")?.execute("call-2", { success: true }, undefined, undefined, undefined);
    assert.equal(runtime.getState().recovery.phase, "readyToFinalize");

    await host.tools.get("reloadHomeModules")?.execute("call-3", { success: false }, undefined, undefined, undefined);

    const recovery = runtime.getState().recovery;
    assert.equal(recovery.phase, "active");
    assert.equal(recovery.phase === "active" ? recovery.compilationPending : false, true);
  } finally {
    await runtime.stop();
  }
});

test("failed validation after summary failure reopens recovery", async () => {
  const host = new FakePiHost(projectRoot);
  host.generateTextFromMessages = async () => {
    throw new Error("summary failed");
  };
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
    await host.tools.get("reloadHomeModules")?.execute("call-2", { success: true }, undefined, undefined, undefined);
    const marker = runtime.getState().recovery.phase === "readyToFinalize"
      ? runtime.getState().recovery.contextMarker
      : "[[LORE_SECTION_STARTED:unknown]]";
    host.appendEntry({
      role: "toolResult",
      toolCallId: "call-2",
      toolName: "reloadHomeModules",
      content: [{ type: "text", text: "reload ok" }],
      details: { lore: { structuredContent: { success: true } } },
    });
    await runtime.processContext({
      rawMessages: [{ role: "custom", content: marker, display: false }],
      normalizedEntries: host.getActiveBranchEntries(),
    });
    assert.equal(runtime.getState().recovery.phase, "finalizationFailed");

    await host.tools.get("reloadHomeModules")?.execute("call-3", { success: false }, undefined, undefined, undefined);

    assert.equal(runtime.getState().recovery.phase, "active");
  } finally {
    await runtime.stop();
  }
});

test("latest validation in one assistant sequence is authoritative", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
    await host.tools.get("reloadHomeModules")?.execute("call-2", { success: true }, undefined, undefined, undefined);
    await host.tools.get("runTestSuite")?.execute("call-3", { success: false }, undefined, undefined, undefined);
    const recovery = runtime.getState().recovery;
    assert.equal(recovery.phase, "active");
    assert.equal(recovery.phase === "active" ? recovery.testsPending : false, true);
  } finally {
    await runtime.stop();
  }
});

test("marker delivery failure does not leave active recovery", async () => {
  const host = new FakePiHost(projectRoot);
  host.sendMessage = () => {
    throw new Error("marker failed");
  };
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    await assert.rejects(
      () => host.tools.get("reloadHomeModules")!.execute("call-1", { success: false }, undefined, undefined, undefined),
      /marker failed/,
    );
    assert.equal(runtime.getState().recovery.phase, "inactive");
  } finally {
    await runtime.stop();
  }
});

test("recovery start does not use a split persistence entry after marker delivery", async () => {
  const host = new FakePiHost(projectRoot);
  host.appendEntry = () => {
    throw new Error("separate append should not be used");
  };
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
    assert.equal(runtime.getState().recovery.phase, "active");
    assert.equal(host.sentMessages.some((event) => event.message.customType === "lore-recovery-context-marker"), true);
  } finally {
    await runtime.stop();
  }
});

test("abandon prevents an in-flight finalization from committing completion", async () => {
  const host = new FakePiHost(projectRoot);
  let releaseSummary: ((value: string) => void) | undefined;
  const summaryStarted = new Promise<void>((resolveStarted) => {
    host.generateTextFromMessages = async (request) => {
      host.generatedTextFromMessagesRequests.push(request);
      resolveStarted();
      return new Promise<string>((resolveSummary) => {
        releaseSummary = resolveSummary;
      });
    };
  });
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    host.currentEntryIdValue = "assistant-1";
    host.currentAssistantStartId = "assistant-1";
    await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
    await host.tools.get("reloadHomeModules")?.execute("call-2", { success: true }, undefined, undefined, undefined);
    const marker = runtime.getState().recovery.phase === "readyToFinalize"
      ? runtime.getState().recovery.contextMarker
      : "[[LORE_SECTION_STARTED:unknown]]";
    const rawMessages = [
      { role: "custom", customType: "lore-recovery-context-marker", content: marker, display: false },
    ];
    host.appendEntry({
      role: "toolResult",
      toolCallId: "call-2",
      toolName: "reloadHomeModules",
      content: [{ type: "text", text: "reload ok" }],
      details: { lore: { structuredContent: { success: true } } },
    });

    const finalization = runtime.processContext({ rawMessages, normalizedEntries: host.getActiveBranchEntries() });
    await summaryStarted;
    const abandon = runtime.abandonRecovery();
    releaseSummary?.("Summary after abandon");
    await Promise.all([finalization, abandon]);

    assert.equal(runtime.getState().recovery.phase, "inactive");
    assert.equal(runtime.getState().completedRecoveries.length, 0);
    assert.equal(host.entries.some((entry) => entry.customType === "lore-recovery-summary"), false);
  } finally {
    await runtime.stop();
  }
});

test("completion message uses immediate display and status clears when notify fails", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    host.currentEntryIdValue = "assistant-1";
    host.currentAssistantStartId = "assistant-1";
    await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
    await host.tools.get("reloadHomeModules")?.execute("call-2", { success: true }, undefined, undefined, undefined);
    host.entries.unshift({
      id: "assistant-1",
      role: "assistant",
      content: [{ type: "toolCall", id: "call-1", name: "reloadHomeModules" }],
    });
    host.appendEntry({
      id: "assistant-2",
      role: "assistant",
      content: [{ type: "toolCall", id: "call-2", name: "reloadHomeModules" }],
    });
    const marker = runtime.getState().recovery.phase === "readyToFinalize"
      ? runtime.getState().recovery.contextMarker
      : "[[LORE_SECTION_STARTED:unknown]]";
    host.appendEntry({
      role: "toolResult",
      toolCallId: "call-2",
      toolName: "reloadHomeModules",
      content: [{ type: "text", text: "reload ok" }],
      details: { lore: { structuredContent: { success: true } } },
    });
    host.notify = () => {
      throw new Error("notify failed");
    };

    await runtime.processContext({
      rawMessages: [{ role: "custom", customType: "lore-recovery-context-marker", content: marker, display: false }],
      normalizedEntries: host.getActiveBranchEntries(),
    });

    const summary = host.entries.find((entry) => entry.customType === "lore-recovery-summary");
    assert.equal(summary?.display, true);
    assert.equal(host.sentMessages.some((event) => event.message.customType === "lore-recovery-summary"), false);
    assert.equal(host.statuses.has("lore-recovery"), false);
    assert.equal(runtime.getState().recovery.phase, "inactive");
  } finally {
    await runtime.stop();
  }
});

test("visible summary and notification failures do not abort completed recovery context", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
    host.appendEntry({
      role: "toolResult",
      toolCallId: "call-1",
      toolName: "reloadHomeModules",
      content: [{ type: "text", text: "reload failed" }],
      details: { lore: { structuredContent: { success: false } } },
    });
    await host.tools.get("reloadHomeModules")?.execute("call-2", { success: true }, undefined, undefined, undefined);
    const marker = runtime.getState().recovery.phase === "readyToFinalize"
      ? runtime.getState().recovery.contextMarker
      : "[[LORE_SECTION_STARTED:unknown]]";
    host.appendEntry({
      role: "toolResult",
      toolCallId: "call-2",
      toolName: "reloadHomeModules",
      content: [{ type: "text", text: "reload ok" }],
      details: { lore: { structuredContent: { success: true } } },
    });
    host.appendDisplayMessageImmediately = () => {
      throw new Error("display append failed");
    };
    host.notify = () => {
      throw new Error("notify failed too");
    };

    const projected = await runtime.processContext({
      rawMessages: [{ role: "custom", customType: "lore-recovery-context-marker", content: marker, display: false }],
      normalizedEntries: host.getActiveBranchEntries(),
    });

    assert.equal(runtime.getState().recovery.phase, "inactive");
    assert.equal(runtime.getState().completedRecoveries.length, 1);
    const replacement = runtime.getState().completedRecoveries[0]?.contextReplacement ?? "";
    assert.ok(replacement.length > 0);
    assert.equal(projected.some((entry) => entry.content === replacement), true);
  } finally {
    await runtime.stop();
  }
});

test("completion message append is idempotent by recovery id", async () => {
  const host = new FakePiHost(projectRoot);
  const runtime = await createLoreExtension(host);
  await runtime.start();
  try {
    await host.tools.get("reloadHomeModules")?.execute("call-1", { success: false }, undefined, undefined, undefined);
    await host.tools.get("reloadHomeModules")?.execute("call-2", { success: true }, undefined, undefined, undefined);
    const marker = runtime.getState().recovery.phase === "readyToFinalize"
      ? runtime.getState().recovery.contextMarker
      : "[[LORE_SECTION_STARTED:unknown]]";
    const recoveryId = runtime.getState().recovery.phase === "readyToFinalize"
      ? runtime.getState().recovery.recoveryId
      : "missing";
    host.appendEntry({
      role: "toolResult",
      toolCallId: "call-2",
      toolName: "reloadHomeModules",
      content: [{ type: "text", text: "reload ok" }],
      details: { lore: { structuredContent: { success: true } } },
    });
    host.appendEntry({
      type: "custom_message",
      role: "custom",
      customType: "lore-recovery-summary",
      content: "already shown",
      display: true,
      details: { recoveryId, hiddenFromModel: true },
    });

    await runtime.processContext({
      rawMessages: [{ role: "custom", customType: "lore-recovery-context-marker", content: marker, display: false }],
      normalizedEntries: host.getActiveBranchEntries(),
    });

    const summaries = host.entries.filter((entry) => entry.customType === "lore-recovery-summary");
    assert.equal(summaries.length, 1);
    assert.equal(summaries[0]?.content, "already shown");
  } finally {
    await runtime.stop();
  }
});

async function waitFor(predicate: () => boolean): Promise<void> {
  const deadline = Date.now() + 2_000;
  while (Date.now() < deadline) {
    if (predicate()) {
      return;
    }
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
  throw new Error("Timed out waiting for condition");
}
