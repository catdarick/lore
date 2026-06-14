import { mkdir } from "node:fs/promises";
import {
  estimateCompletedRecoveryTokenMetrics,
  projectCompletedRecoveries,
  resolveCompletedRecoveryRange,
} from "./context-projection.ts";
import {
  captureRecoveryBaseline,
  captureRecoveryDiff,
  removeRecoveryArtifacts,
} from "./recovery-diff.ts";
import { generateRecoverySummary } from "./recovery-summary.ts";
import type { SessionStateStore } from "./session-state.ts";
import type {
  CompletedRecovery,
  LoreConfig,
  PiEntry,
  PiHost,
  RecoveryObligations,
  RecoveryState,
  ValidationToolName,
} from "./types.ts";
import { decodeValidationSuccess, type ValidationOutcome } from "./lore-protocol.ts";
import { newId, nowMs, stableStringify } from "./util.ts";
import { LoreRecoveryUi } from "./ui.ts";

type RetryableRecovery = Extract<RecoveryState, { phase: "readyToFinalize" | "finalizationFailed" }>;
type ActiveRecovery = Extract<RecoveryState, { phase: "active" }>;

export class RecoveryManager {
  private readonly host: PiHost;
  private readonly config: LoreConfig;
  private readonly store: SessionStateStore;
  private readonly ui: LoreRecoveryUi;
  private readonly synchronizeEmptyKnowledge: () => Promise<void>;
  private finalizingPromise: Promise<void> | undefined;

  constructor(input: {
    host: PiHost;
    config: LoreConfig;
    store: SessionStateStore;
    ui: LoreRecoveryUi;
    synchronizeEmptyKnowledge: () => Promise<void>;
  }) {
    this.host = input.host;
    this.config = input.config;
    this.store = input.store;
    this.ui = input.ui;
    this.synchronizeEmptyKnowledge = input.synchronizeEmptyKnowledge;
  }

  async restoreUi(): Promise<void> {
    const recovery = this.store.current().recovery;
    if (recovery.phase === "inactive") {
      await this.ui.clearStatus();
      return;
    }
    if (recovery.phase === "readyToFinalize" || recovery.phase === "finalizationFailed") {
      await this.ui.showFinalizingStatus();
      return;
    }
    await this.ui.showActiveStatus(recovery);
  }

  async handleValidation(outcome: ValidationOutcome, _finalValidationText: string, toolCallId: string): Promise<void> {
    if (outcome.kind !== "semantic") {
      return;
    }
    const current = this.store.current().recovery;
    if (current.phase === "inactive") {
      if (outcome.success) {
        return;
      }
      const recovery = await this.startRecovery(outcome, toolCallId);
      try {
        await this.ui.appendContextMarker(recovery);
        await this.store.reloadFromBranch();
      } catch (error) {
        await this.cleanupRecoveryArtifacts(recovery.baselineId);
        throw error;
      }
      await this.uiBestEffort(() => this.ui.appendStartSeparator(recovery.recoveryId, recovery.reason));
      await this.uiBestEffort(() => this.ui.showActiveStatus(recovery));
      return;
    }

    const updated = updateRecoveryObligations(recoveryAsActive(current), outcome);
    if (hasPending(updated)) {
      await this.persistRecovery(updated);
      await this.uiBestEffort(() => this.ui.showActiveStatus(updated));
      return;
    }
    await this.persistRecovery({
      phase: "readyToFinalize",
      recoveryId: updated.recoveryId,
      contextMarker: updated.contextMarker,
      startValidationToolName: updated.startValidationToolName,
      startValidationToolCallId: updated.startValidationToolCallId,
      finalValidationToolCallId: toolCallId,
      startEntryId: updated.startEntryId,
      startedAt: updated.startedAt,
      reason: updated.reason,
      baselineId: updated.baselineId,
      compilationPending: false,
      testsPending: false,
    });
    await this.uiBestEffort(() => this.ui.showFinalizingStatus());
  }

  async processContext(input: { rawMessages: unknown[]; normalizedEntries: PiEntry[] }): Promise<PiEntry[]> {
    const recovery = this.store.current().recovery;
    if (recovery.phase === "inactive" || recovery.phase === "active") {
      return projectCompletedRecoveries(input.normalizedEntries, this.store.current().completedRecoveries);
    }

    if (!this.contextContainsMarker(input.rawMessages, recovery.contextMarker)) {
      return projectCompletedRecoveries(input.normalizedEntries, this.store.current().completedRecoveries);
    }
    const finalValidation = findSuccessfulValidationToolResult(input.normalizedEntries, recovery.finalValidationToolCallId);
    if (!finalValidation) {
      return projectCompletedRecoveries(input.normalizedEntries, this.store.current().completedRecoveries);
    }

    await this.finalizeRecovery(recovery, input);
    const completed = this.store.current().completedRecoveries;
    return projectCompletedRecoveries(input.normalizedEntries, completed);
  }

  async abandonRecovery(): Promise<void> {
    const recovery = this.store.current().recovery;
    if (recovery.phase === "inactive") {
      return;
    }
    await this.store.persist({
      kind: "recoveryAbandoned",
      recoveryId: recovery.recoveryId,
      state: { phase: "inactive" },
      abandonedAt: nowMs(),
    });
    if (this.finalizingPromise) {
      await this.finalizingPromise;
    }
    await this.ui.clearStatus();
    await this.cleanupRecoveryArtifacts(recovery.baselineId);
  }

  private async startRecovery(
    outcome: Extract<ValidationOutcome, { kind: "semantic" }>,
    toolCallId: string,
  ): Promise<ActiveRecovery> {
    await mkdir(this.config.stateDir, { recursive: true });
    const recoveryId = newId("lore-recovery");
    const baselineId = await captureRecoveryBaseline(this.config, recoveryId);
    const obligations = failedObligations(outcome.toolName);
    const reason = reasonText(outcome.toolName, outcome.structuredContent);
    return {
      phase: "active",
      recoveryId,
      contextMarker: recoveryContextMarker(recoveryId),
      startValidationToolName: outcome.toolName,
      startEntryId: this.host.getCurrentAssistantSequenceStartEntryId?.() ?? this.host.getCurrentEntryId?.(),
      startValidationToolCallId: toolCallId,
      startedAt: nowMs(),
      reason,
      baselineId,
      ...obligations,
    };
  }

  private async finalizeRecovery(
    recovery: RetryableRecovery,
    input: { rawMessages: unknown[]; normalizedEntries: PiEntry[] },
  ): Promise<void> {
    if (this.finalizingPromise) {
      await this.finalizingPromise;
      return;
    }
    this.finalizingPromise = this.finalizeRecoveryOnce(recovery, input);
    try {
      await this.finalizingPromise;
    } finally {
      this.finalizingPromise = undefined;
    }
  }

  private async finalizeRecoveryOnce(
    recovery: RetryableRecovery,
    input: { rawMessages: unknown[]; normalizedEntries: PiEntry[] },
  ): Promise<void> {
    let diff;
    try {
      diff = await captureRecoveryDiff(this.config, recovery.baselineId);
    } catch (error) {
      await this.persistFinalizationFailed(recovery, "diff", errorMessage(error));
      await this.ui.showInfrastructureError(`Lore recovery diff capture failed: ${errorMessage(error)}`);
      return;
    }

    let summary;
    try {
      summary = await generateRecoverySummary({
        host: this.host,
        config: this.config,
        recovery,
        contextMessages: input.rawMessages,
        diff,
      });
    } catch (error) {
      await this.persistFinalizationFailed(recovery, "summary", errorMessage(error));
      await this.ui.showInfrastructureError(`Lore recovery summary failed: ${errorMessage(error)}`);
      return;
    }

    const completed: CompletedRecovery = {
      recoveryId: recovery.recoveryId,
      startEntryId: recovery.startEntryId,
      startValidationToolCallId: recovery.startValidationToolCallId,
      finalValidationToolCallId: recovery.finalValidationToolCallId,
      summary: summary.summary,
      contextReplacement: summary.contextReplacement,
      diff: stripTransientRecoveryDiffArtifacts(diff),
      completedAt: nowMs(),
    };
    const recoveryRange = resolveCompletedRecoveryRange(input.normalizedEntries, completed);
    if (!recoveryRange) {
      await this.persistFinalizationFailed(
        recovery,
        "context-range",
        `completed recovery range could not be resolved for ${recovery.recoveryId}`,
      );
      await this.ui.showInfrastructureError("Lore recovery completion failed: recovery context range could not be resolved");
      return;
    }
    completed.tokenMetrics = estimateCompletedRecoveryTokenMetrics(input.normalizedEntries, completed, recoveryRange);

    try {
      const committed = await this.store.compareAndPersist(
        (state) => isSameRetryableRecovery(state.recovery, recovery),
        { kind: "completedRecovery", completed },
      );
      if (!committed) {
        return;
      }
    } catch (error) {
      await this.persistFinalizationFailed(recovery, "commit", errorMessage(error));
      await this.ui.showInfrastructureError(`Lore recovery completion persist failed: ${errorMessage(error)}`);
      return;
    }

    try {
      await this.synchronizeEmptyKnowledge();
    } catch (error) {
      await this.uiBestEffort(() =>
        this.ui.showInfrastructureError(
          `Lore recovery completed, but immediate knowledge-cache synchronization failed: ${errorMessage(error)}`,
        ),
      );
    }

    await this.uiBestEffort(() => this.ui.notifyRecoveryCompleted(completed.diff, completed.tokenMetrics));
    await this.uiBestEffort(
      () => this.ui.appendCompletionMessage(completed.recoveryId, completed.summary, completed.contextReplacement),
      (error) => `Lore recovery completed, but the visible summary could not be rendered: ${errorMessage(error)}`,
    );
    await this.uiBestEffort(() => this.ui.clearStatus());
    await this.cleanupRecoveryArtifacts(recovery.baselineId);
  }

  private async persistFinalizationFailed(
    recovery: RetryableRecovery,
    stage: Extract<RecoveryState, { phase: "finalizationFailed" }>["failure"]["stage"],
    message: string,
  ): Promise<void> {
    const persisted = await this.store.compareAndPersist(
      (state) => isSameRetryableRecovery(state.recovery, recovery),
      {
        kind: "recoveryState",
        state: {
      ...recovery,
      phase: "finalizationFailed",
      compilationPending: false,
      testsPending: false,
      failure: { stage, message, failedAt: nowMs() },
        },
      },
    );
    if (persisted) {
      await this.ui.showFinalizingStatus();
    }
  }

  private async cleanupRecoveryArtifacts(baselineId: string): Promise<void> {
    try {
      await removeRecoveryArtifacts(this.config, baselineId);
    } catch (error) {
      await this.ui.showInfrastructureError(`Lore recovery artifact cleanup failed: ${errorMessage(error)}`);
    }
  }

  private async persistRecovery(state: Exclude<RecoveryState, { phase: "inactive" }>): Promise<void> {
    await this.store.persist({ kind: "recoveryState", state });
  }

  private contextContainsMarker(messages: unknown[], marker: string): boolean {
    return messages.some((message) => valueContainsString(message, marker));
  }

  private async uiBestEffort(
    action: () => Promise<unknown>,
    failureMessage: (error: unknown) => string = errorMessage,
  ): Promise<void> {
    try {
      await action();
    } catch (error) {
      try {
        await this.ui.showInfrastructureError(failureMessage(error));
      } catch {
        // UI notification failures must not alter durable recovery state.
      }
    }
  }
}

export function updateRecoveryObligations(
  recovery: ActiveRecovery,
  outcome: Extract<ValidationOutcome, { kind: "semantic" }>,
): ActiveRecovery {
  const next: ActiveRecovery = { ...recovery };
  if (outcome.toolName === "reloadHomeModules") {
    next.compilationPending = !outcome.success;
  } else if (outcome.toolName === "runTestSuite") {
    next.testsPending = !outcome.success;
    if (outcome.success) {
      next.compilationPending = false;
    }
  }
  return next;
}

function failedObligations(toolName: ValidationToolName): RecoveryObligations {
  return {
    compilationPending: toolName === "reloadHomeModules",
    testsPending: toolName === "runTestSuite",
  };
}

function hasPending(obligations: RecoveryObligations): boolean {
  return obligations.compilationPending || obligations.testsPending;
}

function recoveryAsActive(recovery: Exclude<RecoveryState, { phase: "inactive" }>): ActiveRecovery {
  return {
    phase: "active",
    recoveryId: recovery.recoveryId,
    contextMarker: recovery.contextMarker,
    startValidationToolName: recovery.startValidationToolName,
    startValidationToolCallId: recovery.startValidationToolCallId,
    startEntryId: recovery.startEntryId,
    startedAt: recovery.startedAt,
    reason: recovery.reason,
    baselineId: recovery.baselineId,
    compilationPending: recovery.compilationPending,
    testsPending: recovery.testsPending,
  };
}

function reasonText(toolName: string, structuredContent: unknown): string {
  if (toolName === "runTestSuite") {
    const args =
      structuredContent && typeof structuredContent === "object"
        ? (structuredContent as Record<string, unknown>).invocation
        : undefined;
    return `runTestSuite failed${args === undefined ? "" : ` (arguments: ${stableStringify(args)})`}`;
  }
  return "reloadHomeModules failed";
}

function recoveryContextMarker(recoveryId: string): string {
  return `[[LORE_SECTION_STARTED:${recoveryId}]]`;
}

function stripTransientRecoveryDiffArtifacts<T extends { patchPath?: string; truncated: boolean }>(diff: T): T {
  if (!diff.patchPath) {
    return diff;
  }
  return {
    ...diff,
    patchPath: undefined,
    truncated: true,
  };
}

function findSuccessfulValidationToolResult(
  entries: PiEntry[],
  toolCallId: string,
): { entry: PiEntry; toolResult: ToolResultEntry } | undefined {
  const found = findToolResultEntryByCallId(entries, toolCallId);
  if (!found || !decodeToolResultSuccess(found.toolResult)) {
    return undefined;
  }
  return { entry: found.entry, toolResult: found.toolResult };
}

function decodeToolResultSuccess(entry: ToolResultEntry): boolean {
  if (!isValidationToolName(entry.toolName)) {
    return false;
  }
  if (!entry.details || typeof entry.details !== "object") {
    return false;
  }
  const lore = (entry.details as Record<string, unknown>).lore;
  if (!lore || typeof lore !== "object") {
    return false;
  }
  const structuredContent = (lore as Record<string, unknown>).structuredContent;
  if (!structuredContent || typeof structuredContent !== "object") {
    return false;
  }
  try {
    return decodeValidationSuccess(entry.toolName, structuredContent);
  } catch {
    return false;
  }
}

function findToolResultEntryByCallId(
  entries: PiEntry[],
  toolCallId: string,
): { entry: PiEntry; toolResult: ToolResultEntry } | undefined {
  for (const entry of entries) {
    const toolResult = toToolResultEntry(entry);
    if (toolResult?.toolCallId === toolCallId) {
      return { entry, toolResult };
    }
  }
  return undefined;
}

type ToolResultEntry = {
  role: "toolResult";
  toolCallId?: unknown;
  toolName?: unknown;
  content?: unknown;
  details?: unknown;
};

function toToolResultEntry(entry: PiEntry): ToolResultEntry | undefined {
  if (entry.role === "toolResult") {
    return entry as ToolResultEntry;
  }
  const nested = entry.message;
  if (!nested || typeof nested !== "object") {
    return undefined;
  }
  const message = nested as Record<string, unknown>;
  if (message.role !== "toolResult") {
    return undefined;
  }
  return {
    role: "toolResult",
    toolCallId: message.toolCallId,
    toolName: message.toolName,
    content: message.content,
    details: message.details,
  };
}

function isValidationToolName(value: unknown): value is ValidationToolName {
  return value === "reloadHomeModules" || value === "runTestSuite";
}

function valueContainsString(value: unknown, needle: string): boolean {
  if (typeof value === "string") {
    return value.includes(needle);
  }
  if (Array.isArray(value)) {
    return value.some((item) => valueContainsString(item, needle));
  }
  if (!value || typeof value !== "object") {
    return false;
  }
  return Object.values(value as Record<string, unknown>).some((item) => valueContainsString(item, needle));
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function isSameRetryableRecovery(current: RecoveryState, expected: RetryableRecovery): boolean {
  return (
    (current.phase === "readyToFinalize" || current.phase === "finalizationFailed") &&
    current.recoveryId === expected.recoveryId &&
    current.finalValidationToolCallId === expected.finalValidationToolCallId
  );
}
