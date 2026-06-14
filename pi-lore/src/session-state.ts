import type {
  CompletedRecovery,
  KnowledgeSnapshot,
  LoreSessionEvent,
  PiEntry,
  PiHost,
  RecoveryFinalizationFailure,
  RecoveryDiff,
  RecoveryState,
  RecoveryTokenMetrics,
} from "./types.ts";

const detailsKey = "loreExtension";
const idPattern = /^(lore-recovery|lore-baseline)-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

export type FoldedSessionState = {
  knowledge: KnowledgeSnapshot;
  recovery: RecoveryState;
  completedRecoveries: CompletedRecovery[];
};

export type SessionLog = {
  append(event: LoreSessionEvent): Promise<void>;
  readActiveBranch(): Promise<PiEntry[]>;
};

export class SessionStateStore {
  private readonly log: SessionLog;
  private state: FoldedSessionState = emptyFoldedState();
  private operationQueue: Promise<unknown> = Promise.resolve();

  constructor(log: SessionLog);
  constructor(host: PiHost);
  constructor(input: SessionLog | PiHost) {
    this.log = isSessionLog(input) ? input : sessionLogFromHost(input);
  }

  current(): FoldedSessionState {
    return cloneState(this.state);
  }

  async reloadFromBranch(): Promise<FoldedSessionState> {
    return this.serialize(async () => {
      const entries = await this.log.readActiveBranch();
      this.state = foldSessionEntries(entries);
      return this.current();
    });
  }

  async persist(event: LoreSessionEvent): Promise<void> {
    await this.serialize(async () => {
      await this.log.append(event);
      applyEvent(this.state, event);
    });
  }

  async compareAndPersist(predicate: (state: FoldedSessionState) => boolean, event: LoreSessionEvent): Promise<boolean> {
    return this.serialize(async () => {
      if (!predicate(cloneState(this.state))) {
        return false;
      }
      await this.log.append(event);
      applyEvent(this.state, event);
      return true;
    });
  }

  private serialize<T>(operation: () => Promise<T>): Promise<T> {
    const next = this.operationQueue.then(operation, operation);
    this.operationQueue = next.catch(() => undefined);
    return next;
  }
}

export function emptyFoldedState(): FoldedSessionState {
  return {
    knowledge: { hashes: [] },
    recovery: { phase: "inactive" },
    completedRecoveries: [],
  };
}

export function foldSessionEntries(entries: PiEntry[]): FoldedSessionState {
  const state = emptyFoldedState();
  for (const entry of entries) {
    const event = eventFromEntry(entry);
    if (event) {
      applyEvent(state, event);
    }
    const snapshot = knowledgeSnapshotFromEntry(entry);
    if (snapshot) {
      state.knowledge = normalizeSnapshot(snapshot);
    }
  }
  return state;
}

export function eventFromEntry(entry: PiEntry): LoreSessionEvent | undefined {
  const details = entry.details;
  if (!details || typeof details !== "object") {
    return undefined;
  }
  const event = (details as Record<string, unknown>)[detailsKey];
  if (!event || typeof event !== "object" || !("kind" in event)) {
    return undefined;
  }
  return decodeSessionEvent(event);
}

export function knowledgeSnapshotFromEntry(entry: PiEntry): KnowledgeSnapshot | undefined {
  const details = entry.details;
  if (!details || typeof details !== "object") {
    return undefined;
  }
  const obj = details as Record<string, unknown>;
  const lore = obj.lore;
  if (lore && typeof lore === "object") {
    const snapshot = (lore as Record<string, unknown>).knowledgeSnapshot;
    if (isSnapshot(snapshot)) {
      return normalizeSnapshot(snapshot);
    }
  }
  const event = eventFromEntry(entry);
  if (event?.kind === "knowledgeSnapshot" || event?.kind === "knowledgeReset") {
    return normalizeSnapshot(event.snapshot);
  }
  return undefined;
}

export function applyEvent(state: FoldedSessionState, event: LoreSessionEvent): void {
  switch (event.kind) {
    case "knowledgeSnapshot":
    case "knowledgeReset":
      state.knowledge = normalizeSnapshot(event.snapshot);
      break;
    case "recoveryState":
      state.recovery = event.state;
      break;
    case "recoveryAbandoned":
      state.recovery = event.state;
      break;
    case "completedRecovery":
      state.completedRecoveries = state.completedRecoveries.filter(
        (completed) => completed.recoveryId !== event.completed.recoveryId,
      );
      state.completedRecoveries.push(event.completed);
      state.recovery = { phase: "inactive" };
      state.knowledge = { hashes: [] };
      break;
    case "uiMarker":
      break;
  }
}

export function normalizeSnapshot(snapshot: KnowledgeSnapshot): KnowledgeSnapshot {
  return { hashes: [...new Set(snapshot.hashes)].sort() };
}

export function sameSnapshot(left: KnowledgeSnapshot, right: KnowledgeSnapshot): boolean {
  const leftNorm = normalizeSnapshot(left);
  const rightNorm = normalizeSnapshot(right);
  return (
    leftNorm.hashes.length === rightNorm.hashes.length &&
    leftNorm.hashes.every((hash, index) => hash === rightNorm.hashes[index])
  );
}

function isSnapshot(value: unknown): value is KnowledgeSnapshot {
  return (
    Boolean(value) &&
    typeof value === "object" &&
    Array.isArray((value as { hashes?: unknown }).hashes) &&
    (value as { hashes: unknown[] }).hashes.every((hash) => typeof hash === "string")
  );
}

function decodeSessionEvent(value: unknown): LoreSessionEvent | undefined {
  if (!value || typeof value !== "object") {
    return undefined;
  }
  const obj = value as Record<string, unknown>;
  const kind = obj.kind;
  if (kind === "knowledgeSnapshot" || kind === "knowledgeReset") {
    const snapshot = decodeSnapshot(obj.snapshot);
    return snapshot ? { kind, snapshot } : undefined;
  }
  if (kind === "recoveryState") {
    const state = decodeRecoveryState(obj.state);
    return state ? { kind, state } : undefined;
  }
  if (kind === "recoveryAbandoned") {
    const recoveryId = decodeSafeId(obj.recoveryId);
    const abandonedAt = decodeFiniteNumber(obj.abandonedAt);
    if (!recoveryId || abandonedAt === undefined) {
      return undefined;
    }
    return { kind, recoveryId, abandonedAt, state: { phase: "inactive" } };
  }
  if (kind === "completedRecovery") {
    const completed = decodeCompletedRecovery(obj.completed);
    return completed ? { kind, completed } : undefined;
  }
  if (kind === "uiMarker") {
    const marker = obj.marker;
    const recoveryId = decodeSafeId(obj.recoveryId);
    if ((marker === "recovery-start" || marker === "recovery-complete") && recoveryId) {
      return { kind, marker, recoveryId };
    }
  }
  return undefined;
}

function decodeRecoveryState(value: unknown): RecoveryState | undefined {
  if (!value || typeof value !== "object") {
    return undefined;
  }
  const obj = value as Record<string, unknown>;
  if (obj.phase === "inactive" || obj.mode === "inactive") {
    return { phase: "inactive" };
  }

  const legacyMode = obj.mode;
  const phase = typeof obj.phase === "string" ? obj.phase : legacyMode === "finalizing" ? "readyToFinalize" : legacyMode;
  const recoveryId = decodeSafeId(obj.recoveryId);
  const baselineId = decodeSafeId(obj.baselineId);
  const contextMarker = decodeString(obj.contextMarker);
  const startValidationToolName = decodeValidationToolName(obj.startValidationToolName);
  const startValidationToolCallId = decodeString(obj.startValidationToolCallId);
  const startedAt = decodeFiniteNumber(obj.startedAt);
  const reason = decodeString(obj.reason);
  if (!recoveryId || !baselineId || !contextMarker || !startValidationToolName || !startValidationToolCallId || startedAt === undefined || !reason) {
    return undefined;
  }

  const base = {
    recoveryId,
    contextMarker,
    startValidationToolName,
    startValidationToolCallId,
    startEntryId: decodeString(obj.startEntryId),
    startedAt,
    reason,
    baselineId,
  };
  if (phase === "active") {
    const compilationPending = decodeBoolean(obj.compilationPending);
    const testsPending = decodeBoolean(obj.testsPending);
    if (compilationPending === undefined || testsPending === undefined) {
      return undefined;
    }
    return { phase, ...base, compilationPending, testsPending };
  }
  if (phase === "readyToFinalize") {
    const finalValidationToolCallId = decodeString(obj.finalValidationToolCallId);
    if (!finalValidationToolCallId) {
      return undefined;
    }
    return {
      phase,
      ...base,
      finalValidationToolCallId,
      compilationPending: false,
      testsPending: false,
    };
  }
  if (phase === "finalizationFailed") {
    const ready = decodeRecoveryState({ ...obj, phase: "readyToFinalize" });
    const failure = decodeFailure(obj.failure);
    if (!ready || ready.phase !== "readyToFinalize" || !failure) {
      return undefined;
    }
    return { ...ready, phase, failure };
  }
  return undefined;
}

function decodeCompletedRecovery(value: unknown): CompletedRecovery | undefined {
  if (!value || typeof value !== "object") {
    return undefined;
  }
  const obj = value as Record<string, unknown>;
  const recoveryId = decodeSafeId(obj.recoveryId);
  const startValidationToolCallId = decodeString(obj.startValidationToolCallId);
  const finalValidationToolCallId = decodeString(obj.finalValidationToolCallId);
  const summary = decodeString(obj.summary);
  const contextReplacement = decodeString(obj.contextReplacement);
  const completedAt = decodeFiniteNumber(obj.completedAt);
  const diff = decodeRecoveryDiff(obj.diff) ?? fallbackRecoveryDiff("Malformed or missing completed recovery diff");
  if (!recoveryId || !startValidationToolCallId || !finalValidationToolCallId || summary === undefined || contextReplacement === undefined || completedAt === undefined) {
    return undefined;
  }
  return {
    recoveryId,
    startEntryId: decodeString(obj.startEntryId),
    startValidationToolCallId,
    finalValidationToolCallId,
    summary,
    contextReplacement,
    diff,
    tokenMetrics: decodeRecoveryTokenMetrics(obj.tokenMetrics),
    completedAt,
  };
}

function decodeRecoveryTokenMetrics(value: unknown): RecoveryTokenMetrics | undefined {
  if (!value || typeof value !== "object") {
    return undefined;
  }
  const obj = value as Record<string, unknown>;
  const originalRecoveryTokens = decodeNonNegativeFiniteNumber(obj.originalRecoveryTokens);
  const summaryReplacementTokens = decodeNonNegativeFiniteNumber(obj.summaryReplacementTokens);
  if (originalRecoveryTokens === undefined || summaryReplacementTokens === undefined) {
    return undefined;
  }
  return {
    originalRecoveryTokens,
    summaryReplacementTokens,
    estimated: true,
  };
}

function decodeRecoveryDiff(value: unknown): RecoveryDiff | undefined {
  if (!value || typeof value !== "object") {
    return undefined;
  }
  const obj = value as Record<string, unknown>;
  const reliable = decodeBoolean(obj.reliable);
  const changedPaths = decodeStringArray(obj.changedPaths);
  const stats = decodeDiffStats(obj.stats);
  const truncated = decodeBoolean(obj.truncated);
  if (reliable === undefined || !changedPaths || !stats || truncated === undefined) {
    return undefined;
  }
  const reason = decodeString(obj.reason);
  const inlinePatch = decodeString(obj.inlinePatch);
  const patchPath = decodeString(obj.patchPath);
  return {
    reliable,
    reason,
    changedPaths,
    stats,
    inlinePatch,
    patchPath,
    truncated,
  };
}

function decodeDiffStats(value: unknown): RecoveryDiff["stats"] | undefined {
  if (!value || typeof value !== "object") {
    return undefined;
  }
  const obj = value as Record<string, unknown>;
  const filesChanged = decodeNonNegativeFiniteNumber(obj.filesChanged);
  const additions = decodeNonNegativeFiniteNumber(obj.additions);
  const deletions = decodeNonNegativeFiniteNumber(obj.deletions);
  if (filesChanged === undefined || additions === undefined || deletions === undefined) {
    return undefined;
  }
  return { filesChanged, additions, deletions };
}

function fallbackRecoveryDiff(reason: string): RecoveryDiff {
  return {
    reliable: false,
    reason,
    changedPaths: [],
    stats: { filesChanged: 0, additions: 0, deletions: 0 },
    truncated: false,
  };
}

function decodeFailure(value: unknown): RecoveryFinalizationFailure | undefined {
  if (!value || typeof value !== "object") {
    return undefined;
  }
  const obj = value as Record<string, unknown>;
  const stage = obj.stage;
  const message = decodeString(obj.message);
  const failedAt = decodeFiniteNumber(obj.failedAt);
  if (
    (stage === "missing-context-marker" || stage === "missing-final-tool-result" || stage === "diff" || stage === "summary" || stage === "commit") &&
    message &&
    failedAt !== undefined
  ) {
    return { stage, message, failedAt };
  }
  return undefined;
}

function decodeSnapshot(value: unknown): KnowledgeSnapshot | undefined {
  return isSnapshot(value) ? normalizeSnapshot(value) : undefined;
}

function decodeSafeId(value: unknown): string | undefined {
  const text = decodeString(value);
  return text && idPattern.test(text) ? text : undefined;
}

function decodeValidationToolName(value: unknown): "reloadHomeModules" | "runTestSuite" | undefined {
  return value === "reloadHomeModules" || value === "runTestSuite" ? value : undefined;
}

function decodeString(value: unknown): string | undefined {
  return typeof value === "string" ? value : undefined;
}

function decodeStringArray(value: unknown): string[] | undefined {
  return Array.isArray(value) && value.every((entry) => typeof entry === "string") ? value : undefined;
}

function decodeBoolean(value: unknown): boolean | undefined {
  return typeof value === "boolean" ? value : undefined;
}

function decodeFiniteNumber(value: unknown): number | undefined {
  return typeof value === "number" && Number.isFinite(value) ? value : undefined;
}

function decodeNonNegativeFiniteNumber(value: unknown): number | undefined {
  return typeof value === "number" && Number.isFinite(value) && value >= 0 ? value : undefined;
}

function isSessionLog(value: SessionLog | PiHost): value is SessionLog {
  return typeof (value as SessionLog).append === "function" && typeof (value as SessionLog).readActiveBranch === "function";
}

function sessionLogFromHost(host: PiHost): SessionLog {
  return {
    async append(event) {
      if (!host.appendEntry) {
        throw new Error("Session state cannot be persisted: host.appendEntry is unavailable");
      }
      await host.appendEntry({
        details: {
          [detailsKey]: event,
          hiddenFromModel: true,
        },
      });
    },
    async readActiveBranch() {
      if (!host.getActiveBranchEntries) {
        throw new Error("Session state cannot be reloaded: host.getActiveBranchEntries is unavailable");
      }
      const entries = await host.getActiveBranchEntries();
      if (!Array.isArray(entries)) {
        throw new Error("Session state cannot be reloaded: active branch is unavailable");
      }
      return entries;
    },
  };
}

function cloneState(state: FoldedSessionState): FoldedSessionState {
  return {
    knowledge: normalizeSnapshot(state.knowledge),
    recovery: cloneRecoveryState(state.recovery),
    completedRecoveries: state.completedRecoveries.map(cloneCompletedRecovery),
  };
}

function cloneRecoveryState(recovery: RecoveryState): RecoveryState {
  if (recovery.phase !== "finalizationFailed") {
    return { ...recovery };
  }
  return {
    ...recovery,
    failure: { ...recovery.failure },
  };
}

function cloneCompletedRecovery(completed: CompletedRecovery): CompletedRecovery {
  return {
    ...completed,
    tokenMetrics: completed.tokenMetrics ? { ...completed.tokenMetrics } : undefined,
    diff: {
      ...completed.diff,
      changedPaths: [...completed.diff.changedPaths],
      stats: { ...completed.diff.stats },
    },
  };
}
