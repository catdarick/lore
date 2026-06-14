import { eventFromEntry } from "./session-state.ts";
import type { CompletedRecovery, PiEntry, RecoveryTokenMetrics } from "./types.ts";
import { estimateTokens, stableStringify, textFromEntry } from "./util.ts";

export type CompletedRecoveryRange = {
  recoveryId: string;
  startIndex: number;
  endIndex: number;
};

export function projectCompletedRecoveries(entries: PiEntry[], completedRecoveries: CompletedRecovery[]): PiEntry[] {
  const planned = planCompletedRecoveryRanges(entries, completedRecoveries);
  let projected = [...entries];
  const completedById = new Map(completedRecoveries.map((completed) => [completed.recoveryId, completed]));
  for (const range of [...planned.ranges].sort((a, b) => b.startIndex - a.startIndex)) {
    const completed = completedById.get(range.recoveryId);
    if (completed) {
      projected = replaceRange(projected, completed, range.startIndex, range.endIndex);
    }
  }
  return projected.filter((entry) => !isUiOnlyMarker(entry));
}

export function planCompletedRecoveryRanges(
  entries: PiEntry[],
  completedRecoveries: CompletedRecovery[],
): { ranges: CompletedRecoveryRange[]; unresolvedRecoveryIds: string[]; warnings: string[] } {
  const ranges: CompletedRecoveryRange[] = [];
  const unresolvedRecoveryIds: string[] = [];
  const warnings: string[] = [];

  const seen = new Set<string>();
  const sorted = [...completedRecoveries].sort((a, b) => (a.completedAt ?? 0) - (b.completedAt ?? 0));
  for (const completed of sorted) {
    if (seen.has(completed.recoveryId)) {
      warnings.push(`Duplicate completed recovery ignored: ${completed.recoveryId}`);
      continue;
    }
    seen.add(completed.recoveryId);
    const range = resolveCompletedRecoveryRange(entries, completed);
    if (!range) {
      unresolvedRecoveryIds.push(completed.recoveryId);
      warnings.push(`Completed recovery range could not be resolved: ${completed.recoveryId}`);
      continue;
    }
    ranges.push(range);
  }

  ranges.sort((a, b) => a.startIndex - b.startIndex || a.endIndex - b.endIndex);
  const validRanges: CompletedRecoveryRange[] = [];
  let previous: CompletedRecoveryRange | undefined;
  for (const range of ranges) {
    if (previous && range.startIndex <= previous.endIndex) {
      warnings.push(
        `Completed recovery range overlaps another range and was ignored: ${range.recoveryId}`,
      );
      unresolvedRecoveryIds.push(range.recoveryId);
      continue;
    }
    validRanges.push(range);
    previous = range;
  }

  return { ranges: validRanges, unresolvedRecoveryIds, warnings };
}

export function estimateCompletedRecoveryTokenMetrics(
  entries: PiEntry[],
  completed: CompletedRecovery,
  range = resolveCompletedRecoveryRange(entries, completed),
): RecoveryTokenMetrics | undefined {
  if (!range) {
    return undefined;
  }
  const originalText = entries.slice(range.startIndex, range.endIndex + 1).map(entryModelText).join("\n\n");
  return {
    originalRecoveryTokens: estimateTokens(originalText),
    summaryReplacementTokens: estimateTokens(completed.contextReplacement),
    estimated: true,
  };
}

export function resolveCompletedRecoveryRange(
  entries: PiEntry[],
  completed: CompletedRecovery,
): CompletedRecoveryRange | undefined {
  const callEnd = findToolResultIndex(entries, completed.finalValidationToolCallId);
  const entryStart = completed.startEntryId === undefined
    ? -1
    : entries.findIndex((entry) => entry.id === completed.startEntryId);
  const callStart = findValidationStartIndex(entries, completed.startValidationToolCallId);
  const start = entryStart >= 0 ? entryStart : callStart >= 0 ? callStart : -1;
  if (start >= 0 && callEnd >= start) {
    const replaceStart = projectionReplaceStart(entries, completed, start);
    const preserveStart = preserveStartIndex(entries, completed, callEnd);
    const replaceEnd = preserveStart - 1;
    if (replaceEnd < replaceStart) {
      return undefined;
    }
    return { recoveryId: completed.recoveryId, startIndex: replaceStart, endIndex: replaceEnd };
  }
  return undefined;
}

function projectionReplaceStart(entries: PiEntry[], completed: CompletedRecovery, fallbackStart: number): number {
  const startValidationToolCallId = completed.startValidationToolCallId;
  if (!startValidationToolCallId) {
    return fallbackStart;
  }
  const startCall = findAssistantToolCallIndex(entries, startValidationToolCallId);
  if (startCall >= 0 && startCall <= fallbackStart) {
    return startCall;
  }
  return fallbackStart;
}

function preserveStartIndex(entries: PiEntry[], completed: CompletedRecovery, fallbackEnd: number): number {
  const finalValidationToolCallId = completed.finalValidationToolCallId;
  if (!finalValidationToolCallId) {
    return fallbackEnd + 1;
  }

  const finalCall = findAssistantToolCallIndex(entries, finalValidationToolCallId);
  if (finalCall >= 0 && finalCall <= fallbackEnd) {
    return finalCall;
  }

  const finalResult = findToolResultIndex(entries, finalValidationToolCallId);
  if (finalResult >= 0 && finalResult <= fallbackEnd) {
    const nearestAssistant = findNearestAssistantIndexBefore(entries, finalResult);
    if (nearestAssistant >= 0) {
      return nearestAssistant;
    }
    return finalResult;
  }

  return fallbackEnd + 1;
}

function replaceRange(entries: PiEntry[], completed: CompletedRecovery, start: number, end: number): PiEntry[] {
  const replacement: PiEntry = {
    id: `lore-projection-${completed.recoveryId}`,
    role: "assistant",
    type: "message",
    content: completed.contextReplacement,
    details: {
      loreProjection: {
        recoveryId: completed.recoveryId,
      },
    },
  };
  return [...entries.slice(0, start), replacement, ...entries.slice(end + 1)];
}

export function isUiOnlyMarker(entry: PiEntry): boolean {
  const event = eventFromEntry(entry);
  return Boolean(event);
}

function findToolResultIndex(entries: PiEntry[], toolCallId: string | undefined): number {
  if (!toolCallId) {
    return -1;
  }
  return entries.findIndex((entry) => roleOf(entry) === "toolResult" && toolCallIdOf(entry) === toolCallId);
}

function findValidationStartIndex(entries: PiEntry[], toolCallId: string): number {
  const assistant = findAssistantToolCallIndex(entries, toolCallId);
  if (assistant >= 0) {
    return assistant;
  }
  return findToolResultIndex(entries, toolCallId);
}

function findAssistantToolCallIndex(entries: PiEntry[], toolCallId: string): number {
  return entries.findIndex((entry) => roleOf(entry) === "assistant" && entryContainsToolCallId(entry, toolCallId));
}

function entryContainsToolCallId(entry: PiEntry, toolCallId: string): boolean {
  return valueContainsToolCallId(contentOf(entry), toolCallId);
}

function valueContainsToolCallId(value: unknown, toolCallId: string): boolean {
  if (Array.isArray(value)) {
    return value.some((item) => valueContainsToolCallId(item, toolCallId));
  }
  if (!value || typeof value !== "object") {
    return false;
  }
  const obj = value as Record<string, unknown>;
  for (const [key, nested] of Object.entries(obj)) {
    if ((key === "id" || key === "toolCallId" || key === "call_id") && nested === toolCallId) {
      return true;
    }
    if (valueContainsToolCallId(nested, toolCallId)) {
      return true;
    }
  }
  return false;
}

function findNearestAssistantIndexBefore(entries: PiEntry[], index: number): number {
  for (let i = index - 1; i >= 0; i -= 1) {
    const entry = entries[i];
    if (entry && roleOf(entry) === "assistant") {
      return i;
    }
  }
  return -1;
}

function entryModelText(entry: PiEntry): string {
  const text = textFromEntry({ ...entry, content: contentOf(entry) });
  if (text) {
    return text;
  }
  return stableStringify({
    role: entry.role,
    type: entry.type,
    content: entry.content,
    toolCallId: entry.toolCallId,
    toolName: entry.toolName,
    details: entry.details,
  });
}

function roleOf(entry: PiEntry): unknown {
  return entry.role ?? rawMessageRecord(entry)?.role;
}

function toolCallIdOf(entry: PiEntry): unknown {
  return entry.toolCallId ?? rawMessageRecord(entry)?.toolCallId;
}

function contentOf(entry: PiEntry): unknown {
  return entry.content ?? rawMessageRecord(entry)?.content;
}

function rawMessageRecord(entry: PiEntry): Record<string, unknown> | undefined {
  const message = entry.message;
  return message && typeof message === "object" ? (message as Record<string, unknown>) : undefined;
}
