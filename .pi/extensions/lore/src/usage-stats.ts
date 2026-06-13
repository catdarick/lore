import { estimateCompletedRecoveryTokenMetrics, planCompletedRecoveryRanges } from "./context-projection.ts";
import { estimateLoreToolResultTokens } from "./tool-result-text.ts";
import type { CompletedRecovery, PiEntry } from "./types.ts";
import type {
  EstimatedUsageBucket,
  LoreRecoveryUsage,
  LoreToolUsage,
  LoreUsageStats,
} from "./usage-types.ts";

export const loreUsageStatsCustomType = "lore-usage-stats";
const unknownLoreToolName = "unknown Lore tool";
export type { EstimatedUsageBucket, LoreRecoveryUsage, LoreToolUsage, LoreUsageStats } from "./usage-types.ts";

export function analyzeLoreUsage(input: {
  entries: PiEntry[];
  completedRecoveries: CompletedRecovery[];
  registeredToolNames: string[];
}): LoreUsageStats {
  const registeredTools = new Set(input.registeredToolNames);
  const completedRecoveries = uniqueCompletedRecoveries(input.completedRecoveries);
  const warnings: string[] = [];
  if (completedRecoveries.length !== input.completedRecoveries.length) {
    warnings.push("Duplicate completed recoveries were ignored by recovery ID");
  }

  const rangePlan = planCompletedRecoveryRanges(input.entries, completedRecoveries);
  warnings.push(...rangePlan.warnings);
  const ranges = rangePlan.ranges;
  const toolCalls = indexAssistantToolCalls(input.entries);
  const byTool = new Map<string, LoreToolUsage>();

  input.entries.forEach((entry, index) => {
    if (entry.role !== "toolResult") {
      return;
    }
    const hasLoreMetadata = hasLoreDetails(entry);
    const toolName = resolveToolName(entry, toolCalls) ?? (hasLoreMetadata ? unknownLoreToolName : undefined);
    if (!toolName) {
      return;
    }
    if (toolName === unknownLoreToolName) {
      warnings.push(`Lore tool result could not be matched to a tool name at entry ${entry.id ?? index}`);
    }
    if (!registeredTools.has(toolName) && !hasLoreMetadata) {
      return;
    }

    const usage = ensureToolUsage(byTool, toolName);
    const bucket = isInRange(index, ranges) ? usage.summarizedRecovery : usage.main;
    bucket.calls += 1;
    bucket.tokens += estimateLoreToolResultTokens(entry.content);
  });

  const tools = [...byTool.values()].sort((a, b) => {
    const tokenDelta = totalTokens(b) - totalTokens(a);
    return tokenDelta !== 0 ? tokenDelta : a.toolName.localeCompare(b.toolName);
  });
  const totals = {
    main: sumBucket(tools.map((tool) => tool.main)),
    summarizedRecovery: sumBucket(tools.map((tool) => tool.summarizedRecovery)),
  };
  const recovery = summarizeRecoveries(input.entries, completedRecoveries, warnings);

  return {
    tools,
    totals,
    recovery,
    warnings,
    estimated: true,
  };
}

export function formatLoreUsageStats(stats: LoreUsageStats): string {
  const lines = [
    "Lore tool-result context statistics (estimated, ~4 characters/token)",
    "",
  ];

  if (stats.tools.length === 0) {
    lines.push("No completed Lore tool results found on the active branch.", "");
  } else {
    const nameWidth = Math.max("Tool".length, ...stats.tools.map((tool) => tool.toolName.length));
    const mainWidth = "Main / not Lore-summarized".length;
    const recoveryWidth = "Recovery-summarized".length;
    lines.push(
      `${pad("Tool", nameWidth)}  ${pad("Main / not Lore-summarized", mainWidth)}  ${pad("Recovery-summarized", recoveryWidth)}  Total`,
    );
    for (const tool of stats.tools) {
      lines.push(
        `${pad(tool.toolName, nameWidth)}  ${pad(formatBucket(tool.main), mainWidth)}  ${pad(formatBucket(tool.summarizedRecovery), recoveryWidth)}  ${formatBucket(totalBucket(tool))}`,
      );
    }
    lines.push(
      `${pad("Total", nameWidth)}  ${pad(formatBucket(stats.totals.main), mainWidth)}  ${pad(formatBucket(stats.totals.summarizedRecovery), recoveryWidth)}  ${formatBucket({
        calls: stats.totals.main.calls + stats.totals.summarizedRecovery.calls,
        tokens: stats.totals.main.tokens + stats.totals.summarizedRecovery.tokens,
      })}`,
      "",
    );
  }

  const coverage = stats.recovery.completedRecoveries - stats.recovery.missingMetricsRecoveries;
  lines.push(
    "Recovery summaries",
    `Completed recoveries:             ${formatNumber(stats.recovery.completedRecoveries)}`,
    `Original context summarized:      ${formatTokens(stats.recovery.originalTokensSummarized)}`,
    `Summary replacements:             ${formatTokens(stats.recovery.summaryReplacementTokens)}`,
    `Estimated context reduction:      ${formatTokens(stats.recovery.estimatedReductionTokens)}`,
    `Lore-tool portion summarized:     ${formatTokens(stats.totals.summarizedRecovery.tokens)}`,
  );
  if (stats.recovery.missingMetricsRecoveries > 0) {
    lines.push(
      `Coverage:                        ${coverage} of ${stats.recovery.completedRecoveries} completed recoveries; ${stats.recovery.missingMetricsRecoveries} legacy recovery unavailable`,
    );
  }
  if (stats.warnings.length > 0) {
    lines.push("", "Warnings", ...stats.warnings.map((warning) => `- ${warning}`));
  }
  lines.push(
    "",
    "Estimates count Lore tool-result text in the active branch only.",
    "They do not include user/assistant text, tool-call arguments, system prompts,",
    "provider billing, retries, cache effects, or Pi compaction attribution.",
  );
  return lines.join("\n");
}

function summarizeRecoveries(entries: PiEntry[], completedRecoveries: CompletedRecovery[], warnings: string[]): LoreRecoveryUsage {
  let originalTokensSummarized = 0;
  let summaryReplacementTokens = 0;
  let missingMetricsRecoveries = 0;

  for (const completed of completedRecoveries) {
    const metrics = completed.tokenMetrics ?? estimateCompletedRecoveryTokenMetrics(entries, completed);
    if (!metrics) {
      missingMetricsRecoveries += 1;
      warnings.push(`Token metrics unavailable for completed recovery: ${completed.recoveryId}`);
      continue;
    }
    originalTokensSummarized += metrics.originalRecoveryTokens;
    summaryReplacementTokens += metrics.summaryReplacementTokens;
  }

  return {
    completedRecoveries: completedRecoveries.length,
    originalTokensSummarized,
    summaryReplacementTokens,
    estimatedReductionTokens: originalTokensSummarized - summaryReplacementTokens,
    missingMetricsRecoveries,
  };
}

function uniqueCompletedRecoveries(completedRecoveries: CompletedRecovery[]): CompletedRecovery[] {
  const seen = new Set<string>();
  const result: CompletedRecovery[] = [];
  for (const completed of completedRecoveries) {
    if (seen.has(completed.recoveryId)) {
      continue;
    }
    seen.add(completed.recoveryId);
    result.push(completed);
  }
  return result;
}

function indexAssistantToolCalls(entries: PiEntry[]): Map<string, string> {
  const result = new Map<string, string>();
  for (const entry of entries) {
    if (entry.role !== "assistant") {
      continue;
    }
    collectToolCalls(entry.content, result);
  }
  return result;
}

function collectToolCalls(value: unknown, result: Map<string, string>): void {
  if (Array.isArray(value)) {
    for (const item of value) {
      collectToolCalls(item, result);
    }
    return;
  }
  if (!value || typeof value !== "object") {
    return;
  }
  const obj = value as Record<string, unknown>;
  const id = stringValue(obj.id) ?? stringValue(obj.toolCallId) ?? stringValue(obj.call_id);
  const name = stringValue(obj.name) ?? stringValue(obj.toolName);
  if (id && name && isToolCallLike(obj)) {
    result.set(id, name);
  }
  for (const nested of Object.values(obj)) {
    collectToolCalls(nested, result);
  }
}

function isToolCallLike(obj: Record<string, unknown>): boolean {
  const type = stringValue(obj.type);
  return type === undefined || type === "toolCall" || type === "tool_call" || type === "function_call";
}

function resolveToolName(entry: PiEntry, toolCalls: Map<string, string>): string | undefined {
  const direct = stringValue(entry.toolName);
  if (direct) {
    return direct;
  }
  const toolCallId = stringValue(entry.toolCallId);
  return toolCallId ? toolCalls.get(toolCallId) : undefined;
}

function hasLoreDetails(entry: PiEntry): boolean {
  if (!entry.details || typeof entry.details !== "object") {
    return false;
  }
  return "lore" in (entry.details as Record<string, unknown>);
}

function ensureToolUsage(byTool: Map<string, LoreToolUsage>, toolName: string): LoreToolUsage {
  const existing = byTool.get(toolName);
  if (existing) {
    return existing;
  }
  const next: LoreToolUsage = {
    toolName,
    main: { calls: 0, tokens: 0 },
    summarizedRecovery: { calls: 0, tokens: 0 },
  };
  byTool.set(toolName, next);
  return next;
}

function isInRange(index: number, ranges: Array<{ startIndex: number; endIndex: number }>): boolean {
  return ranges.some((range) => index >= range.startIndex && index <= range.endIndex);
}

function sumBucket(buckets: EstimatedUsageBucket[]): EstimatedUsageBucket {
  return buckets.reduce(
    (total, bucket) => ({
      calls: total.calls + bucket.calls,
      tokens: total.tokens + bucket.tokens,
    }),
    { calls: 0, tokens: 0 },
  );
}

function totalBucket(tool: LoreToolUsage): EstimatedUsageBucket {
  return {
    calls: tool.main.calls + tool.summarizedRecovery.calls,
    tokens: tool.main.tokens + tool.summarizedRecovery.tokens,
  };
}

function totalTokens(tool: LoreToolUsage): number {
  return tool.main.tokens + tool.summarizedRecovery.tokens;
}

function formatBucket(bucket: EstimatedUsageBucket): string {
  return `${formatNumber(bucket.calls)} ${bucket.calls === 1 ? "call" : "calls"} / ${formatTokens(bucket.tokens)}`;
}

function formatTokens(tokens: number): string {
  return `~${formatNumber(tokens)}`;
}

function formatNumber(value: number): string {
  return Math.round(value).toLocaleString("en-US");
}

function pad(text: string, width: number): string {
  return text.padEnd(width, " ");
}

function stringValue(value: unknown): string | undefined {
  return typeof value === "string" && value.length > 0 ? value : undefined;
}
