import { readFile } from "node:fs/promises";
import type { CompletedRecovery, LoreConfig, PiHost, RecoveryDiff, RecoveryState } from "./types.ts";

export async function generateRecoverySummary(input: {
  host: PiHost;
  config: LoreConfig;
  recovery: Exclude<RecoveryState, { phase: "inactive" }>;
  contextMessages?: unknown[];
  diff: RecoveryDiff;
}): Promise<Pick<CompletedRecovery, "summary" | "contextReplacement">> {
  const diffText = await renderDiffForContext(input.diff);
  const summaryMode = inferSummaryMode(input.recovery);
  const formatInstructions = recoverySummaryFormatInstructions(summaryMode);
  const markerPrompt = [
    `Summarize actions done that appears ONLY AFTER ${input.recovery.contextMarker}.`,
    "Return ONLY Markdown in the required format. Do not add any extra text.",
    "",
    formatInstructions,
  ].join("\n");

  if (!input.host.generateTextFromMessages) {
    throw new Error("Recovery summarization requires host.generateTextFromMessages");
  }
  if (!input.contextMessages || input.contextMessages.length === 0) {
    throw new Error("Recovery summarization requires context messages with recovery marker");
  }
  if (!contextMessagesContainMarker(input.contextMessages, input.recovery.contextMarker)) {
    throw new Error("Recovery summarization marker was not found in current context messages");
  }

  const summary = await input.host.generateTextFromMessages({
    messages: input.contextMessages,
    prompt: markerPrompt,
    timeoutMs: input.config.summaryTimeoutMs,
    tools: false,
  });
  const contextReplacement = [
    "[[LORE_FIXES_APPLIED]]",
    "",
    summary.trim(),
    "",
    "Diff:",
    diffText,
  ]
    .join("\n")
    .trim();
  return { summary: summary.trim(), contextReplacement };
}

export async function renderDiffForContext(diff: RecoveryDiff): Promise<string> {
  let patch = diff.inlinePatch ?? "";
  if (!patch && diff.patchPath) {
    try {
      patch = await readFile(diff.patchPath, "utf8");
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      patch = `Unable to read recovery patch at ${diff.patchPath}: ${message}`;
    }
  }

  if (!patch.trim()) {
    return renderDiffOverview(diff, "No textual diff was captured during recovery.");
  }
  const boundedPatch = patch.length > 0 ? truncatePatch(patch, diff) : patch;
  if (!diff.reliable) {
    const reason = diff.reason ?? "unavailable";
    return [`Warning: diff may be partial (${reason}).`, "", renderDiffOverview(diff, boundedPatch)].join("\n");
  }
  return renderDiffOverview(diff, boundedPatch);
}

function recoverySummaryFormatInstructions(mode: SummaryMode): string {
  if (mode === "errors") {
    return [
      "Format:",
      "```markdown",
      "### Errors and fixes",
      "- `<error>` -> `<fix method>`",
      "```",
      "",
      "Rules:",
      "- Include ONLY error + fix method bullets (no extra narrative).",
      "- Do NOT include any other sections.",
      "- Keep trivial items short; provide more detail only for non-trivial fixes.",
    ].join("\n");
  }

  return [
    "Format:",
    "```markdown",
    "### Failed tests, causes, and fixes",
    "- `<test name or suite>` — cause: `<why it failed>`; fix: `<how it was fixed>`",
    "",
    "### Unresolved issues",
    "- `<known unresolved issue or still-failing test>`",
    "```",
    "",
    "Rules:",
    "- Include only actually failed tests from the recovery context.",
    "- Omit '### Unresolved issues' entirely if there are no known unresolved issues.",
    "- Do NOT include '### Errors and fixes' in test-recovery mode.",
    "- Keep trivial items short; provide more detail only for non-trivial fixes.",
  ].join("\n");
}

type SummaryMode = "errors" | "tests";

function inferSummaryMode(recovery: Exclude<RecoveryState, { phase: "inactive" }>): SummaryMode {
  if (recovery.startValidationToolName === "runTestSuite") {
    return "tests";
  }
  if (recovery.reason.includes("runTestSuite")) {
    return "tests";
  }
  return "errors";
}

function renderDiffOverview(diff: RecoveryDiff, patch: string): string {
  const stats = `${diff.stats.filesChanged} files changed, +${diff.stats.additions}/-${diff.stats.deletions}`;
  const paths = renderChangedPaths(diff.changedPaths);
  return [stats, paths, "", patch].join("\n").trim();
}

function renderChangedPaths(paths: string[]): string {
  if (paths.length === 0) {
    return "Changed paths: none";
  }
  const maxChars = 8_000;
  const lines = ["Changed paths:"];
  let used = lines[0].length + 1;
  for (let index = 0; index < paths.length; index += 1) {
    const line = `- ${paths[index]}`;
    if (used + line.length + 1 > maxChars) {
      lines.push(`- ${paths.length - index} additional paths omitted`);
      break;
    }
    lines.push(line);
    used += line.length + 1;
  }
  return lines.join("\n");
}

function truncatePatch(patch: string, diff: RecoveryDiff): string {
  if (!diff.truncated) {
    return patch;
  }
  const maxChars = Math.max(0, Math.min(patch.length, 20_000));
  return [
    patch.slice(0, maxChars),
    "",
    diff.patchPath
      ? `[Patch excerpt truncated. Full patch was retained until recovery completion or abandon.]`
      : `[Patch excerpt truncated. Full patch artifact is unavailable.]`,
  ].join("\n");
}

function contextMessagesContainMarker(messages: unknown[], marker: string): boolean {
  for (const message of messages) {
    if (!message || typeof message !== "object") {
      continue;
    }
    const content = (message as { content?: unknown }).content;
    if (typeof content === "string" && content.includes(marker)) {
      return true;
    }
    if (Array.isArray(content)) {
      const hasMarker = content.some((part) => {
        if (typeof part === "string") {
          return part.includes(marker);
        }
        if (!part || typeof part !== "object") {
          return false;
        }
        const text = (part as { text?: unknown }).text;
        return typeof text === "string" && text.includes(marker);
      });
      if (hasMarker) {
        return true;
      }
    }
  }
  return false;
}
