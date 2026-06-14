import type { PiComponent, PiToolRenderContext, PiToolRenderOptions, PiToolResult } from "./types.ts";
import { loreToolResultDisplayText, estimateLoreToolResultTokens } from "./tool-result-text.ts";
import { stableStringify } from "./util.ts";

const MAX_PREVIEW_CHARS = 180;
const COLLAPSED_PREVIEW_MAX_LINES = 6;
const COLLAPSED_PREVIEW_MAX_CHARS = 240;

export function renderLoreToolCall(toolName: string, args: unknown, theme: unknown, context: PiToolRenderContext): PiComponent {
  const invocation = renderInvocation(toolName, args, theme);
  const summary = readCallSummary(context);
  const header = summary ? `${invocation} ${styleMuted(theme, `– ${summary}`)}` : invocation;
  return textComponent(context.lastComponent, header);
}

export function renderLoreToolResult(
  toolName: string,
  result: PiToolResult,
  options: PiToolRenderOptions,
  theme: unknown,
  context: PiToolRenderContext,
): PiComponent {
  const invocation = renderInvocation(toolName, context.args, theme);

  if (options.isPartial) {
    const summary = "running…";
    const update = writeCallSummary(context, summary);
    const text = update === "changed" ? "" : `${invocation} ${styleMuted(theme, `– ${summary}`)}`;
    return textComponent(context.lastComponent, text);
  }

  const outputText = loreToolResultDisplayText(result.content);
  const tokenCount = estimateLoreToolResultTokens(result.content);
  const summary = `completed (~${tokenCount} tokens)`;
  const update = writeCallSummary(context, summary);
  const summaryLine = `${invocation} ${styleMuted(theme, `– ${summary}`)}`;
  const collapsedPreview = previewOutput(outputText);

  if (!options.expanded) {
    if (update === "changed") {
      return textComponent(context.lastComponent, "");
    }
    if (update !== "unavailable") {
      return textComponent(context.lastComponent, styleMutedLines(theme, collapsedPreview));
    }
    return textComponent(context.lastComponent, [summaryLine, styleMutedLines(theme, collapsedPreview)].join("\n"));
  }

  const structuredContent = extractStructuredContent(result.details);
  const outputLabel = styleMuted(theme, "Output:");
  const structuredLabel = styleMuted(theme, "Structured Content:");
  if (update === "changed") {
    return textComponent(context.lastComponent, "");
  }

  const sections: string[] = update !== "unavailable"
    ? [outputLabel, styleMutedLines(theme, outputText || "(no text output)")]
    : [summaryLine, "", outputLabel, styleMutedLines(theme, outputText || "(no text output)")];

  if (structuredContent !== undefined) {
    sections.push("", structuredLabel, formatJsonBlock(structuredContent));
  }
  return textComponent(context.lastComponent, sections.join("\n"));
}

function previewOutput(outputText: string): string {
  if (!outputText) {
    return "(no text output)";
  }
  const rawLines = outputText.split(/\r?\n/);
  if (rawLines.every((line) => line.trim().length === 0)) {
    return "(no text output)";
  }

  const previewLines: string[] = [];
  let consumedChars = 0;
  for (const line of rawLines) {
    if (previewLines.length >= COLLAPSED_PREVIEW_MAX_LINES) {
      break;
    }
    const remaining = COLLAPSED_PREVIEW_MAX_CHARS - consumedChars;
    if (remaining <= 0) {
      break;
    }
    const clipped = line.length > remaining ? `${line.slice(0, Math.max(0, remaining - 1))}…` : line;
    previewLines.push(clipped);
    consumedChars += clipped.length;
  }

  const preview = previewLines.join("\n").replace(/\s+$/g, "");
  const clippedByLines = rawLines.length > previewLines.length;
  const clippedByChars = outputText.length > preview.length;
  if (!clippedByLines && !clippedByChars) {
    return preview;
  }
  return `${preview}\n…`;
}

function readCallSummary(context: PiToolRenderContext): string | undefined {
  const state = context.state;
  if (!state || typeof state !== "object") {
    return undefined;
  }
  const value = (state as Record<string, unknown>).loreToolSummary;
  return typeof value === "string" && value.trim().length > 0 ? value : undefined;
}

function writeCallSummary(context: PiToolRenderContext, summary: string): "changed" | "unchanged" | "unavailable" {
  const state = context.state;
  if (!state || typeof state !== "object") {
    return "unavailable";
  }
  const record = state as Record<string, unknown>;
  if (record.loreToolSummary === summary) {
    return "unchanged";
  }
  record.loreToolSummary = summary;
  context.invalidate?.();
  return "changed";
}

function extractStructuredContent(details: unknown): unknown {
  if (!details || typeof details !== "object") {
    return undefined;
  }
  const lore = (details as Record<string, unknown>).lore;
  if (!lore || typeof lore !== "object") {
    return undefined;
  }
  return (lore as Record<string, unknown>).structuredContent;
}

function formatJsonBlock(value: unknown): string {
  try {
    return JSON.stringify(value ?? {}, null, 2) ?? "{}";
  } catch {
    return stableStringify(value);
  }
}

function summarizeJson(value: unknown, maxChars: number): string {
  const json = stableStringify(value ?? {});
  return truncate(json, maxChars);
}

function renderInvocation(toolName: string, args: unknown, theme: unknown): string {
  const argsPreview = summarizeJson(args, MAX_PREVIEW_CHARS);
  const styledName = styleToolName(theme, toolName);
  return argsPreview === "{}" ? styledName : `${styledName} ${styleMuted(theme, argsPreview)}`;
}

function styleToolName(theme: unknown, text: string): string {
  const themed = tryCall(theme, "bold", [text]);
  if (typeof themed === "string") {
    return themed;
  }
  return `\u001b[1m${text}\u001b[22m`;
}

function styleMuted(theme: unknown, text: string): string {
  const fg = tryCall(theme, "fg", ["dim", text]);
  if (typeof fg === "string") {
    return fg;
  }
  return `\u001b[90m${text}\u001b[39m`;
}

function styleMutedLines(theme: unknown, text: string): string {
  return text
    .split(/\r?\n/)
    .map((line) => styleMuted(theme, line))
    .join("\n");
}

function tryCall(target: unknown, method: string, args: unknown[]): unknown {
  if (!target || typeof target !== "object") {
    return undefined;
  }
  const fn = (target as Record<string, unknown>)[method];
  if (typeof fn !== "function") {
    return undefined;
  }
  try {
    return (fn as (...params: unknown[]) => unknown)(...args);
  } catch {
    return undefined;
  }
}

function truncate(text: string, maxChars: number): string {
  if (text.length <= maxChars) {
    return text;
  }
  return `${text.slice(0, Math.max(0, maxChars - 1))}…`;
}

function textComponent(lastComponent: unknown, text: string): PiComponent {
  if (lastComponent instanceof StaticTextComponent) {
    lastComponent.setText(text);
    return lastComponent;
  }
  return new StaticTextComponent(text);
}

class StaticTextComponent implements PiComponent {
  private text: string;

  constructor(text: string) {
    this.text = text;
  }

  setText(text: string): void {
    this.text = text;
  }

  render(width: number): string[] {
    return wrapLines(this.text, width);
  }

  invalidate(): void {
    // No cached width-dependent state.
  }
}

function wrapLines(text: string, width: number): string[] {
  if (text.length === 0) {
    return [];
  }
  const safeWidth = Number.isFinite(width) && width > 0 ? Math.floor(width) : 1;
  const sourceLines = text.split(/\r?\n/);
  const wrapped: string[] = [];

  for (const line of sourceLines) {
    if (line.length === 0) {
      wrapped.push("");
      continue;
    }
    for (let i = 0; i < line.length; i += safeWidth) {
      wrapped.push(line.slice(i, i + safeWidth));
    }
  }

  return wrapped;
}
