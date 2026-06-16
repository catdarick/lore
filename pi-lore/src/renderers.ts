import { recoverySummaryCustomType } from "./ui.ts";
import { loreUsageStatsCustomType } from "./usage-stats.ts";

type RendererApi = {
  registerMessageRenderer?: (customType: string, renderer: (message: unknown, options: unknown, theme: unknown) => unknown) => void;
};

export function registerRecoverySummaryRenderer(pi: PiExtensionApi): void {
  pi.registerMessageRenderer?.(recoverySummaryCustomType, (message, options, theme) => {
    const text = recoverySummaryRenderText(message, options, theme);
    return new StaticTextComponent(text);
  });
}

export function registerUsageStatsRenderer(pi: PiExtensionApi): void {
  pi.registerMessageRenderer?.(loreUsageStatsCustomType, (message, _options, theme) => {
    const title = styleTitle(theme, "Lore context statistics (estimated)");
    const content = readMessageContent(message);
    const body = content.startsWith("Lore ") ? content.replace(/^.*(?:\r?\n){2}/, "") : content;
    return new StaticTextComponent([title, styleMutedLines(theme, body)].join("\n"));
  });
}

function recoverySummaryRenderText(message: unknown, options: unknown, theme: unknown): string {
  const expanded = readExpanded(options);
  const content = readMessageContent(message);
  const title = styleTitle(theme, "Lore recovery context summary");
  const body = expanded ? content : previewRecoverySummary(content);
  return [title, styleMutedLines(theme, body)].join("\n");
}

function readExpanded(options: unknown): boolean {
  if (!options || typeof options !== "object") {
    return false;
  }
  return (options as { expanded?: unknown }).expanded === true;
}

function readMessageContent(message: unknown): string {
  if (!message || typeof message !== "object") {
    return "";
  }
  const content = (message as { content?: unknown }).content;
  if (typeof content === "string") {
    return content.trim();
  }
  return "";
}

function previewRecoverySummary(content: string): string {
  if (!content) {
    return "(no summary content)";
  }
  const lines = content
    .split(/\r?\n/)
    .map((line) => line.trimEnd());
  const nonEmptyStart = lines.findIndex((line) => line.trim().length > 0);
  const start = nonEmptyStart >= 0 ? nonEmptyStart : 0;
  const preview = lines.slice(start, start + 5).join("\n").trim();
  if (lines.length <= start + 5 && preview.length <= 500) {
    return preview;
  }
  const clipped = preview.length > 500 ? `${preview.slice(0, 499)}…` : preview;
  return `${clipped}\n…`;
}

function styleTitle(theme: unknown, text: string): string {
  const bold = tryTheme(theme, "bold", [text]);
  if (typeof bold === "string") {
    return bold;
  }
  return `\u001b[1m${text}\u001b[22m`;
}

function styleMuted(theme: unknown, text: string): string {
  const muted = tryTheme(theme, "fg", ["dim", text]);
  if (typeof muted === "string") {
    return muted;
  }
  return `\u001b[90m${text}\u001b[39m`;
}

function styleMutedLines(theme: unknown, text: string): string {
  return text
    .split(/\r?\n/)
    .map((line) => styleMuted(theme, line))
    .join("\n");
}

function tryTheme(theme: unknown, method: string, args: unknown[]): unknown {
  if (!theme || typeof theme !== "object") {
    return undefined;
  }
  const fn = (theme as Record<string, unknown>)[method];
  if (typeof fn !== "function") {
    return undefined;
  }
  try {
    return (fn as (...params: unknown[]) => unknown)(...args);
  } catch {
    return undefined;
  }
}

class StaticTextComponent {
  private text: string;

  constructor(text: string) {
    this.text = text;
  }

  render(width: number): string[] {
    if (this.text.length === 0) {
      return [];
    }
    const safeWidth = Number.isFinite(width) && width > 0 ? Math.floor(width) : 1;
    const lines = this.text.split(/\r?\n/);
    const wrapped: string[] = [];
    for (const line of lines) {
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

  invalidate(): void {
    // stateless
  }
}

