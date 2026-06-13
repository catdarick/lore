import { estimateTokens } from "./util.ts";

export function loreToolResultContextText(content: unknown): string {
  const extracted = extractToolResultText(content);
  if (extracted.textParts.length > 0) {
    return extracted.textParts.join("\n\n").trim();
  }
  return extracted.nonTextParts.join("\n\n").trim();
}

export function loreToolResultDisplayText(content: unknown): string {
  const extracted = extractToolResultText(content);
  if (extracted.textParts.length > 0) {
    return [...new Set(extracted.textParts)].join("\n\n").trim();
  }
  return [...new Set(extracted.nonTextParts)].join("\n\n").trim();
}

export function estimateLoreToolResultTokens(content: unknown): number {
  return estimateTokens(loreToolResultContextText(content));
}

function extractToolResultText(content: unknown): { textParts: string[]; nonTextParts: string[] } {
  if (typeof content === "string") {
    const value = content.trim();
    return { textParts: value.length > 0 ? [value] : [], nonTextParts: [] };
  }
  if (!Array.isArray(content)) {
    return { textParts: [], nonTextParts: [] };
  }

  const textParts: string[] = [];
  const nonTextParts: string[] = [];
  for (const part of content) {
    if (!part || typeof part !== "object") {
      continue;
    }
    const record = part as { type?: unknown; text?: unknown };
    if (record.type === "text" && typeof record.text === "string") {
      const value = record.text.trim();
      if (value.length > 0) {
        textParts.push(value);
      }
      continue;
    }
    if (typeof record.type === "string") {
      nonTextParts.push(`[${record.type} content]`);
    }
  }

  return { textParts, nonTextParts };
}
