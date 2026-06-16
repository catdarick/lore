import type { PiHost } from "./types.ts";

export const loreExtensionStatusKey = "lore-extension";

export type LoreExtensionStatus =
  | "preparing"
  | "setupRequired"
  | "downloading"
  | "starting"
  | "active"
  | "disabled"
  | "paused"
  | "unavailable";

export type LoreExtensionStatusTone = "info" | "warning" | "error" | "pending";

type LoreExtensionStatusOptions = {
  loreStatus: LoreExtensionStatus;
  tone: "info" | "warning" | "error";
};

const statusPresentation: Record<LoreExtensionStatus, { text: string; tone: LoreExtensionStatusTone }> = {
  preparing: { text: "preparing…", tone: "pending" },
  setupRequired: { text: "setup required", tone: "warning" },
  downloading: { text: "downloading…", tone: "pending" },
  starting: { text: "starting…", tone: "pending" },
  active: { text: "active", tone: "info" },
  disabled: { text: "disabled", tone: "info" },
  paused: { text: "paused", tone: "warning" },
  unavailable: { text: "unavailable", tone: "error" },
};

const themeRole: Record<LoreExtensionStatusTone, string> = {
  info: "text",
  warning: "warning",
  error: "error",
  pending: "accent",
};

const ansiForeground: Record<LoreExtensionStatusTone | "label", number> = {
  label: 34,
  info: 97,
  warning: 33,
  error: 31,
  pending: 36,
};

export function loreExtensionStatusText(status: LoreExtensionStatus): string {
  return `Lore: ${statusPresentation[status].text}`;
}

export function renderLoreExtensionStatus(status: LoreExtensionStatus, theme?: unknown): string {
  const presentation = statusPresentation[status];
  return [
    styleText(theme, "accent", "Lore:", ansiForeground.label),
    styleText(theme, themeRole[presentation.tone], presentation.text, ansiForeground[presentation.tone]),
  ].join(" ");
}

export function loreExtensionStatusFromOptions(options: unknown): LoreExtensionStatus | undefined {
  if (!options || typeof options !== "object") return undefined;
  const status = (options as { loreStatus?: unknown }).loreStatus;
  return isLoreExtensionStatus(status) ? status : undefined;
}

export async function showLoreExtensionStatus(host: Pick<PiHost, "setStatus">, status: LoreExtensionStatus): Promise<void> {
  const presentation = statusPresentation[status];
  const options: LoreExtensionStatusOptions = {
    loreStatus: status,
    tone: presentation.tone === "pending" ? "warning" : presentation.tone,
  };
  try {
    await host.setStatus?.(loreExtensionStatusKey, loreExtensionStatusText(status), options);
  } catch {
    // Status rendering is best-effort and must never affect Lore startup.
  }
}

function isLoreExtensionStatus(value: unknown): value is LoreExtensionStatus {
  return typeof value === "string" && Object.prototype.hasOwnProperty.call(statusPresentation, value);
}

function styleText(theme: unknown, role: string, text: string, ansiCode: number): string {
  const themed = tryThemeForeground(theme, role, text);
  return themed ?? `\u001b[${ansiCode}m${text}\u001b[39m`;
}

function tryThemeForeground(theme: unknown, role: string, text: string): string | undefined {
  if (!theme || typeof theme !== "object") return undefined;
  const foreground = (theme as { fg?: unknown }).fg;
  if (typeof foreground !== "function") return undefined;
  try {
    const result = (foreground as (name: string, value: string) => unknown)(role, text);
    return typeof result === "string" ? result : undefined;
  } catch {
    return undefined;
  }
}
