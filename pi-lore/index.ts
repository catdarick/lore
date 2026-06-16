import { createLoreExtension } from "./src/index.ts";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { loadLoreConfig } from "./src/config.ts";
import { installPiImmediateDisplayBridge, type PiImmediateDisplayOptions } from "./src/pi-immediate-display.ts";
import { recoverySummaryCustomType } from "./src/ui.ts";
import { formatLoreUsageStats, loreUsageStatsCustomType } from "./src/usage-stats.ts";
import type { PiCustomMessage, PiEntry, PiHost, PiSendMessageOptions, PiToolRegistration } from "./src/types.ts";
import type { LoreUsageStats } from "./src/usage-types.ts";

type ExtensionContext = {
  sessionManager: {
    getBranch: () => unknown[];
    getLeafId: () => string | null;
  };
  modelRegistry?: {
    getApiKeyAndHeaders?: (model: unknown) => Promise<{ ok: true; apiKey?: string; headers?: Record<string, string> } | { ok: false; error: string }>;
  };
  model?: unknown;
  ui: {
    notify: (message: string, tone?: "info" | "warning" | "error") => void;
    setStatus: (key: string, text: string | undefined) => void;
  };
};

type PiAiModule = {
  completeSimple: (
    model: unknown,
    context: {
      messages: Array<Record<string, unknown>>;
    },
    options: {
      timeoutMs: number;
      signal?: AbortSignal;
      apiKey?: string;
      headers?: Record<string, string>;
    },
  ) => Promise<{
    stopReason: string;
    errorMessage?: string;
    content: Array<{ type: string; text?: string }>;
  }>;
};

let cachedPiAiModule: PiAiModule | undefined;

type PiExtensionApi = {
  on?: (event: string, handler: (...args: unknown[]) => unknown) => void;
  registerTool?: (tool: unknown) => void;
  registerMessageRenderer?: (customType: string, renderer: (message: unknown, options: unknown, theme: unknown) => unknown) => void;
  registerCommand?: (name: string, options: unknown) => void;
  getActiveTools?: () => string[];
  getAllTools?: () => Array<{ name?: unknown }>;
  setActiveTools?: (toolNames: string[]) => void;
  appendEntry?: (customType: string, data: unknown) => void;
  getConfig?: (name?: string) => unknown;
  sendMessage?: (
    message: Pick<PiCustomMessage, "customType" | "content" | "display" | "details">,
    options?: PiSendMessageOptions | PiImmediateDisplayOptions,
  ) => void;
};

const LORE_CONTEXT_MARKER_GUIDANCE = [
  "Pi may inject system context messages into the conversation. These are not user prompts; do not reply to them or acknowledge them.",
  "There are two types of system context markers:",
  "- [[LORE_SECTION_STARTED:<id>]]: Marks the beginning of a message block that will eventually be summarized. Ignore this marker until you are explicitly instructed to summarize the section.",
  "- [[LORE_FIXES_APPLIED]]: Indicates that previous failures have been resolved and summarized. Analyze these past errors and fixes to avoid repeating failed approaches, but do not proactively re-read or scan files just to verify the fixes unless explicitly requested.",
].join("\n");

export default async function lorePiExtension(pi: PiExtensionApi): Promise<void> {
  validateRequiredPiCapabilities(pi);
  registerRecoverySummaryRenderer(pi);
  registerUsageStatsRenderer(pi);
  registerLoreSystemPromptGuidance(pi);

  let loreToolNames: string[] = [];
  let currentContext: ExtensionContext | undefined;
  let startupError: string | undefined;

  pi.on?.("session_start", (_event, ctx) => {
    currentContext = toExtensionContext(ctx) ?? currentContext;
    activateLoreTools(pi, loreToolNames);
  });
  pi.on?.("model_select", (_event, ctx) => {
    currentContext = toExtensionContext(ctx) ?? currentContext;
    activateLoreTools(pi, loreToolNames);
  });
  pi.registerCommand?.("lore-status", {
    description: "Show Lore extension registration status",
    handler: async (_args: unknown, ctx: { ui?: { notify?: (message: string, type?: string) => void } }) => {
      const activeLoreToolNames = currentActiveLoreToolNames(pi, loreToolNames);
      const message =
        startupError
          ? `Lore extension failed to start: ${startupError}`
          : activeLoreToolNames.length > 0
            ? `Lore extension active tools (${activeLoreToolNames.length}/${loreToolNames.length} registered): ${activeLoreToolNames.join(", ")}`
            : loreToolNames.length > 0
              ? `Lore extension has ${loreToolNames.length} registered tools, but none are active.`
              : "Lore extension has not registered any tools.";
      ctx.ui?.notify?.(message, startupError ? "error" : activeLoreToolNames.length > 0 ? "info" : "warning");
    },
  });
  const immediateDisplay = await installPiImmediateDisplayBridge({
    sendMessage: (message, options) => pi.sendMessage?.(message, options),
  });
  const host = adaptPiHost(pi, {
    getCurrentContext: () => currentContext,
    setCurrentContext: (next) => {
      currentContext = next;
    },
    appendDisplayMessageImmediately: immediateDisplay.appendImmediately,
  });
  host.onLoreToolsRegistered = (toolNames) => {
    loreToolNames = toolNames;
  };

  const runtime = await createLoreExtension(host);
  registerLoreSettingsCommand(pi, host, runtime, (next) => {
    currentContext = next ?? currentContext;
  });
  registerUsageStatsCommand(pi, runtime, host, (next) => {
    currentContext = next ?? currentContext;
  });
  registerLoreRestartCommand(pi, runtime, (next) => {
    currentContext = next ?? currentContext;
  });
  pi.registerCommand?.("lore-recovery-abandon", {
    description: "Abandon the active Lore recovery and retain the original messages",
    handler: async () => {
      await runtime.abandonRecovery();
    },
  });

  pi.on?.("context", async (event, ctx) => {
    currentContext = toExtensionContext(ctx) ?? currentContext;
    if (!event || typeof event !== "object") {
      return undefined;
    }
    const messages = (event as { messages?: unknown }).messages;
    if (!Array.isArray(messages)) {
      return undefined;
    }
    const projected = await runtime.processContext({
      rawMessages: messages,
      normalizedEntries: normalizePiMessages(messages),
    });
    return { messages: denormalizePiMessages(projected) };
  });

  await runtime.start();
  const state = runtime.getState();
  loreToolNames = state.registeredToolNames;
  startupError = state.startupError;
  if (!startupError) {
    activateLoreTools(pi, loreToolNames);
  }
}

export { createLoreExtension };
export type { ExtensionRuntime, PiHost } from "./src/types.ts";

function registerLoreSystemPromptGuidance(pi: PiExtensionApi): void {
  pi.on?.("before_agent_start", (event) => appendLoreContextMarkerGuidance(event));
}

function appendLoreContextMarkerGuidance(event: unknown): { systemPrompt: string } | undefined {
  if (!event || typeof event !== "object") {
    return undefined;
  }

  const systemPrompt = (event as { systemPrompt?: unknown }).systemPrompt;
  if (typeof systemPrompt !== "string") {
    return undefined;
  }

  return {
    systemPrompt: [systemPrompt, "", LORE_CONTEXT_MARKER_GUIDANCE].join("\n"),
  };
}

function registerRecoverySummaryRenderer(pi: PiExtensionApi): void {
  pi.registerMessageRenderer?.(recoverySummaryCustomType, (message, options, theme) => {
    const text = recoverySummaryRenderText(message, options, theme);
    return new StaticTextComponent(text);
  });
}

function registerUsageStatsRenderer(pi: PiExtensionApi): void {
  pi.registerMessageRenderer?.(loreUsageStatsCustomType, (message, _options, theme) => {
    const title = styleTitle(theme, "Lore context statistics (estimated)");
    const content = readMessageContent(message);
    const body = content.startsWith("Lore ") ? content.replace(/^.*(?:\r?\n){2}/, "") : content;
    return new StaticTextComponent([title, styleMutedLines(theme, body)].join("\n"));
  });
}

function registerUsageStatsCommand(
  pi: PiExtensionApi,
  runtime: { getUsageStats: () => Promise<LoreUsageStats> },
  host: Pick<PiHost, "appendDisplayMessageImmediately">,
  setCurrentContext: (next: ExtensionContext | undefined) => void,
): void {
  pi.registerCommand?.("lore-stats", {
    description: "Show estimated Lore tool context and recovery statistics",
    handler: async (_args: unknown, ctx: unknown) => {
      setCurrentContext(toExtensionContext(ctx));
      const stats = await runtime.getUsageStats();
      if (!host.appendDisplayMessageImmediately) {
        throw new Error("Lore statistics require immediate display support");
      }
      await host.appendDisplayMessageImmediately({
        customType: loreUsageStatsCustomType,
        display: true,
        content: formatLoreUsageStats(stats),
        details: {
          hiddenFromModel: true,
          generatedAt: Date.now(),
        },
      });
    },
  });
}

function registerLoreSettingsCommand(
  pi: PiExtensionApi,
  host: PiHost,
  runtime: { listAvailableToolNames: () => Promise<string[]> },
  setCurrentContext: (next: ExtensionContext | undefined) => void,
): void {
  pi.registerCommand?.("lore-settings", {
    description: "Configure Lore tools and recovery",
    handler: async (args: unknown, ctx: unknown) => {
      setCurrentContext(toExtensionContext(ctx));
      const input = typeof args === "string" ? args.trim() : "";
      if (input === "show") {
        commandUi(ctx)?.notify?.(formatLoreConfigStatus(host), "info");
        return;
      }
      if (input.startsWith("set ")) {
        const patch = parseLoreSettingsPatch(input.slice(4));
        const path = writeProjectLoreConfig(host, patch);
        commandUi(ctx)?.notify?.(`Updated ${path}. Run /reload to apply tool registration changes.`, "info");
        return;
      }
      await configureLoreSettingsMenu(pi, host, runtime, ctx);
    },
  });
}

async function configureLoreSettingsMenu(
  pi: PiExtensionApi,
  host: PiHost,
  runtime: { listAvailableToolNames: () => Promise<string[]> },
  ctx: unknown,
): Promise<void> {
  const ui = requireInteractiveUi(ctx);
  while (true) {
    const choice = await ui.select("Lore settings", ["Tools", "Recovery"]);
    if (choice === "Tools") {
      await configureLoreToolsCheckboxes(pi, host, runtime, ctx);
      continue;
    }
    if (choice === "Recovery") {
      await configureLoreRecoveryCheckboxes(host, ctx);
      continue;
    }
    return;
  }
}

async function configureLoreToolsCheckboxes(
  pi: PiExtensionApi,
  host: PiHost,
  runtime: { listAvailableToolNames: () => Promise<string[]> },
  ctx: unknown,
): Promise<void> {
  const ui = requireCustomUi(ctx);
  const config = loadLoreConfig(host);
  const toolNames = await runtime.listAvailableToolNames();
  if (toolNames.length === 0) {
    ui.notify?.("Lore MCP did not report any public tools.", "warning");
    return;
  }
  const checked = await ui.custom((_tui, _theme, _keybindings, done) =>
    new CheckboxListComponent(
      "Lore tools",
      toolNames.map((name) => ({ id: name, label: name, checked: toolIsEnabled(config.tools, name) })),
      done,
    ),
  );
  if (!checked) {
    return;
  }
  const enabled = new Set(checked);
  const disabled = toolNames.filter((name) => !enabled.has(name));
  persistInteractiveLoreConfig(
    host,
    {
      tools: {
        disabled,
      },
    },
    ui,
  );
  applyActiveLoreToolSelection(pi, toolNames, checked);
}

function applyActiveLoreToolSelection(pi: PiExtensionApi, loreToolNames: string[], enabledLoreToolNames: string[]): void {
  const activeTools = pi.getActiveTools?.();
  if (!activeTools || !pi.setActiveTools) {
    return;
  }
  const loreTools = new Set(loreToolNames);
  const nonLoreActiveTools = activeTools.filter((name) => !loreTools.has(name));
  pi.setActiveTools([...new Set([...nonLoreActiveTools, ...enabledLoreToolNames])]);
}

async function configureLoreRecoveryCheckboxes(host: PiHost, ctx: unknown): Promise<void> {
  const ui = requireCustomUi(ctx);
  const config = loadLoreConfig(host);
  const checked = await ui.custom((_tui, _theme, _keybindings, done) =>
    new CheckboxListComponent(
      "Lore recovery",
      [
        { id: "compilation", label: "Compilation recovery", checked: config.recovery.compilation },
        { id: "tests", label: "Test recovery", checked: config.recovery.tests },
      ],
      done,
    ),
  );
  if (!checked) {
    return;
  }
  persistInteractiveLoreConfig(
    host,
    { recovery: { compilation: checked.includes("compilation"), tests: checked.includes("tests") } },
    ui,
  );
}

function persistInteractiveLoreConfig(
  host: PiHost,
  patch: Record<string, unknown>,
  ui: { notify?: (message: string, type?: string) => void },
): void {
  const path = writeProjectLoreConfig(host, patch);
  ui.notify?.(`Updated ${path}. Run /reload to apply tool registration changes.`, "info");
}

function requireInteractiveUi(ctx: unknown): {
  notify?: (message: string, type?: string) => void;
  select: (title: string, options: string[]) => Promise<string | undefined>;
} {
  const ui = commandUi(ctx);
  if (!ui?.select) {
    throw new Error("/lore-settings requires Pi UI select support");
  }
  return ui as {
    notify?: (message: string, type?: string) => void;
    select: (title: string, options: string[]) => Promise<string | undefined>;
  };
}

function requireCustomUi(ctx: unknown): {
  notify?: (message: string, type?: string) => void;
  custom: <T>(factory: (...args: unknown[]) => unknown) => Promise<T | undefined>;
} {
  const ui = commandUi(ctx);
  if (!ui?.custom) {
    throw new Error("/lore-settings requires Pi custom UI support");
  }
  return ui as {
    notify?: (message: string, type?: string) => void;
    custom: <T>(factory: (...args: unknown[]) => unknown) => Promise<T | undefined>;
  };
}

function toolIsEnabled(config: { disabled: string[] }, name: string): boolean {
  return !config.disabled.includes(name);
}

function formatLoreConfigStatus(host: PiHost): string {
  const config = loadLoreConfig(host);
  return [
    "Lore settings:",
    `Project config: ${projectLoreConfigPath(host)}`,
    "Effective tool/recovery settings:",
    JSON.stringify({ tools: config.tools, recovery: config.recovery }, null, 2),
    "",
    'Update project config with: /lore-settings set {"recovery":{"tests":false}}',
    "Changes apply after /reload or Pi restart.",
  ].join("\n");
}

function parseLoreSettingsPatch(text: string): Record<string, unknown> {
  let parsed: unknown;
  try {
    parsed = JSON.parse(text);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`Invalid /lore-settings JSON: ${message}`);
  }
  if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
    throw new Error("Invalid /lore-settings JSON: expected an object");
  }
  const patch = parsed as Record<string, unknown>;
  const allowed = new Set(["tools", "recovery"]);
  const unsupported = Object.keys(patch).filter((key) => !allowed.has(key));
  if (unsupported.length > 0) {
    throw new Error(`Unsupported /lore-settings keys: ${unsupported.join(", ")}`);
  }
  validateLoreSettingsPatchShape(patch);
  return patch;
}

function validateLoreSettingsPatchShape(patch: Record<string, unknown>): void {
  const tools = patch.tools;
  if (tools !== undefined) {
    if (!isPlainObject(tools)) {
      throw new Error("Invalid /lore-settings JSON: tools must be an object");
    }
    const unsupportedTools = Object.keys(tools).filter((key) => key !== "disabled");
    if (unsupportedTools.length > 0) {
      throw new Error(`Unsupported /lore-settings tools keys: ${unsupportedTools.join(", ")}`);
    }
  }
  const recovery = patch.recovery;
  if (recovery !== undefined) {
    if (!isPlainObject(recovery)) {
      throw new Error("Invalid /lore-settings JSON: recovery must be an object");
    }
    const unsupportedRecovery = Object.keys(recovery).filter((key) => key !== "compilation" && key !== "tests");
    if (unsupportedRecovery.length > 0) {
      throw new Error(`Unsupported /lore-settings recovery keys: ${unsupportedRecovery.join(", ")}`);
    }
  }
}

function writeProjectLoreConfig(host: PiHost, patch: Record<string, unknown>): string {
  const path = projectLoreConfigPath(host);
  const existing = readJsonFileIfPresent(path);
  const next = deepMergeObject(existing, patch);
  removeUnsupportedLoreSettings(next);
  loadLoreConfig({ ...host, getConfig: () => next });
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, `${JSON.stringify(next, null, 2)}\n`, "utf8");
  return path;
}

function readJsonFileIfPresent(path: string): Record<string, unknown> {
  if (!existsSync(path)) {
    return {};
  }
  try {
    const parsed = JSON.parse(readFileSync(path, "utf8"));
    if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
      throw new Error("expected an object");
    }
    return parsed as Record<string, unknown>;
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`Invalid Lore project config at ${path}: ${message}`);
  }
}

function projectLoreConfigPath(host: PiHost): string {
  return join(resolve(String(host.projectDir ?? host.cwd ?? process.cwd())), ".pi", "lore.config.json");
}

function deepMergeObject(left: Record<string, unknown>, right: Record<string, unknown>): Record<string, unknown> {
  const result: Record<string, unknown> = { ...left };
  for (const [key, value] of Object.entries(right)) {
    if (isPlainObject(result[key]) && isPlainObject(value)) {
      result[key] = deepMergeObject(result[key], value);
    } else {
      result[key] = value;
    }
  }
  return result;
}

function removeUnsupportedLoreSettings(config: Record<string, unknown>): void {
  const tools = config.tools;
  if (tools && typeof tools === "object" && !Array.isArray(tools)) {
    delete (tools as Record<string, unknown>).enabled;
  }
}

function uniqueSorted(values: string[]): string[] {
  return [...new Set(values)].sort();
}

class CheckboxListComponent {
  private readonly title: string;
  private readonly items: Array<{ id: string; label: string; checked: boolean }>;
  private readonly done: (value: string[] | undefined) => void;
  private cursor = 0;

  constructor(
    title: string,
    items: Array<{ id: string; label: string; checked: boolean }>,
    done: (value: string[] | undefined) => void,
  ) {
    this.title = title;
    this.items = items;
    this.done = done;
  }

  render(width: number): string[] {
    const safeWidth = Number.isFinite(width) && width > 0 ? Math.floor(width) : 80;
    return [
      this.title,
      "Space/Enter: toggle  Esc: back/save",
      ...this.items.map((item, index) => {
        const cursor = index === this.cursor ? "›" : " ";
        const checkbox = item.checked ? "[X]" : "[ ]";
        return `${cursor} ${checkbox} ${item.label}`.slice(0, safeWidth);
      }),
    ];
  }

  handleInput(data: string): void {
    if (data === "\u001b[B" || data === "j") {
      this.cursor = Math.min(this.cursor + 1, this.items.length - 1);
      return;
    }
    if (data === "\u001b[A" || data === "k") {
      this.cursor = Math.max(this.cursor - 1, 0);
      return;
    }
    if ((data === " " || data === "\r" || data === "\n") && this.items[this.cursor]) {
      this.items[this.cursor].checked = !this.items[this.cursor].checked;
      return;
    }
    if (data.startsWith("\u001b")) {
      this.done(this.items.filter((item) => item.checked).map((item) => item.id));
    }
  }

  invalidate(): void {
    // stateless render cache
  }
}

function isPlainObject(value: unknown): value is Record<string, unknown> {
  return Boolean(value) && typeof value === "object" && !Array.isArray(value);
}

function registerLoreRestartCommand(
  pi: PiExtensionApi,
  runtime: { restartLore: () => Promise<void> },
  setCurrentContext: (next: ExtensionContext | undefined) => void,
): void {
  pi.registerCommand?.("lore-restart", {
    description: "Restart the Lore MCP binary",
    handler: async (_args: unknown, ctx: unknown) => {
      setCurrentContext(toExtensionContext(ctx));
      const ui = commandUi(ctx);
      ui?.notify?.("Restarting Lore MCP binary...", "info");
      try {
        await runtime.restartLore();
        ui?.notify?.("Lore MCP binary restarted.", "info");
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        ui?.notify?.(`Lore MCP binary restart failed: ${message}`, "error");
        throw error;
      }
    },
  });
}

function commandUi(ctx: unknown): {
  notify?: (message: string, type?: string) => void;
  select?: (title: string, options: string[]) => Promise<string | undefined>;
  custom?: <T>(factory: (done: (value: T | undefined) => void) => unknown) => Promise<T | undefined>;
} | undefined {
  if (!ctx || typeof ctx !== "object") {
    return undefined;
  }
  const ui = (ctx as { ui?: unknown }).ui;
  if (!ui || typeof ui !== "object") {
    return undefined;
  }
  const notify = (ui as { notify?: unknown }).notify;
  const select = (ui as { select?: unknown }).select;
  const custom = (ui as { custom?: unknown }).custom;
  if (typeof notify !== "function" && typeof select !== "function" && typeof custom !== "function") {
    return undefined;
  }
  return {
    notify: typeof notify === "function" ? (notify as (message: string, type?: string) => void) : undefined,
    select: typeof select === "function" ? (select as (title: string, options: string[]) => Promise<string | undefined>) : undefined,
    custom: typeof custom === "function" ? (custom as <T>(factory: (...args: unknown[]) => unknown) => Promise<T | undefined>) : undefined,
  };
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

function adaptPiHost(
  pi: PiExtensionApi,
  context: {
    getCurrentContext: () => ExtensionContext | undefined;
    setCurrentContext: (next: ExtensionContext | undefined) => void;
    appendDisplayMessageImmediately: (message: PiCustomMessage) => Promise<void>;
  },
): PiHost {
  return {
    cwd: process.cwd(),
    getConfig(name) {
      return pi.getConfig?.(name);
    },
    registerTool(tool) {
      if (!pi.registerTool) {
        throw new Error("Lore extension requires pi.registerTool");
      }
      pi.registerTool(wrapPiToolRegistration(tool, context.setCurrentContext));
    },
    hasTool(name) {
      try {
        const allTools = pi.getAllTools?.() ?? [];
        return allTools.some((tool) => tool && typeof tool === "object" && (tool as { name?: unknown }).name === name);
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        if (message.includes("Extension runtime not initialized")) {
          return false;
        }
        throw error;
      }
    },
    on(event, handler) {
      pi.on?.(event, (...args: unknown[]) => {
        const next = toExtensionContext(args[1]);
        if (next) {
          context.setCurrentContext(next);
        }
        return handler(...args);
      });
    },
    appendEntry(entry) {
      if (!pi.appendEntry) {
        throw new Error("Lore extension requires pi.appendEntry");
      }
      pi.appendEntry("lore", entry);
    },
    sendMessage(message, options) {
      if (!pi.sendMessage) {
        throw new Error("Lore extension requires pi.sendMessage");
      }
      pi.sendMessage(message, options);
    },
    async appendDisplayMessageImmediately(message) {
      const existing = getExistingCustomMessage(context.getCurrentContext(), message);
      if (existing) {
        return "already-present";
      }
      await context.appendDisplayMessageImmediately(message);
      return "appended";
    },
    getActiveBranchEntries() {
      const ctx = context.getCurrentContext();
      if (!ctx) {
        throw new Error("Lore extension requires an active Pi session context");
      }
      return normalizePiEntries(ctx.sessionManager.getBranch());
    },
    getCurrentEntryId() {
      const leafId = context.getCurrentContext()?.sessionManager.getLeafId();
      return leafId ?? undefined;
    },
    getCurrentAssistantSequenceStartEntryId() {
      const ctx = context.getCurrentContext();
      if (!ctx) {
        return undefined;
      }
      return findAssistantSequenceStartEntryId(normalizePiEntries(ctx.sessionManager.getBranch()));
    },
    setStatus(key, text) {
      const ctx = context.getCurrentContext();
      if (ctx) {
        ctx.ui.setStatus(key, text);
        return;
      }
      if (text !== undefined) {
        console.warn(`[lore] status ${key}: ${text}`);
      }
    },
    clearStatus(key) {
      const ctx = context.getCurrentContext();
      if (ctx) {
        ctx.ui.setStatus(key, undefined);
        return;
      }
      console.warn(`[lore] status cleared: ${key}`);
    },
    notify(message, options) {
      const ctx = context.getCurrentContext();
      if (ctx) {
        ctx.ui.notify(message, notificationTone(options));
        return;
      }
      const tone = notificationTone(options);
      if (tone === "error") {
        console.error(`[lore] ${message}`);
      } else {
        console.warn(`[lore] ${message}`);
      }
    },
    async generateText(request) {
      const { model, auth } = await resolvePiModelAuth(context.getCurrentContext());

      const piAi = await loadPiAiModule();
      return completeSimpleText(
        piAi,
        model,
        [
          {
            role: "user",
            content: [{ type: "text", text: request.prompt }],
            timestamp: Date.now(),
          },
        ],
        {
          timeoutMs: request.timeoutMs,
          signal: request.signal,
          apiKey: auth.apiKey,
          headers: auth.headers,
        },
        "Lore generateText failed: provider returned empty summary text",
      );
    },
    async generateTextFromMessages(request) {
      const { model, auth } = await resolvePiModelAuth(context.getCurrentContext());

      const piAi = await loadPiAiModule();
      const llmMessages = toLlmMessages(request.messages);
      return completeSimpleText(
        piAi,
        model,
        [
          ...llmMessages,
          {
            role: "user",
            content: [{ type: "text", text: request.prompt }],
            timestamp: Date.now(),
          },
        ],
        {
          timeoutMs: request.timeoutMs,
          signal: request.signal,
          apiKey: auth.apiKey,
          headers: auth.headers,
        },
        "Lore generateTextFromMessages failed: provider returned empty summary text",
      );
    },
  };
}

function getExistingCustomMessage(ctx: ExtensionContext | undefined, message: PiCustomMessage): PiEntry | undefined {
  if (!ctx) {
    throw new Error("Lore extension requires an active Pi session context");
  }
  const recoveryId = toRecord(message.details)?.recoveryId;
  if (typeof recoveryId !== "string") {
    return undefined;
  }
  return normalizePiEntries(ctx.sessionManager.getBranch()).find((entry) => {
    const details = toRecord(entry.details);
    return entry.customType === message.customType && details?.recoveryId === recoveryId;
  });
}

function validateRequiredPiCapabilities(pi: PiExtensionApi): void {
  const missing = [
    typeof pi.registerTool === "function" ? undefined : "registerTool",
    typeof pi.appendEntry === "function" ? undefined : "appendEntry",
    typeof pi.sendMessage === "function" ? undefined : "sendMessage",
    typeof pi.on === "function" ? undefined : "on",
  ].filter((name): name is string => typeof name === "string");
  if (missing.length > 0) {
    throw new Error(`Lore extension requires Pi capabilities: ${missing.join(", ")}`);
  }
}

function wrapPiToolRegistration(
  tool: PiToolRegistration,
  setCurrentContext: (next: ExtensionContext | undefined) => void,
): PiToolRegistration {
  return {
    name: tool.name,
    label: tool.label ?? tool.name,
    description: tool.description ?? tool.name,
    promptSnippet: tool.promptSnippet,
    promptGuidelines: tool.promptGuidelines,
    executionMode: tool.executionMode,
    renderShell: tool.renderShell,
    renderCall: tool.renderCall,
    renderResult: tool.renderResult,
    parameters: tool.parameters,
    execute: async (toolCallId, params, signal, onUpdate, ctx) => {
      const next = toExtensionContext(ctx);
      if (next) {
        setCurrentContext(next);
      }
      return tool.execute(toolCallId, params, signal, onUpdate, ctx);
    },
  };
}

function notificationTone(options: unknown): "info" | "warning" | "error" {
  if (!options || typeof options !== "object") {
    return "info";
  }
  const tone = (options as { tone?: unknown }).tone;
  if (tone === "error") {
    return "error";
  }
  if (tone === "warning") {
    return "warning";
  }
  return "info";
}

function activateLoreTools(pi: PiExtensionApi, toolNames: string[]): void {
  if (toolNames.length === 0) {
    return;
  }
  try {
    const activeTools = pi.getActiveTools?.() ?? [];
    pi.setActiveTools?.([...new Set([...activeTools, ...toolNames])]);
  } catch {
    // Active-tool actions are unavailable during extension loading. The same
    // activation runs again on session_start/model_select after Pi binds core.
  }
}

function currentActiveLoreToolNames(pi: PiExtensionApi, registeredLoreToolNames: string[]): string[] {
  try {
    const activeTools = pi.getActiveTools?.();
    if (!activeTools) {
      return registeredLoreToolNames;
    }
    const active = new Set(activeTools);
    return registeredLoreToolNames.filter((name) => active.has(name));
  } catch {
    return registeredLoreToolNames;
  }
}

function toExtensionContext(value: unknown): ExtensionContext | undefined {
  if (!value || typeof value !== "object") {
    return undefined;
  }
  const sessionManager = (value as { sessionManager?: unknown }).sessionManager;
  const ui = (value as { ui?: unknown }).ui;
  if (!sessionManager || typeof sessionManager !== "object" || !ui || typeof ui !== "object") {
    return undefined;
  }
  const getBranch = (sessionManager as { getBranch?: unknown }).getBranch;
  const getLeafId = (sessionManager as { getLeafId?: unknown }).getLeafId;
  const notify = (ui as { notify?: unknown }).notify;
  const setStatus = (ui as { setStatus?: unknown }).setStatus;
  if (
    typeof getBranch !== "function" ||
    typeof getLeafId !== "function" ||
    typeof notify !== "function" ||
    typeof setStatus !== "function"
  ) {
    return undefined;
  }
  return value as ExtensionContext;
}

function normalizePiEntries(entries: unknown[]): PiEntry[] {
  return entries.map(normalizePiEntry).filter((entry): entry is PiEntry => entry !== undefined);
}

function normalizePiEntry(entry: unknown): PiEntry | undefined {
  if (!entry || typeof entry !== "object") {
    return undefined;
  }
  const raw = entry as Record<string, unknown>;
  const id = typeof raw.id === "string" ? raw.id : undefined;
  const parentId = typeof raw.parentId === "string" ? raw.parentId : undefined;
  const createdAt = timestampToMs(raw.timestamp);

  if (raw.type === "custom" && raw.customType === "lore" && raw.data && typeof raw.data === "object") {
    return {
      ...(raw.data as Record<string, unknown>),
      id,
      parentId,
      createdAt,
    };
  }

  if (raw.type === "message" && raw.message && typeof raw.message === "object") {
    const message = raw.message as Record<string, unknown>;
    return {
      id,
      parentId,
      createdAt,
      type: "message",
      role: asString(message.role),
      content: message.content,
      details: message.details,
      toolCallId: message.toolCallId,
      toolName: message.toolName,
      isError: message.isError,
      timestamp: message.timestamp,
    };
  }

  if (raw.type === "custom_message") {
    return {
      id,
      parentId,
      createdAt,
      type: "custom_message",
      role: "custom",
      customType: raw.customType,
      content: raw.content,
      details: raw.details,
      display: raw.display,
      timestamp: raw.timestamp,
    };
  }

  return {
    ...raw,
    id,
    parentId,
    createdAt,
  };
}

function normalizePiMessages(messages: unknown[]): PiEntry[] {
  return messages.filter((message) => !isHiddenFromModelMessage(message)).map((message) => {
    if (!message || typeof message !== "object") {
      return { role: "unknown", content: "", __raw: message };
    }
    const obj = message as Record<string, unknown>;
    const role = asString(obj.role) ?? "unknown";
    return {
      id: asString(obj.id),
      role,
      type: role,
      content: obj.content,
      details: obj.details,
      customType: obj.customType,
      display: obj.display,
      toolCallId: obj.toolCallId,
      toolName: obj.toolName,
      isError: obj.isError,
      timestamp: obj.timestamp,
      __raw: message,
    };
  });
}

function denormalizePiMessages(entries: PiEntry[]): unknown[] {
  return entries.map((entry) => {
    if (entry.__raw !== undefined) {
      return entry.__raw;
    }
    const details = toRecord(entry.details);
    if (details?.loreProjection && typeof entry.content === "string") {
      return {
        role: "custom",
        customType: "lore-recovery-projection",
        content: entry.content,
        display: false,
        details: entry.details,
        timestamp: Date.now(),
      };
    }
    return {
      role: "custom",
      customType: "lore-recovery-projection",
      content: typeof entry.content === "string" ? entry.content : "",
      display: false,
      details: entry.details,
      timestamp: Date.now(),
    };
  });
}

function findAssistantSequenceStartEntryId(entries: PiEntry[]): string | undefined {
  let foundAssistantId: string | undefined;
  for (let index = entries.length - 1; index >= 0; index -= 1) {
    const role = entries[index].role;
    if (role === "assistant") {
      foundAssistantId = entries[index].id ?? foundAssistantId;
      continue;
    }
    if (role === "toolResult") {
      continue;
    }
    if (foundAssistantId) {
      break;
    }
  }
  return foundAssistantId;
}

function timestampToMs(value: unknown): number | undefined {
  if (typeof value === "string") {
    const parsed = Date.parse(value);
    return Number.isFinite(parsed) ? parsed : undefined;
  }
  if (typeof value === "number" && Number.isFinite(value)) {
    return value;
  }
  return undefined;
}

function asString(value: unknown): string | undefined {
  return typeof value === "string" ? value : undefined;
}

function toRecord(value: unknown): Record<string, unknown> | undefined {
  return value && typeof value === "object" ? (value as Record<string, unknown>) : undefined;
}

const COMPACTION_SUMMARY_PREFIX = `The conversation history before this point was compacted into the following summary:\n\n<summary>\n`;
const COMPACTION_SUMMARY_SUFFIX = `\n</summary>`;
const BRANCH_SUMMARY_PREFIX = `The following is a summary of a branch that this conversation came back from:\n\n<summary>\n`;
const BRANCH_SUMMARY_SUFFIX = `</summary>`;

async function resolvePiModelAuth(
  ctx: ExtensionContext | undefined,
): Promise<{ model: unknown; auth: { apiKey?: string; headers?: Record<string, string> } }> {
  if (!ctx) {
    throw new Error("Lore generateText failed: no active Pi context");
  }
  if (!ctx.model) {
    throw new Error("Lore generateText failed: no active Pi model selected");
  }
  const modelRegistry = ctx.modelRegistry;
  if (!modelRegistry || typeof modelRegistry.getApiKeyAndHeaders !== "function") {
    throw new Error("Lore generateText failed: Pi model registry is unavailable in this context");
  }
  const auth = await modelRegistry.getApiKeyAndHeaders(ctx.model);
  if (!auth.ok) {
    throw new Error(`Lore generateText failed: ${auth.error}`);
  }
  return {
    model: ctx.model,
    auth: {
      apiKey: auth.apiKey,
      headers: auth.headers,
    },
  };
}

async function completeSimpleText(
  piAi: PiAiModule,
  model: unknown,
  messages: Array<Record<string, unknown>>,
  options: {
    timeoutMs: number;
    signal?: AbortSignal;
    apiKey?: string;
    headers?: Record<string, string>;
  },
  emptySummaryError: string,
): Promise<string> {
  const result = await piAi.completeSimple(model, { messages }, options);
  if (result.stopReason === "error" || result.stopReason === "aborted") {
    throw new Error(result.errorMessage ?? `summary generation failed (${result.stopReason})`);
  }
  const summary = result.content
    .filter((part) => part.type === "text")
    .map((part) => (typeof part.text === "string" ? part.text : ""))
    .join("\n")
    .trim();
  if (!summary) {
    throw new Error(emptySummaryError);
  }
  return summary;
}

function toLlmMessages(messages: unknown[]): Array<Record<string, unknown>> {
  const result: Array<Record<string, unknown>> = [];
  for (const message of messages) {
    const normalized = normalizeContextMessageForLlm(message);
    if (normalized) {
      result.push(normalized);
    }
  }
  return result;
}

function normalizeContextMessageForLlm(message: unknown): Record<string, unknown> | undefined {
  const obj = toRecord(message);
  if (!obj) {
    return undefined;
  }
  if (isHiddenFromModelMessage(obj)) {
    return undefined;
  }
  const role = asString(obj.role);
  const timestamp = typeof obj.timestamp === "number" ? obj.timestamp : Date.now();

  if (role === "user" || role === "assistant" || role === "toolResult") {
    return obj;
  }

  if (role === "custom") {
    const content =
      typeof obj.content === "string" ? [{ type: "text", text: obj.content }] : (obj.content as unknown[] | undefined);
    return {
      role: "user",
      content,
      timestamp,
    };
  }

  if (role === "branchSummary" && typeof obj.summary === "string") {
    return {
      role: "user",
      content: [{ type: "text", text: BRANCH_SUMMARY_PREFIX + obj.summary + BRANCH_SUMMARY_SUFFIX }],
      timestamp,
    };
  }

  if (role === "compactionSummary" && typeof obj.summary === "string") {
    return {
      role: "user",
      content: [{ type: "text", text: COMPACTION_SUMMARY_PREFIX + obj.summary + COMPACTION_SUMMARY_SUFFIX }],
      timestamp,
    };
  }

  if (role === "bashExecution") {
    if (obj.excludeFromContext === true) {
      return undefined;
    }
    return {
      role: "user",
      content: [{ type: "text", text: bashExecutionToText(obj) }],
      timestamp,
    };
  }

  return undefined;
}

function isHiddenFromModelMessage(value: unknown): boolean {
  const message = toRecord(value);
  const details = toRecord(message?.details);
  // Immediate delivery keeps display-only recovery summaries out of Pi's
  // active queue; this predicate handles persisted/reloaded copies.
  return message?.hiddenFromModel === true || details?.hiddenFromModel === true;
}

function bashExecutionToText(message: Record<string, unknown>): string {
  const command = asString(message.command) ?? "";
  let text = `Ran \`${command}\`\n`;
  const output = asString(message.output);
  if (output) {
    text += `\`\`\`\n${output}\n\`\`\``;
  } else {
    text += "(no output)";
  }
  if (message.cancelled === true) {
    text += "\n\n(command cancelled)";
  } else if (typeof message.exitCode === "number" && message.exitCode !== 0) {
    text += `\n\nCommand exited with code ${message.exitCode}`;
  }
  if (message.truncated === true && typeof message.fullOutputPath === "string") {
    text += `\n\n[Output truncated. Full output: ${message.fullOutputPath}]`;
  }
  return text;
}

async function loadPiAiModule(): Promise<PiAiModule> {
  if (cachedPiAiModule) {
    return cachedPiAiModule;
  }

  const errors: string[] = [];

  try {
    const direct = (await import("@earendil-works/pi-ai")) as unknown as PiAiModule;
    if (typeof direct.completeSimple === "function") {
      cachedPiAiModule = direct;
      return direct;
    }
    errors.push("@earendil-works/pi-ai loaded but did not export completeSimple");
  } catch (error) {
    errors.push(error instanceof Error ? error.message : String(error));
  }

  for (const path of candidatePiAiPaths()) {
    if (!existsSync(path)) {
      continue;
    }
    try {
      const module = (await import(pathToFileURL(path).href)) as unknown as PiAiModule;
      if (typeof module.completeSimple === "function") {
        cachedPiAiModule = module;
        return module;
      }
      errors.push(`${path} loaded but did not export completeSimple`);
    } catch (error) {
      errors.push(`${path}: ${error instanceof Error ? error.message : String(error)}`);
    }
  }

  throw new Error(`Lore generateText failed: unable to load pi-ai completeSimple (${errors.join(" | ")})`);
}

function candidatePiAiPaths(): string[] {
  const paths = new Set<string>();

  const seeds = [process.argv[1], filePathFromImportMeta()]
    .filter((value): value is string => typeof value === "string" && value.length > 0)
    .map((value) => dirname(value));

  for (const seed of seeds) {
    let cursor = seed;
    for (let level = 0; level < 7; level += 1) {
      paths.add(join(cursor, "node_modules", "@earendil-works", "pi-ai", "dist", "index.js"));
      const parent = dirname(cursor);
      if (parent === cursor) {
        break;
      }
      cursor = parent;
    }
  }

  return [...paths];
}

function filePathFromImportMeta(): string {
  return fileURLToPath(import.meta.url);
}

export const __test = {
  appendLoreContextMarkerGuidance,
  currentActiveLoreToolNames,
  parseLoreSettingsPatch,
  projectLoreConfigPath,
  registerLoreSettingsCommand,
  registerLoreSystemPromptGuidance,
  registerLoreRestartCommand,
  registerUsageStatsCommand,
  normalizePiMessages,
  toLlmMessages,
};
