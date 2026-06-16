import { loadLoreConfig } from "./config.ts";
import { framedCustomUi, selectLoreMenu, styleUiSelection, styleUiTitle, type CustomUiKeybindings, type CustomUiTheme, type LoreMenuUi } from "./custom-ui.ts";
import { projectLoreConfigPath as canonicalProjectLoreConfigPath, updateProjectLoreConfig } from "./project-config.ts";
import { commandUi, type ManualCommandResult } from "./setup.ts";
import type { PiHost } from "./types.ts";


type LoreSettingsController = {
  setProjectEnabled: (enabled: boolean, ctx?: unknown) => Promise<void>;
  configureCommand: (ctx?: unknown) => Promise<ManualCommandResult>;
};

type PiExtensionApi = {
  registerCommand?: (name: string, options: unknown) => void;
  getActiveTools?: () => string[];
  setActiveTools?: (toolNames: string[]) => void;
};

export function registerLoreSettingsCommand(
  pi: PiExtensionApi,
  host: PiHost,
  runtime: { listAvailableToolNames: () => Promise<string[]> },
  controller: LoreSettingsController,
  setCurrentContextFromRaw: (ctx: unknown) => void,
): void {
  pi.registerCommand?.("lore-settings", {
    description: "Configure Lore for this project",
    handler: async (args: unknown, ctx: unknown) => {
      setCurrentContextFromRaw(ctx);
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
      await configureLoreSettingsMenu(pi, host, runtime, controller, ctx);
    },
  });
}

async function configureLoreSettingsMenu(
  pi: PiExtensionApi,
  host: PiHost,
  runtime: { listAvailableToolNames: () => Promise<string[]> },
  controller: LoreSettingsController,
  ctx: unknown,
): Promise<void> {
  const ui = requireInteractiveUi(ctx);
  while (true) {
    const enabled = loadLoreConfig(host).enabled;
    const projectAction = enabled ? "Disable Lore for this project" : "Enable Lore for this project";
    const choice = await selectLoreMenu(ui, "Lore settings", [projectAction, "Set command to run Lore", "Tools", "Recovery"]);
    if (choice === projectAction) {
      await controller.setProjectEnabled(!enabled, ctx);
      continue;
    }
    if (choice === "Set command to run Lore") {
      await controller.configureCommand(ctx);
      continue;
    }
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
  const checked = await ui.custom((_tui, theme, keybindings, done) =>
    framedCustomUi(
      new CheckboxListComponent(
        "Lore tools",
        toolNames.map((name) => ({ id: name, label: name, checked: toolIsEnabled(config.tools, name) })),
        theme as CustomUiTheme,
        keybindings as CustomUiKeybindings,
        done,
      ),
      theme,
      "↑↓ navigate  space/enter toggle  escape/ctrl+c save and back",
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
  const checked = await ui.custom((_tui, theme, keybindings, done) =>
    framedCustomUi(
      new CheckboxListComponent(
        "Lore recovery",
        [
          { id: "compilation", label: "Compilation recovery", checked: config.recovery.compilation },
          { id: "tests", label: "Test recovery", checked: config.recovery.tests },
        ],
        theme as CustomUiTheme,
        keybindings as CustomUiKeybindings,
        done,
      ),
      theme,
      "↑↓ navigate  space/enter toggle  escape/ctrl+c save and back",
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

function requireInteractiveUi(ctx: unknown): LoreMenuUi & {
  notify?: (message: string, type?: string) => void;
} {
  const ui = commandUi(ctx);
  if (!ui?.select && !ui?.custom) {
    throw new Error("/lore-settings requires Pi custom or select UI support");
  }
  return ui;
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
    "Effective settings:",
    JSON.stringify({ enabled: config.enabled, command: config.command ?? null, tools: config.tools, recovery: config.recovery }, null, 2),
    "",
    'Update project config with: /lore-settings set {"recovery":{"tests":false}}',
    "Changes apply after /reload or Pi restart.",
  ].join("\n");
}

export function parseLoreSettingsPatch(text: string): Record<string, unknown> {
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
  return updateProjectLoreConfig(host, patch);
}

export function projectLoreConfigPath(host: PiHost): string {
  return canonicalProjectLoreConfigPath(host);
}

function uniqueSorted(values: string[]): string[] {
  return [...new Set(values)].sort();
}

class CheckboxListComponent {
  private readonly title: string;
  private readonly items: Array<{ id: string; label: string; checked: boolean }>;
  private readonly theme: CustomUiTheme;
  private readonly keybindings: CustomUiKeybindings;
  private readonly done: (value: string[] | undefined) => void;
  private cursor = 0;

  constructor(
    title: string,
    items: Array<{ id: string; label: string; checked: boolean }>,
    theme: CustomUiTheme,
    keybindings: CustomUiKeybindings,
    done: (value: string[] | undefined) => void,
  ) {
    this.title = title;
    this.items = items;
    this.theme = theme;
    this.keybindings = keybindings;
    this.done = done;
  }

  render(width: number): string[] {
    const safeWidth = Number.isFinite(width) && width > 0 ? Math.floor(width) : 80;
    return [
      styleUiTitle(this.theme, this.title),
      "",
      ...this.items.map((item, index) => {
        const selected = index === this.cursor;
        const cursor = selected ? "→" : " ";
        const checkbox = item.checked ? "[X]" : "[ ]";
        return styleUiSelection(this.theme, `${cursor} ${checkbox} ${item.label}`.slice(0, safeWidth), selected);
      }),
    ];
  }

  handleInput(data: string): void {
    if (this.keybindings.matches(data, "tui.select.down") || data === "j") {
      this.cursor = Math.min(this.cursor + 1, this.items.length - 1);
      return;
    }
    if (this.keybindings.matches(data, "tui.select.up") || data === "k") {
      this.cursor = Math.max(this.cursor - 1, 0);
      return;
    }
    if ((data === " " || this.keybindings.matches(data, "tui.select.confirm")) && this.items[this.cursor]) {
      this.items[this.cursor].checked = !this.items[this.cursor].checked;
      return;
    }
    if (this.keybindings.matches(data, "tui.select.cancel")) {
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

