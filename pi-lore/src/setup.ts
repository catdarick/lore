import { resolve } from "node:path";
import { currentBinaryTarget, loadBundledBinaryManifest, type BinaryTarget } from "./binary-manifest.ts";
import { installManagedLoreBinary, planManagedLoreBinary, validateLoreCommandForProject, type BinaryManagerOptions, type ManagedBinaryPlan } from "./binary-manager.ts";
import { effectiveLoreProjectDir, loadLoreConfig } from "./config.ts";
import { framedCustomUi, selectLoreMenu, styleUiTitle, type CustomUiKeybindings, type CustomUiTheme } from "./custom-ui.ts";
import { showLoreExtensionStatus, type LoreExtensionStatus } from "./extension-status.ts";
import { normalizeLoreCommand, setLoreCommand, setLoreEnabled } from "./project-config.ts";
import { probeProjectGhcVersion, type ProbeResult } from "./toolchain-probe.ts";
import type { ExtensionRuntime, LoreProcessConfig, PiHost } from "./types.ts";

export type SetupReason = "downloadRequired" | "unsupportedGhc";
export type LoreStartupFailure =
  | { kind: "planning"; summary: string; details: string }
  | { kind: "download"; summary: string; details: string }
  | { kind: "process"; summary: string; details: string; processConfig: LoreProcessConfig };
export type LoreStartupState =
  | { kind: "idle" }
  | { kind: "disabled" }
  | { kind: "planning" }
  | { kind: "waitingForSetup"; reason: SetupReason; plan?: ManagedBinaryPlan }
  | { kind: "installing"; ghcVersion: string }
  | { kind: "starting"; command: string; mode: "managed" | "custom"; ghcVersion?: string; provider?: ProbeResult["provider"] }
  | { kind: "ready"; command: string; mode: "managed" | "custom"; ghcVersion?: string; provider?: ProbeResult["provider"] }
  | { kind: "failed"; failure: LoreStartupFailure }
  | { kind: "skippedForSession" };

export type SetupUi = {
  notify?: (message: string, tone?: "info" | "warning" | "error" | string) => void;
  select?: (title: string, options: string[]) => Promise<string | undefined>;
  custom?: <T>(factory: (...args: unknown[]) => unknown) => Promise<T | undefined>;
};

export type StartupCoordinator = {
  startAutomatically: () => Promise<void>;
  attachContext: (ctx: unknown) => void;
  openSetup: (ctx?: unknown) => Promise<void>;
  setProjectEnabled: (enabled: boolean, ctx?: unknown) => Promise<void>;
  configureCommand: (ctx?: unknown) => Promise<ManualCommandResult>;
  statusText: () => string;
  restartOrSetup: (ctx?: unknown) => Promise<RestartOutcome>;
  getState: () => LoreStartupState;
};

export type RestartOutcome =
  | { kind: "restarted" }
  | { kind: "failed"; summary: string }
  | { kind: "openedSetup" }
  | { kind: "disabled" }
  | { kind: "cancelled" };

type MenuAction = "continue" | "done";
export type ManualCommandResult = "applied" | "cancelled";

export function createStartupCoordinator(input: {
  host: PiHost;
  runtime: ExtensionRuntime;
  projectDir: string;
  onReady?: (toolNames: string[]) => void;
  activate?: () => void;
  deactivate?: () => void;
  binaryOptions?: Partial<BinaryManagerOptions>;
}): StartupCoordinator {
  let state: LoreStartupState = { kind: "idle" };
  let currentCtx: unknown;
  let startupPromise: Promise<void> | undefined;
  let setupPromise: Promise<void> | undefined;
  let suppressedAutomaticPrompt = false;
  let lastProcessConfig: LoreProcessConfig | undefined;

  const configProjectDir = resolve(input.projectDir);

  function binaryOptions(): BinaryManagerOptions {
    const config = loadLoreConfig(input.host);
    const projectDir = effectiveLoreProjectDir(config, input.host);
    return { projectDir, env: { ...process.env, ...config.env }, timeoutMs: config.startupTimeoutMs, ...input.binaryOptions };
  }

  async function transition(next: LoreStartupState): Promise<void> {
    state = next;
    await showLoreExtensionStatus(input.host, startupDisplayStatus(next));
  }

  async function refreshStatus(): Promise<void> {
    await showLoreExtensionStatus(input.host, startupDisplayStatus(state));
  }

  async function startAutomatically(): Promise<void> {
    if (startupPromise) return startupPromise;
    startupPromise = doStartAutomatically()
      .catch(async (error) => {
        const details = error instanceof Error ? error.message : String(error);
        await transition({ kind: "failed", failure: { kind: "planning", summary: shortStartupFailure(details), details } });
        input.deactivate?.();
      })
      .finally(() => { startupPromise = undefined; });
    await startupPromise;
    if (!setupPromise) await maybeOpenAutomaticSetup();
  }

  async function doStartAutomatically(): Promise<void> {
    const config = loadLoreConfig(input.host);
    if (!config.enabled) { await transition({ kind: "disabled" }); return; }
    if (config.command) {
      await startProcess({ ...config, command: config.command }, "custom");
      return;
    }
    await transition({ kind: "planning" });
    const plan = await planManagedLoreBinary(binaryOptions());
    if (plan.kind === "ready") {
      await startProcess({ ...config, command: plan.path }, "managed", plan);
      return;
    }
    await transition({ kind: "waitingForSetup", reason: plan.kind, plan });
  }

  async function startProcess(config: LoreProcessConfig, mode: "managed" | "custom", plan?: ManagedBinaryPlan): Promise<void> {
    lastProcessConfig = config;
    await transition({ kind: "starting", command: config.command, mode, ghcVersion: plan?.ghcVersion, provider: plan?.provider });
    const result = await input.runtime.startResolved(config);
    if (result.ok) {
      await transition({ kind: "ready", command: config.command, mode, ghcVersion: plan?.ghcVersion, provider: plan?.provider });
      input.onReady?.(result.registeredToolNames);
      input.activate?.();
      return;
    }
    await input.runtime.stop().catch(() => undefined);
    const details = result.error.message;
    await transition({ kind: "failed", failure: { kind: "process", summary: shortStartupFailure(details), details, processConfig: config } });
    input.deactivate?.();
  }

  async function maybeOpenAutomaticSetup(): Promise<void> {
    if (suppressedAutomaticPrompt) return;
    if (!currentCtx) return;
    if (state.kind !== "waitingForSetup" && state.kind !== "failed") return;
    const ui = commandUi(currentCtx);
    if (!ui?.select && !ui?.custom) return;
    await openSetup(currentCtx, true);
  }

  function attachContext(ctx: unknown): void {
    currentCtx = ctx ?? currentCtx;
    void refreshStatus().then(() => maybeOpenAutomaticSetup()).catch(async (error) => {
      const details = error instanceof Error ? error.message : String(error);
      await transition({ kind: "failed", failure: { kind: "planning", summary: shortStartupFailure(details), details } });
      input.deactivate?.();
    });
  }

  async function openSetup(ctx: unknown = currentCtx, automatic = false): Promise<void> {
    currentCtx = ctx ?? currentCtx;
    if (setupPromise) return setupPromise;
    setupPromise = doOpenSetup(automatic).finally(() => { setupPromise = undefined; });
    return setupPromise;
  }

  async function doOpenSetup(automatic: boolean): Promise<void> {
    const ui = requireSetupUi(currentCtx);
    for (;;) {
      const before = state.kind;
      const config = loadLoreConfig(input.host);
      if (!config.enabled || state.kind === "ready") return;
      if (state.kind === "failed") { const action = await failureMenu(ui, automatic); if (shouldContinueSetup(action, before)) continue; return; }
      if (state.kind === "waitingForSetup") {
        if (state.plan?.kind === "downloadRequired") { const action = await downloadMenu(ui, state.plan, automatic); if (shouldContinueSetup(action, before)) continue; return; }
        if (state.plan?.kind === "unsupportedGhc") { const action = await unsupportedMenu(ui, state.plan, automatic); if (shouldContinueSetup(action, before)) continue; return; }
      }
      await startAutomatically();
      if (state.kind === before || isSetupTerminalState(state.kind)) return;
    }
  }

  function shouldContinueSetup(action: MenuAction, before: LoreStartupState["kind"]): boolean {
    return action === "continue" || (state.kind !== before && !isSetupTerminalState(state.kind));
  }


  async function downloadMenu(ui: SetupUi, plan: Extract<ManagedBinaryPlan, { kind: "downloadRequired" }>, automatic: boolean): Promise<MenuAction> {
    const choice = await selectLoreMenu(ui, `Set up Lore\n\nThis project uses GHC ${plan.ghcVersion}. Lore needs a matching server binary before it can start.`, ["Download", "Set command to run Lore", "Disable Lore for this project", "Not now"]);
    if (choice === "Download") {
      await transition({ kind: "installing", ghcVersion: plan.ghcVersion });
      try {
        const command = await installManagedLoreBinary(plan, binaryOptions());
        await startProcess({ ...loadLoreConfig(input.host), command }, "managed", plan);
      } catch (error) {
        const details = error instanceof Error ? error.message : String(error);
        await transition({ kind: "failed", failure: { kind: "download", summary: shortStartupFailure(details), details } });
        input.deactivate?.();
        return "continue";
      }
    } else if (choice === "Set command to run Lore") return useManualCommand(ui);
    else if (choice === "Disable Lore for this project") await disableLore();
    else if (choice === "Not now" || automatic) await notNow();
    return "done";
  }

  async function unsupportedMenu(ui: SetupUi, plan: Extract<ManagedBinaryPlan, { kind: "unsupportedGhc" }>, automatic: boolean): Promise<MenuAction> {
    while (true) {
      const choice = await selectLoreMenu(ui, `Lore needs a custom build\n\nThis project uses GHC ${plan.ghcVersion}, but Lore does not provide a prebuilt binary for that exact compiler version.`, ["Build instructions", "Set command to run Lore", "Disable Lore for this project", "Not now"]);
      if (choice === "Build instructions") { ui.notify?.(buildInstructions(plan), "info"); continue; }
      if (choice === "Set command to run Lore") return useManualCommand(ui);
      if (choice === "Disable Lore for this project") { await disableLore(); return "done"; }
      if (choice === "Not now" || automatic) { await notNow(); return "done"; }
      return "done";
    }
  }

  async function failureMenu(ui: SetupUi, automatic: boolean): Promise<MenuAction> {
    const failure = state.kind === "failed" ? state.failure : undefined;
    const title = failureTitle(failure);
    const choice = await selectLoreMenu(ui, `${title.heading}\n\n${title.message}${failure?.summary ? `\n\n${failure.summary}` : ""}`, ["Retry", "Set command to run Lore", "Details", "Disable Lore for this project", "Not now"]);
    if (choice === "Retry") { await retryLast(); return state.kind === "failed" ? "continue" : "done"; }
    else if (choice === "Set command to run Lore") return useManualCommand(ui);
    else if (choice === "Details") { ui.notify?.(failure?.details ?? "No details available", "error"); return "continue"; }
    else if (choice === "Disable Lore for this project") await disableLore();
    else if (choice === "Not now" || automatic) await notNow();
    return "done";
  }

  async function useManualCommand(ui: SetupUi): Promise<MenuAction> {
    const result = await manualCommandFlow(ui);
    return result === "cancelled" || state.kind === "failed" ? "continue" : "done";
  }

  async function manualCommandFlow(ui: SetupUi): Promise<ManualCommandResult> {
    if (!ui.custom) throw new Error("Lore command configuration requires Pi custom UI support");
    const text = await ui.custom<string>((_tui, theme, keybindings, done) =>
      framedCustomUi(
        new TextInputComponent(
          ["Lore command", "", `Current: ${loadLoreConfig(input.host).command ?? "managed automatically"}`, "Enter a command name or executable path"].join("\n"),
          theme as CustomUiTheme,
          keybindings as CustomUiKeybindings,
          done,
        ),
        theme as CustomUiTheme,
        "enter confirm  escape/ctrl+c cancel",
      ),
    );
    if (text === undefined) return "cancelled";
    try {
      const config = loadLoreConfig(input.host);
      const command = normalizeLoreCommand(effectiveLoreProjectDir(config, input.host), text);
      const options = binaryOptions();
      const { probe, target } = await projectIdentity(options);
      await validateLoreCommandForProject({ command, cwd: config.cwd, expectedLoreVersion: loadBundledBinaryManifest().loreVersion, expectedGhcVersion: probe.ghcVersion, expectedTarget: target, env: options.env, run: options.run });
      setLoreCommand(configProjectDir, command);
      const updated = loadLoreConfig(input.host);
      if (!updated.enabled) {
        await transition({ kind: "disabled" });
        return "applied";
      }
      await startProcess({ ...updated, command }, "custom");
      return "applied";
    } catch (error) {
      const message = manualErrorMessage(error);
      const choice = await selectLoreMenu(ui, message, ["Try again", "Back"]);
      return choice === "Try again" ? manualCommandFlow(ui) : "cancelled";
    }
  }

  async function projectIdentity(options: BinaryManagerOptions): Promise<{ probe: ProbeResult; target: BinaryTarget }> {
    return { probe: options.probe ? await options.probe(options.projectDir) : await probeProjectGhcVersion({ projectDir: options.projectDir, run: options.run, env: options.env, timeoutMs: options.timeoutMs }), target: options.target ?? currentBinaryTarget() };
  }

  async function retryLast(): Promise<void> {
    const failure = state.kind === "failed" ? state.failure : undefined;
    if (!lastProcessConfig || (failure && failure.kind !== "process")) {
      await startAutomatically();
      return;
    }
    await input.runtime.stop().catch(() => undefined);
    await startProcess(lastProcessConfig, lastProcessConfig.command === loadLoreConfig(input.host).command ? "custom" : "managed");
  }

  async function disableLore(): Promise<void> {
    setLoreEnabled(configProjectDir, false);
    await input.runtime.stop().catch(() => undefined);
    await transition({ kind: "disabled" });
    input.deactivate?.();
  }

  async function notNow(): Promise<void> {
    suppressedAutomaticPrompt = true;
    await input.runtime.stop().catch(() => undefined);
    await transition({ kind: "skippedForSession" });
    input.deactivate?.();
  }

  async function setProjectEnabled(enabled: boolean, ctx?: unknown): Promise<void> {
    if (ctx) currentCtx = ctx;
    suppressedAutomaticPrompt = false;
    if (!enabled) {
      await disableLore();
      return;
    }
    if (loadLoreConfig(input.host).enabled && state.kind !== "disabled") return;
    setLoreEnabled(configProjectDir, true);
    await transition({ kind: "idle" });
    await startAutomatically();
  }

  async function configureCommand(ctx?: unknown): Promise<ManualCommandResult> {
    if (ctx) currentCtx = ctx;
    return manualCommandFlow(requireSetupUi(currentCtx));
  }

  async function restartOrSetup(ctx?: unknown): Promise<RestartOutcome> {
    if (ctx) currentCtx = ctx;
    if (state.kind === "ready") {
      await retryLast();
      return state.kind === "failed" ? { kind: "failed", summary: state.failure.summary } : { kind: "restarted" };
    }
    if (state.kind === "disabled") return { kind: "disabled" };
    await openSetup(ctx);
    if (state.kind === "ready") return { kind: "restarted" };
    if (state.kind === "failed") return { kind: "failed", summary: state.failure.summary };
    if (state.kind === "disabled") return { kind: "disabled" };
    if (state.kind === "skippedForSession") return { kind: "cancelled" };
    return { kind: "openedSetup" };
  }

  function statusText(): string {
    const lines = [`Lore status: ${state.kind}`];
    if (state.kind === "ready" || state.kind === "starting") lines.push(`Mode: ${state.mode}`, `Command: ${state.command}`, ...(state.ghcVersion ? [`GHC: ${state.ghcVersion}`] : []), ...(state.provider ? [`Provider: ${state.provider}`] : []));
    if (state.kind === "waitingForSetup" && state.plan) lines.push(`Reason: ${state.reason}`, `GHC: ${state.plan.ghcVersion}`, `Provider: ${state.plan.provider}`);
    if (state.kind === "failed") lines.push(`Failure: ${state.failure.summary}`);
    return lines.join("\n");
  }

  return { startAutomatically, attachContext, openSetup, setProjectEnabled, configureCommand, statusText, restartOrSetup, getState: () => state };
}

function isSetupTerminalState(kind: LoreStartupState["kind"]): boolean {
  return kind === "ready" || kind === "disabled" || kind === "skippedForSession";
}

function requireSetupUi(ctx: unknown): SetupUi {
  const ui = commandUi(ctx);
  if (!ui?.select && !ui?.custom) throw new Error("Lore setup requires Pi custom or select UI support");
  return ui;
}

export function commandUi(ctx: unknown): SetupUi | undefined {
  if (!ctx || typeof ctx !== "object") return undefined;
  const ui = (ctx as { ui?: unknown }).ui;
  if (!ui || typeof ui !== "object") return undefined;
  const notify = (ui as { notify?: unknown }).notify;
  const select = (ui as { select?: unknown }).select;
  const custom = (ui as { custom?: unknown }).custom;
  return { notify: typeof notify === "function" ? notify as SetupUi["notify"] : undefined, select: typeof select === "function" ? select as SetupUi["select"] : undefined, custom: typeof custom === "function" ? custom as SetupUi["custom"] : undefined };
}

function failureTitle(failure: LoreStartupFailure | undefined): { heading: string; message: string } {
  if (failure?.kind === "download") {
    return { heading: "Lore setup failed", message: "The matching Lore binary could not be downloaded or installed." };
  }
  if (failure?.kind === "planning") {
    return { heading: "Lore setup failed", message: "Lore could not determine or prepare the server binary for this project." };
  }
  return { heading: "Lore could not start", message: "The `lore-mcp` process exited before Lore finished starting." };
}

function buildInstructions(plan: Extract<ManagedBinaryPlan, { kind: "unsupportedGhc" }>): string {
  return [`Build Lore for GHC ${plan.ghcVersion}`, "", "git clone https://github.com/catdarick/lore.git", "cd lore", `git checkout v${plan.loreVersion}`, `cabal build exe:lore-mcp -w ghc-${plan.ghcVersion}`, `cabal list-bin exe:lore-mcp -w ghc-${plan.ghcVersion}`, "", "The matching GHC must already be installed and available to Cabal.", "Then open /lore-settings and choose “Set command to run Lore”."].join("\n");
}

function shortStartupFailure(details: string): string {
  const timeout = details.match(/timed out after ([^\n.]+)/i);
  if (timeout) return `Startup timed out after ${timeout[1]}`;
  const exit = details.match(/exit code (\d+)/i);
  if (exit) return `Exit code ${exit[1]}`;
  return details.split(/\r?\n/).find((line) => line.trim())?.slice(0, 160) ?? "Startup failed";
}

function manualErrorMessage(error: unknown): string {
  const message = error instanceof Error ? error.message : String(error);
  const ghc = message.match(/GHC version mismatch\. Expected ([^,]+), got ([^\n]+)/);
  if (ghc) return `This binary was built for GHC ${ghc[2]}, but the project uses GHC ${ghc[1]}. Lore requires an exact compiler match.`;
  return message;
}

function startupDisplayStatus(state: LoreStartupState): LoreExtensionStatus {
  switch (state.kind) {
    case "idle":
    case "planning":
      return "preparing";
    case "waitingForSetup":
      return "setupRequired";
    case "installing":
      return "downloading";
    case "starting":
      return "starting";
    case "ready":
      return "active";
    case "disabled":
      return "disabled";
    case "skippedForSession":
      return "paused";
    case "failed":
      return "unavailable";
  }
}

class TextInputComponent {
  private text = "";
  private readonly title: string;
  private readonly theme: CustomUiTheme;
  private readonly keybindings: CustomUiKeybindings;
  private readonly done: (value: string | undefined) => void;

  constructor(
    title: string,
    theme: CustomUiTheme,
    keybindings: CustomUiKeybindings,
    done: (value: string | undefined) => void,
  ) {
    this.title = title;
    this.theme = theme;
    this.keybindings = keybindings;
    this.done = done;
  }

  render(width: number): string[] {
    const safeWidth = Math.max(1, width || 80);
    const titleLines = this.title.split(/\r?\n/);
    const firstHeading = titleLines.findIndex((line) => line.trim().length > 0);
    return [
      ...titleLines.map((line, index) => {
        const clipped = line.slice(0, safeWidth);
        return index === firstHeading ? styleUiTitle(this.theme, clipped) : clipped;
      }),
      "",
      (this.text || " ").slice(0, safeWidth),
    ];
  }

  handleInput(data: string): void {
    if (this.keybindings.matches(data, "tui.select.cancel")) {
      this.done(undefined);
      return;
    }
    if (this.keybindings.matches(data, "tui.input.submit")) {
      this.done(this.text);
      return;
    }
    if (this.keybindings.matches(data, "tui.editor.deleteCharBackward")) {
      this.text = this.text.slice(0, -1);
      return;
    }
    if (!data.startsWith("\u001b")) this.text += data;
  }

  invalidate(): void {
    // stateless render cache
  }
}
