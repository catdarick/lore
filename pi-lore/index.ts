import { createLoreExtension } from "./src/index.ts";
import { resolve } from "node:path";
import { loadLoreConfig } from "./src/config.ts";
import { parseLoreSettingsPatch, projectLoreConfigPath, registerLoreSettingsCommand } from "./src/settings-ui.ts";
import { commandUi, createStartupCoordinator, type StartupCoordinator } from "./src/setup.ts";
import { installPiImmediateDisplayBridge, type PiImmediateDisplayOptions } from "./src/pi-immediate-display.ts";
import { formatLoreUsageStats, loreUsageStatsCustomType } from "./src/usage-stats.ts";
import { registerRecoverySummaryRenderer, registerUsageStatsRenderer } from "./src/renderers.ts";
import {
  adaptPiHost,
  currentActiveLoreToolNames,
  deactivateLoreTools,
  denormalizePiMessages,
  loreStatusTone,
  normalizePiMessages,
  syncLoreToolActivation,
  toLlmMessages,
  toExtensionContext,
  validateRequiredPiCapabilities,
  type ExtensionContext,
  type PiExtensionApi,
} from "./src/pi-adapter.ts";
import type { PiHost } from "./src/types.ts";
import type { LoreUsageStats } from "./src/usage-types.ts";

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
  let startupCoordinator: StartupCoordinator | undefined;

  pi.on?.("session_start", (_event, ctx) => {
    currentContext = toExtensionContext(ctx) ?? currentContext;
    startupCoordinator?.attachContext(ctx);
    syncLoreToolActivation(pi, startupCoordinator, loreToolNames);
  });
  pi.on?.("model_select", (_event, ctx) => {
    currentContext = toExtensionContext(ctx) ?? currentContext;
    startupCoordinator?.attachContext(ctx);
    syncLoreToolActivation(pi, startupCoordinator, loreToolNames);
  });
  pi.registerCommand?.("lore-status", {
    description: "Show Lore extension registration status",
    handler: async (_args: unknown, ctx: { ui?: { notify?: (message: string, type?: string) => void } }) => {
      const activeLoreToolNames = currentActiveLoreToolNames(pi, loreToolNames);
      const coordinatorState = startupCoordinator?.getState();
      const message = startupCoordinator?.statusText() ?? (
        startupError
          ? `Lore extension failed to start: ${startupError}`
          : activeLoreToolNames.length > 0
            ? `Lore extension active tools (${activeLoreToolNames.length}/${loreToolNames.length} registered): ${activeLoreToolNames.join(", ")}`
            : loreToolNames.length > 0
              ? `Lore extension has ${loreToolNames.length} registered tools, but none are active.`
              : "Lore extension has not registered any tools.");
      ctx.ui?.notify?.(message, loreStatusTone(coordinatorState, startupError, activeLoreToolNames.length));
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
  startupCoordinator = createStartupCoordinator({
    host,
    runtime,
    projectDir: resolve(String(host.projectDir ?? host.cwd ?? process.cwd())),
    onReady(toolNames) { loreToolNames = toolNames; },
    activate() { syncLoreToolActivation(pi, startupCoordinator, loreToolNames); },
    deactivate() { syncLoreToolActivation(pi, startupCoordinator, loreToolNames); },
  });
  registerLoreSettingsCommand(pi, host, runtime, startupCoordinator, (ctx) => {
    currentContext = toExtensionContext(ctx) ?? currentContext;
  });
  registerUsageStatsCommand(pi, runtime, host, (next) => {
    currentContext = next ?? currentContext;
  });
  registerLoreRestartCommand(pi, startupCoordinator, (next) => {
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

  startLoreRuntimeInBackground({ start: () => startupCoordinator!.startAutomatically(), getState: runtime.getState }, {
    onSettled(state, unexpectedError) {
      loreToolNames = state.registeredToolNames;
      startupError = state.startupError ?? unexpectedError?.message;
      if (!startupError) {
        syncLoreToolActivation(pi, startupCoordinator, loreToolNames);
      }
    },
  });
}

export { createLoreExtension };
export type { ExtensionRuntime, PiHost } from "./src/types.ts";

function registerLoreSystemPromptGuidance(pi: PiExtensionApi): void {
  pi.on?.("before_agent_start", (event) => appendLoreContextMarkerGuidance(event));
}

function startLoreRuntimeInBackground(
  runtime: Pick<ExtensionRuntime, "start" | "getState">,
  callbacks: {
    onSettled: (state: ReturnType<ExtensionRuntime["getState"]>, unexpectedError?: Error) => void;
  },
): void {
  setTimeout(() => {
    let unexpectedError: Error | undefined;
    void runtime
      .start()
      .catch((error) => {
        unexpectedError = error instanceof Error ? error : new Error(String(error));
      })
      .finally(() => {
        callbacks.onSettled(runtime.getState(), unexpectedError);
      });
  }, 0);
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

function registerLoreRestartCommand(
  pi: PiExtensionApi,
  runtime: { restartLore: () => Promise<void> } | StartupCoordinator | undefined,
  setCurrentContext: (next: ExtensionContext | undefined) => void,
): void {
  pi.registerCommand?.("lore-restart", {
    description: "Restart the Lore MCP binary",
    handler: async (_args: unknown, ctx: unknown) => {
      setCurrentContext(toExtensionContext(ctx));
      const ui = commandUi(ctx);
      try {
        if (runtime && "restartOrSetup" in runtime) {
          const outcome = await runtime.restartOrSetup(ctx);
          if (outcome.kind === "restarted") ui?.notify?.("Lore MCP binary restarted.", "info");
          else if (outcome.kind === "failed") ui?.notify?.(`Lore MCP binary restart failed: ${outcome.summary}`, "error");
          else if (outcome.kind === "openedSetup") ui?.notify?.("Lore setup opened.", "info");
          else if (outcome.kind === "cancelled") ui?.notify?.("Lore restart cancelled.", "warning");
          return;
        }
        ui?.notify?.("Restarting Lore MCP binary...", "info");
        await runtime?.restartLore();
        ui?.notify?.("Lore MCP binary restarted.", "info");
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        ui?.notify?.(`Lore MCP binary restart failed: ${message}`, "error");
        throw error;
      }
    },
  });
}

export const __test = {
  appendLoreContextMarkerGuidance,
  currentActiveLoreToolNames,
  deactivateLoreTools,
  syncLoreToolActivation,
  loreStatusTone,
  parseLoreSettingsPatch,
  projectLoreConfigPath,
  registerLoreSettingsCommand,
  registerLoreSystemPromptGuidance,
  registerLoreRestartCommand,
  registerUsageStatsCommand,
  startLoreRuntimeInBackground,
  normalizePiMessages,
  toLlmMessages,
};
