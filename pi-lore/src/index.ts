import { loadLoreConfig } from "./config.ts";
import { showLoreExtensionStatus } from "./extension-status.ts";
import { KnowledgeSynchronizer } from "./knowledge.ts";
import { LoreClient } from "./mcp-client.ts";
import { RecoveryManager } from "./recovery.ts";
import { SessionStateStore } from "./session-state.ts";
import { isPrivateLoreMethod, LoreToolProxy } from "./tool-proxy.ts";
import type { ExtensionRuntime, LoreConfig, LoreProcessConfig, PiEntry, PiHost } from "./types.ts";
import { LoreRecoveryUi } from "./ui.ts";
import { analyzeLoreUsage } from "./usage-stats.ts";

export async function createLoreExtension(host: PiHost = {}): Promise<ExtensionRuntime> {
  let config = loadLoreConfig(host);
  const store = new SessionStateStore(host);
  const ui = new LoreRecoveryUi(host);
  let client: LoreClient | undefined;
  let knowledge: KnowledgeSynchronizer | undefined;
  const recovery = new RecoveryManager({
    host,
    config,
    store,
    ui,
    synchronizeEmptyKnowledge: () => knowledge?.synchronizeEmpty() ?? Promise.resolve(),
  });
  let proxy: LoreToolProxy | undefined;
  let processConfig: LoreProcessConfig | undefined;
  let registeredToolNames: string[] = [];
  let startupError: string | undefined;
  let startupReady = false;
  let loadedBranchKey: string | undefined;
  let branchLoadPromise: Promise<void> | undefined;
  let runtimeGeneration = 0;

  async function resolveProcessConfig(): Promise<LoreProcessConfig> {
    if (processConfig) return processConfig;
    if (config.command === undefined) {
      throw new Error("Lore process configuration is unresolved; use startResolved with an explicit lore-mcp command");
    }
    processConfig = { ...config, command: config.command };
    return processConfig;
  }

  function invalidateRuntimeState(): void {
    runtimeGeneration += 1;
    startupReady = false;
    loadedBranchKey = undefined;
    branchLoadPromise = undefined;
  }

  async function startResolved(resolved: LoreProcessConfig): Promise<{ ok: true; registeredToolNames: string[] } | { ok: false; error: Error }> {
    invalidateRuntimeState();
    await client?.stop().catch(() => undefined);
    config = resolved;
    processConfig = resolved;
    client = undefined;
    knowledge = undefined;
    try {
      await start();
      if (startupError) return { ok: false, error: new Error(startupError) };
      return { ok: true, registeredToolNames: [...registeredToolNames] };
    } catch (error) {
      return { ok: false, error: error instanceof Error ? error : new Error(String(error)) };
    }
  }

  async function ensureRuntime(): Promise<{ client: LoreClient; knowledge: KnowledgeSynchronizer; recovery: RecoveryManager; proxy: LoreToolProxy; processConfig: LoreProcessConfig }> {
    const resolved = await resolveProcessConfig();
    if (client && knowledge && proxy) return { client, knowledge, recovery, proxy, processConfig: resolved };
    client = new LoreClient(resolved);
    knowledge = new KnowledgeSynchronizer(client, store, () => host.getCurrentEntryId?.());
    if (proxy) proxy.replaceRuntime({ config, client, knowledge });
    else proxy = new LoreToolProxy({ host, config, client, knowledge, recovery, ui });
    return { client, knowledge, recovery, proxy, processConfig: resolved };
  }

  async function restoreBranchState(): Promise<void> {
    const generation = runtimeGeneration;
    if (!knowledge) return;
    await knowledge.restoreActiveBranch();
    await recovery.restoreUi();
    if (generation !== runtimeGeneration) return;
    loadedBranchKey = currentBranchKey();
  }

  async function ensureBranchStateLoaded(): Promise<void> {
    if (!startupReady || startupError) return;
    const key = currentBranchKey();
    if (loadedBranchKey === key) return;
    if (!branchLoadPromise) {
      const pending = restoreBranchState().finally(() => {
        if (branchLoadPromise === pending) branchLoadPromise = undefined;
      });
      branchLoadPromise = pending;
    }
    await branchLoadPromise;
  }

  async function start(): Promise<void> {
    let resolved: LoreProcessConfig | undefined;
    try {
      const runtime = await ensureRuntime();
      resolved = runtime.processConfig;
      await showLoreExtensionStatus(host, "starting");
      await runtime.client.start();
      if (registeredToolNames.length === 0) {
        const tools = await runtime.proxy.registerAll();
        registeredToolNames = tools.map((tool) => tool.name).filter((name) => !name.startsWith("lore/"));
      }
      startupError = undefined;
      startupReady = true;
      await showLoreExtensionStatus(host, "active");
      await host.onLoreToolsRegistered?.(registeredToolNames);
    } catch (error) {
      if (registeredToolNames.length === 0) registeredToolNames = [];
      await client?.stop();
      const message = formatStartupError(resolved ?? processConfig ?? config, error);
      startupError = message;
      startupReady = false;
      await showLoreExtensionStatus(host, "unavailable");
    }
  }

  async function stop(): Promise<void> {
    invalidateRuntimeState();
    await client?.stop();
  }

  async function restartLore(): Promise<void> {
    await showLoreExtensionStatus(host, "starting");
    try {
      if (!client) {
        await start();
        if (startupError) throw new Error(startupError);
        return;
      }
      await client.restart();
      startupReady = true;
      startupError = undefined;
      await restoreBranchState();
      await showLoreExtensionStatus(host, "active");
    } catch (error) {
      await showLoreExtensionStatus(host, "unavailable");
      throw error;
    }
  }

  function runSafely(action: () => Promise<void>): void {
    void action().catch(async (error) => {
      const message = error instanceof Error ? error.message : String(error);
      await host.notify?.(`Lore extension lifecycle error: ${message}`, { tone: "error" });
    });
  }

  function scheduleBranchRestore(): void {
    loadedBranchKey = undefined;
    runSafely(ensureBranchStateLoaded);
  }

  function currentBranchKey(): string {
    return host.getCurrentEntryId?.() ?? "__attached__";
  }

  host.on?.("session_start", () => scheduleBranchRestore());
  host.on?.("session_tree", () => scheduleBranchRestore());
  host.on?.("session_compact", () => runSafely(() => (startupReady && !startupError ? knowledge?.reset() ?? Promise.resolve() : Promise.resolve())));
  host.on?.("session_shutdown", () => runSafely(stop));

  return {
    start,
    startResolved,
    stop,
    restartLore,
    async listAvailableToolNames() {
      if (!client) throw new Error("Lore client is not started");
      const tools = await client.listTools();
      return tools.map((tool) => tool.name).filter((name) => !isPrivateLoreMethod(name)).sort();
    },
    abandonRecovery() {
      return recovery.abandonRecovery();
    },
    async processContext(input: { rawMessages: unknown[]; normalizedEntries: PiEntry[] }): Promise<PiEntry[]> {
      await ensureBranchStateLoaded();
      return recovery.processContext(input);
    },
    async getUsageStats() {
      await ensureBranchStateLoaded();
      const entries = await host.getActiveBranchEntries?.();
      if (!Array.isArray(entries)) throw new Error("Lore statistics require an active Pi branch");
      return analyzeLoreUsage({ entries, completedRecoveries: store.current().completedRecoveries, registeredToolNames });
    },
    getState() {
      return { ...store.current(), registeredToolNames: [...registeredToolNames], startupError };
    },
  };
}

function formatStartupError(config: { command?: string; args: string[]; cwd?: string }, error: unknown): string {
  const message = error instanceof Error ? error.message : String(error);
  const lines = [message, ""];
  if (config.command) lines.push(`Lore command: ${[config.command, ...config.args].join(" ")}`);
  lines.push(`Lore cwd: ${config.cwd ?? process.cwd()}`);
  return lines.join("\n");
}

export default createLoreExtension;
export * from "./types.ts";
export { LoreClient } from "./mcp-client.ts";
export { JsonRpcClient } from "./json-rpc-client.ts";
export { decodeValidationSuccess } from "./lore-protocol.ts";
export { foldSessionEntries } from "./session-state.ts";
export { projectCompletedRecoveries } from "./context-projection.ts";
export { analyzeLoreUsage, formatLoreUsageStats } from "./usage-stats.ts";
export { updateRecoveryObligations } from "./recovery.ts";
