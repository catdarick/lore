import { loadLoreConfig } from "./config.ts";
import { KnowledgeSynchronizer } from "./knowledge.ts";
import { LoreClient } from "./mcp-client.ts";
import { RecoveryManager } from "./recovery.ts";
import { SessionStateStore } from "./session-state.ts";
import { isPrivateLoreMethod, LoreToolProxy } from "./tool-proxy.ts";
import type { ExtensionRuntime, PiEntry, PiHost } from "./types.ts";
import { LoreRecoveryUi } from "./ui.ts";
import { analyzeLoreUsage } from "./usage-stats.ts";

export async function createLoreExtension(host: PiHost = {}): Promise<ExtensionRuntime> {
  const config = loadLoreConfig(host);
  const client = new LoreClient(config);
  const store = new SessionStateStore(host);
  const knowledge = new KnowledgeSynchronizer(client, store, () => host.getCurrentEntryId?.());
  const ui = new LoreRecoveryUi(host);
  let registeredToolNames: string[] = [];
  const recovery = new RecoveryManager({
    host,
    config,
    store,
    ui,
    synchronizeEmptyKnowledge: () => knowledge.synchronizeEmpty(),
  });
  const proxy = new LoreToolProxy({ host, config, client, knowledge, recovery, ui });
  let startupError: string | undefined;
  let loadedBranchKey: string | undefined;
  let branchLoadPromise: Promise<void> | undefined;

  async function restoreBranchState(): Promise<void> {
    await knowledge.restoreActiveBranch();
    await recovery.restoreUi();
    loadedBranchKey = currentBranchKey();
  }

  async function ensureBranchStateLoaded(): Promise<void> {
    const key = currentBranchKey();
    if (loadedBranchKey === key) {
      return;
    }
    if (!branchLoadPromise) {
      branchLoadPromise = restoreBranchState().finally(() => {
        branchLoadPromise = undefined;
      });
    }
    await branchLoadPromise;
  }

  async function start(): Promise<void> {
    try {
      await client.start();
      const tools = await proxy.registerAll();
      registeredToolNames = tools.map((tool) => tool.name).filter((name) => !name.startsWith("lore/"));
      startupError = undefined;
      await host.onLoreToolsRegistered?.(registeredToolNames);
    } catch (error) {
      registeredToolNames = [];
      await client.stop();
      const message = error instanceof Error ? error.message : String(error);
      startupError = message;
      await host.setStatus?.("lore-extension", `Lore extension unavailable: ${message}`, { tone: "error" });
      await host.notify?.(`Lore extension unavailable: ${message}`, { tone: "error" });
    }
  }

  async function stop(): Promise<void> {
    await client.stop();
  }

  async function restartLore(): Promise<void> {
    await client.restart();
    await restoreBranchState();
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
  host.on?.("session_compact", () => runSafely(() => knowledge.reset()));
  host.on?.("session_shutdown", () => runSafely(stop));

  return {
    start,
    stop,
    restartLore,
    async listAvailableToolNames() {
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
      if (!Array.isArray(entries)) {
        throw new Error("Lore statistics require an active Pi branch");
      }
      return analyzeLoreUsage({
        entries,
        completedRecoveries: store.current().completedRecoveries,
        registeredToolNames,
      });
    },
    getState() {
      return { ...store.current(), registeredToolNames: [...registeredToolNames], startupError };
    },
  };
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
