import { resolve } from "node:path";
import { resolveManagedLoreBinary } from "./binary-manager.ts";
import { loadLoreConfig } from "./config.ts";
import { KnowledgeSynchronizer } from "./knowledge.ts";
import { LoreClient } from "./mcp-client.ts";
import { RecoveryManager } from "./recovery.ts";
import { SessionStateStore } from "./session-state.ts";
import { isPrivateLoreMethod, LoreToolProxy } from "./tool-proxy.ts";
import type { ExtensionRuntime, LoreConfig, LoreProcessConfig, PiEntry, PiHost } from "./types.ts";
import { LoreRecoveryUi } from "./ui.ts";
import { analyzeLoreUsage } from "./usage-stats.ts";

type ManagedResolver = (input: { projectDir: string; env: NodeJS.ProcessEnv; timeoutMs: number; onStatus?: (message: string) => void | Promise<void> }) => Promise<string>;
let managedResolver: ManagedResolver = ({ projectDir, env, timeoutMs, onStatus }) => resolveManagedLoreBinary({ projectDir, env, timeoutMs, onStatus });

export function setManagedLoreBinaryResolverForTests(resolver: ManagedResolver): () => void {
  const previous = managedResolver;
  managedResolver = resolver;
  return () => { managedResolver = previous; };
}

export async function createLoreExtension(host: PiHost = {}): Promise<ExtensionRuntime> {
  const config = loadLoreConfig(host);
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

  async function resolveProcessConfig(): Promise<LoreProcessConfig> {
    if (processConfig) return processConfig;
    if (config.command !== undefined) {
      processConfig = { ...config, command: config.command };
      return processConfig;
    }
    const env = { ...process.env, ...config.env };
    const startupCwd = config.cwd ?? resolve(String(host.projectDir ?? host.cwd ?? process.cwd()));
    const projectDir = env.LORE_PROJECT_ROOT ? resolve(startupCwd, env.LORE_PROJECT_ROOT) : startupCwd;
    const command = await managedResolver({
      projectDir,
      env,
      timeoutMs: config.startupTimeoutMs,
      onStatus: (message) => host.setStatus?.("lore-extension", message, { tone: "info" }),
    });
    processConfig = { ...config, command };
    return processConfig;
  }

  async function ensureRuntime(): Promise<{ client: LoreClient; knowledge: KnowledgeSynchronizer; recovery: RecoveryManager; proxy: LoreToolProxy; processConfig: LoreProcessConfig }> {
    const resolved = await resolveProcessConfig();
    if (client && knowledge && proxy) return { client, knowledge, recovery, proxy, processConfig: resolved };
    client = new LoreClient(resolved);
    knowledge = new KnowledgeSynchronizer(client, store, () => host.getCurrentEntryId?.());
    proxy = new LoreToolProxy({ host, config, client, knowledge, recovery, ui });
    return { client, knowledge, recovery, proxy, processConfig: resolved };
  }

  async function restoreBranchState(): Promise<void> {
    if (!knowledge) return;
    await knowledge.restoreActiveBranch();
    await recovery.restoreUi();
    loadedBranchKey = currentBranchKey();
  }

  async function ensureBranchStateLoaded(): Promise<void> {
    if (!startupReady || startupError) return;
    const key = currentBranchKey();
    if (loadedBranchKey === key) return;
    if (!branchLoadPromise) {
      branchLoadPromise = restoreBranchState().finally(() => { branchLoadPromise = undefined; });
    }
    await branchLoadPromise;
  }

  async function start(): Promise<void> {
    let resolved: LoreProcessConfig | undefined;
    try {
      const runtime = await ensureRuntime();
      resolved = runtime.processConfig;
      await host.setStatus?.("lore-extension", "Starting Lore…", { tone: "info" });
      await runtime.client.start();
      const tools = await runtime.proxy.registerAll();
      registeredToolNames = tools.map((tool) => tool.name).filter((name) => !name.startsWith("lore/"));
      startupError = undefined;
      startupReady = true;
      await host.clearStatus?.("lore-extension");
      await host.onLoreToolsRegistered?.(registeredToolNames);
    } catch (error) {
      registeredToolNames = [];
      await client?.stop();
      const message = formatStartupError(resolved ?? processConfig ?? config, error);
      startupError = message;
      startupReady = false;
      await host.setStatus?.("lore-extension", `Lore extension unavailable: ${message}`, { tone: "error" });
      await host.notify?.(`Lore extension unavailable: ${message}`, { tone: "error" });
    }
  }

  async function stop(): Promise<void> {
    await client?.stop();
  }

  async function restartLore(): Promise<void> {
    if (!client) {
      await start();
      if (startupError) throw new Error(startupError);
      return;
    }
    await client.restart();
    startupReady = true;
    startupError = undefined;
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
  host.on?.("session_compact", () => runSafely(() => (startupReady && !startupError ? knowledge?.reset() ?? Promise.resolve() : Promise.resolve())));
  host.on?.("session_shutdown", () => runSafely(stop));

  return {
    start,
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
