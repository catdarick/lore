import { JsonRpcClient } from "./json-rpc-client.ts";
import { LoreProcessError, LoreRemoteError } from "./errors.ts";
import { decodeStructuredToolResult } from "./lore-protocol.ts";
import type { JsonObject, JsonValue, LoreCallOptions, LoreProcessConfig, LoreStructuredToolResult, McpTool } from "./types.ts";

type QueueItem<T> = {
  run: () => Promise<T>;
  resolve: (value: T) => void;
  reject: (error: Error) => void;
};

export class LoreClient {
  private readonly config: LoreProcessConfig;
  private rpc?: JsonRpcClient;
  private started = false;
  private queue: Promise<unknown> = Promise.resolve();
  private lifecycleQueue: Promise<unknown> = Promise.resolve();
  private staleAfterFailure = false;
  private desiredState: "running" | "stopped" = "stopped";

  constructor(config: LoreProcessConfig) {
    this.config = config;
  }

  async start(): Promise<void> {
    this.desiredState = "running";
    await this.enqueueLifecycle(async () => {
      await this.startUnlocked();
    });
  }

  async stop(): Promise<void> {
    this.desiredState = "stopped";
    await this.enqueueLifecycle(async () => {
      await this.stopUnlocked();
    });
    await this.queue.catch(() => undefined);
  }

  async restart(): Promise<void> {
    this.desiredState = "running";
    await this.enqueueLifecycle(async () => {
      await this.stopUnlocked();
      await this.startUnlocked();
    });
  }

  async listTools(): Promise<McpTool[]> {
    const result = await this.enqueue(() => this.rawRequest("tools/list", {}, this.config.startupTimeoutMs));
    if (!result || typeof result !== "object" || !Array.isArray((result as { tools?: unknown }).tools)) {
      throw new Error("Lore tools/list returned an invalid result");
    }
    return (result as { tools: McpTool[] }).tools.map((tool) => ({
      name: tool.name,
      description: tool.description,
      inputSchema: tool.inputSchema,
    }));
  }

  async callStructured(name: string, args: unknown, options: LoreCallOptions = {}): Promise<LoreStructuredToolResult> {
    const timeoutMs = options.timeoutMs ?? this.config.toolTimeoutMs[name] ?? this.config.defaultToolTimeoutMs;
    const result = await this.enqueue(() =>
      this.rawRequest(
        "lore/tools/callStructured",
        { name, arguments: args === undefined ? {} : args } as JsonObject,
        timeoutMs,
        options.signal,
      ),
    );
    return decodeStructuredToolResult(result);
  }

  async getCachedDefinitions(): Promise<string[]> {
    const result = await this.enqueue(() =>
      this.rawRequest("lore/knowledge/getCachedDefinitions", {}, this.config.defaultToolTimeoutMs),
    );
    if (!result || typeof result !== "object" || !Array.isArray((result as { hashes?: unknown }).hashes)) {
      throw new Error("Lore getCachedDefinitions returned an invalid result");
    }
    return (result as { hashes: unknown[] }).hashes.map((hash) => {
      if (typeof hash !== "string") {
        throw new Error("Lore getCachedDefinitions returned a non-string hash");
      }
      return hash;
    });
  }

  async setCachedDefinitions(hashes: string[]): Promise<void> {
    await this.enqueue(() =>
      this.rawRequest(
        "lore/knowledge/setCachedDefinitions",
        { hashes } as JsonObject,
        this.config.defaultToolTimeoutMs,
      ),
    );
  }

  private async verifyPrivateKnowledgeRpc(rpc = this.rpc): Promise<void> {
    if (!rpc) {
      throw new Error("Lore server is not running");
    }
    const result = await this.rawRequestOn(rpc, "lore/knowledge/getCachedDefinitions", {}, this.config.startupTimeoutMs);
    if (!result || typeof result !== "object" || !Array.isArray((result as { hashes?: unknown }).hashes)) {
      throw new Error("Lore server does not support lore/knowledge/getCachedDefinitions");
    }
  }

  private async verifyStructuredToolRpc(rpc = this.rpc): Promise<void> {
    if (!rpc) {
      throw new Error("Lore server is not running");
    }
    try {
      await this.rawRequestOn(
        rpc,
        "lore/tools/callStructured",
        { name: "__pi_lore_extension_protocol_probe__", arguments: {} } as JsonObject,
        this.config.startupTimeoutMs,
      );
    } catch (error) {
      if (error instanceof LoreRemoteError && error.rpcCode !== -32601) {
        return;
      }
      throw new Error("Lore server does not support lore/tools/callStructured");
    }
  }

  private enqueue<T>(run: () => Promise<T>): Promise<T> {
    const item = new Promise<T>((resolve, reject) => {
      const queueItem: QueueItem<T> = { run, resolve, reject };
      this.queue = this.queue.then(() => this.runQueueItem(queueItem), () => this.runQueueItem(queueItem));
    });
    return item;
  }

  private async runQueueItem<T>(item: QueueItem<T>): Promise<void> {
    try {
      if (this.desiredState === "stopped") {
        throw new LoreProcessError("Lore client is stopped");
      }
      if (!this.started || !this.rpc?.isRunning || this.staleAfterFailure) {
        if (this.desiredState !== "running") {
          throw new LoreProcessError("Lore client is stopped");
        }
        await this.ensureRunning();
      }
      item.resolve(await item.run());
    } catch (error) {
      if (!(error instanceof LoreRemoteError)) {
        this.staleAfterFailure = true;
      }
      item.reject(error instanceof Error ? error : new Error(String(error)));
    }
  }

  private async ensureRunning(): Promise<void> {
    await this.enqueueLifecycle(async () => {
      if (this.desiredState !== "running") {
        throw new LoreProcessError("Lore client is stopped");
      }
      if (this.staleAfterFailure || !this.started || !this.rpc?.isRunning) {
        await this.stopUnlocked();
        if (this.desiredState !== "running") {
          throw new LoreProcessError("Lore client is stopped");
        }
        await this.startUnlocked();
      }
    });
  }

  private async startUnlocked(): Promise<void> {
    if (this.started && this.rpc?.isRunning) {
      return;
    }
    const rpc = new JsonRpcClient(this.config);
    this.rpc = rpc;
    rpc.start();
    try {
      await this.rawRequestOn(
        rpc,
        "initialize",
        {
          protocolVersion: "2024-11-05",
          capabilities: {},
          clientInfo: { name: "pi-lore-extension", version: "1.0.4" },
        },
        this.config.startupTimeoutMs,
      );
      rpc.notify("notifications/initialized");
      await this.verifyPrivateKnowledgeRpc(rpc);
      await this.verifyStructuredToolRpc(rpc);
      this.started = true;
      this.staleAfterFailure = false;
    } catch (error) {
      await this.stopRpcIfCurrent(rpc);
      throw error;
    }
  }

  private async stopUnlocked(): Promise<void> {
    this.started = false;
    const rpc = this.rpc;
    if (!rpc) {
      return;
    }
    await rpc.stop();
    if (this.rpc === rpc) {
      this.rpc = undefined;
    }
  }

  private async rawRequest(
    method: string,
    params: JsonValue | undefined,
    timeoutMs: number,
    signal?: AbortSignal,
  ): Promise<unknown> {
    if (!this.rpc?.isRunning) {
      throw new Error("Lore process is not running");
    }
    return this.rpc.request(method, params, timeoutMs, signal);
  }

  private rawRequestOn(
    rpc: JsonRpcClient,
    method: string,
    params: JsonValue | undefined,
    timeoutMs: number,
    signal?: AbortSignal,
  ): Promise<unknown> {
    return rpc.request(method, params, timeoutMs, signal);
  }

  private async stopRpcIfCurrent(rpc: JsonRpcClient): Promise<void> {
    await rpc.stop();
    if (this.rpc === rpc) {
      this.rpc = undefined;
      this.started = false;
    }
  }

  private enqueueLifecycle<T>(operation: () => Promise<T>): Promise<T> {
    const next = this.lifecycleQueue.then(operation, operation);
    this.lifecycleQueue = next.catch(() => undefined);
    return next;
  }
}
