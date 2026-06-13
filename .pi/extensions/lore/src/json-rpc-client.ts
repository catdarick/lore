import { spawn, type ChildProcessWithoutNullStreams } from "node:child_process";
import { EventEmitter } from "node:events";
import { StringDecoder } from "node:string_decoder";
import { LoreCancelledError, LoreProcessError, LoreProtocolError, LoreRemoteError, LoreTimeoutError } from "./errors.ts";
import type { JsonValue, LoreConfig } from "./types.ts";

type PendingRequest = {
  id: number;
  method: string;
  resolve: (value: unknown) => void;
  reject: (error: Error) => void;
  timer?: NodeJS.Timeout;
  abortHandler?: () => void;
  signal?: AbortSignal;
};

export type JsonRpcClientEvents = {
  stderr: [string];
  fatal: [Error];
};

export class JsonRpcClient extends EventEmitter<JsonRpcClientEvents> {
  private readonly config: LoreConfig;
  private child?: ChildProcessWithoutNullStreams;
  private nextId = 1;
  private pending = new Map<number, PendingRequest>();
  private stdoutBuffer = "";
  private stdoutDecoder = new StringDecoder("utf8");
  private generation = 0;
  private stopping = false;

  constructor(config: LoreConfig) {
    super();
    this.config = config;
  }

  get isRunning(): boolean {
    return Boolean(this.child && !this.child.killed && this.child.exitCode === null);
  }

  start(): void {
    if (this.isRunning) {
      return;
    }
    this.stopping = false;
    this.generation += 1;
    this.stdoutBuffer = "";
    this.stdoutDecoder = new StringDecoder("utf8");
    const child = spawn(this.config.command, this.config.args, {
      cwd: this.config.cwd,
      env: { ...process.env, ...this.config.env },
      stdio: ["pipe", "pipe", "pipe"],
    });
    this.child = child;
    const processGeneration = this.generation;

    child.stdout.on("data", (chunk: Buffer) => {
      if (processGeneration !== this.generation) {
        return;
      }
      this.handleStdout(chunk);
    });
    child.stderr.on("data", (chunk: Buffer) => {
      if (processGeneration === this.generation) {
        this.emit("stderr", chunk.toString("utf8"));
      }
    });
    child.on("error", (error) => {
      if (processGeneration === this.generation) {
        this.failAll(new LoreProcessError(`Lore process error: ${error.message}`));
      }
    });
    child.on("exit", (code, signal) => {
      if (processGeneration !== this.generation || this.stopping) {
        return;
      }
      this.failAll(new LoreProcessError(`Lore process exited unexpectedly (code ${code}, signal ${signal})`));
      this.emit("fatal", new LoreProcessError("Lore process exited unexpectedly"));
    });
  }

  async stop(): Promise<void> {
    this.stopping = true;
    this.generation += 1;
    this.failAll(new LoreProcessError("Lore process stopped"));
    const child = this.child;
    this.child = undefined;
    if (!child || child.exitCode !== null) {
      return;
    }
    await new Promise<void>((resolve) => {
      const timer = setTimeout(() => {
        child.kill("SIGKILL");
        resolve();
      }, 1_000);
      child.once("exit", () => {
        clearTimeout(timer);
        resolve();
      });
      child.kill("SIGTERM");
    });
  }

  notify(method: string, params?: JsonValue): void {
    this.write({ jsonrpc: "2.0", method, params });
  }

  request(method: string, params: JsonValue | undefined, timeoutMs: number, signal?: AbortSignal): Promise<unknown> {
    if (!this.isRunning) {
      return Promise.reject(new LoreProcessError("Lore process is not running"));
    }
    if (signal?.aborted) {
      return Promise.reject(new LoreCancelledError(`Lore request ${method} was cancelled before it started`));
    }

    const id = this.nextId++;
    const request = { jsonrpc: "2.0", id, method, params };
    const promise = new Promise<unknown>((resolve, reject) => {
      const pending: PendingRequest = { id, method, resolve, reject, signal };
      if (timeoutMs > 0) {
        pending.timer = setTimeout(() => {
          this.pending.delete(id);
          this.cleanupPending(pending);
          reject(new LoreTimeoutError(`Lore request ${method} timed out after ${timeoutMs}ms`));
          void this.stop();
        }, timeoutMs);
      }
      if (signal) {
        pending.abortHandler = () => {
          this.pending.delete(id);
          this.cleanupPending(pending);
          reject(new LoreCancelledError(`Lore request ${method} was cancelled`));
          void this.stop();
        };
        signal.addEventListener("abort", pending.abortHandler, { once: true });
      }
      this.pending.set(id, pending);
      try {
        this.write(request);
      } catch (error) {
        this.pending.delete(id);
        this.cleanupPending(pending);
        reject(error instanceof Error ? error : new Error(String(error)));
      }
    });
    promise.catch(() => undefined);
    return promise;
  }

  private write(value: unknown): void {
    if (!this.child || !this.child.stdin.writable) {
      throw new LoreProcessError("Lore process stdin is unavailable");
    }
    this.child.stdin.write(`${JSON.stringify(value)}\n`, "utf8");
  }

  private handleStdout(chunk: Buffer): void {
    this.stdoutBuffer += this.stdoutDecoder.write(chunk);
    while (true) {
      const newline = this.stdoutBuffer.indexOf("\n");
      if (newline < 0) {
        return;
      }
      const line = this.stdoutBuffer.slice(0, newline).trimEnd();
      this.stdoutBuffer = this.stdoutBuffer.slice(newline + 1);
      if (line.length === 0) {
        continue;
      }
      let message: unknown;
      try {
        message = JSON.parse(line);
      } catch {
        const error = new LoreProtocolError(`Malformed JSON-RPC output from Lore: ${line.slice(0, 200)}`);
        this.failAll(error);
        this.emit("fatal", error);
        void this.stop();
        return;
      }
      this.handleMessage(message);
    }
  }

  private handleMessage(message: unknown): void {
    if (!message || typeof message !== "object") {
      const error = new LoreProtocolError("Lore emitted a non-object JSON-RPC message");
      this.failAll(error);
      this.emit("fatal", error);
      void this.stop();
      return;
    }
    const obj = message as Record<string, unknown>;
    if (typeof obj.id !== "number") {
      return;
    }
    const pending = this.pending.get(obj.id);
    if (!pending) {
      return;
    }
    this.pending.delete(obj.id);
    this.cleanupPending(pending);
    if (obj.error) {
      const errorObj = obj.error as { message?: unknown; code?: unknown; data?: unknown };
      const messageText = typeof errorObj.message === "string" ? errorObj.message : "JSON-RPC error";
      const code = typeof errorObj.code === "number" ? errorObj.code : undefined;
      pending.reject(new LoreRemoteError(`${pending.method} failed: ${messageText}`, code, errorObj.data));
      return;
    }
    if (!("result" in obj)) {
      pending.reject(new LoreProtocolError(`${pending.method} response is missing result`));
      return;
    }
    pending.resolve(obj.result);
  }

  private cleanupPending(pending: PendingRequest): void {
    if (pending.timer) {
      clearTimeout(pending.timer);
    }
    if (pending.signal && pending.abortHandler) {
      pending.signal.removeEventListener("abort", pending.abortHandler);
    }
  }

  private failAll(error: Error): void {
    for (const pending of this.pending.values()) {
      this.cleanupPending(pending);
      pending.reject(error);
    }
    this.pending.clear();
  }
}
