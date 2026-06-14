import { randomUUID } from "node:crypto";
import { existsSync, realpathSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { dirname, join } from "node:path";
import { pathToFileURL } from "node:url";
import type { PiCustomMessage, PiSendMessageOptions } from "./types.ts";

export type ImmediateDisplayInstaller = {
  appendImmediately(message: PiCustomMessage): Promise<void>;
};

export type PiImmediateDisplayOptions = PiSendMessageOptions & {
  loreDisplayOnlyImmediate: true;
  loreImmediateDisplayRequestId: string;
};

type AgentSessionConstructor = {
  prototype: {
    sendCustomMessage?: unknown;
  };
};

type InstallInput = {
  sendMessage?: (message: PiCustomMessage, options?: PiSendMessageOptions | PiImmediateDisplayOptions) => void;
  agentSessionClass?: AgentSessionConstructor;
  piVersion?: string;
  acknowledgementTimeoutMs?: number;
};

type PendingDisplay = {
  resolve(): void;
  reject(error: unknown): void;
};

type PatchState = {
  patchedClasses: WeakSet<object>;
  pending: Map<string, PendingDisplay>;
};

const patchMarker = Symbol.for("lore.pi.immediate-display.patch");
const supportedRange = ">=0.79.1 <0.80.0";
const defaultAcknowledgementTimeoutMs = 5_000;

export async function installPiImmediateDisplayBridge(input: InstallInput): Promise<ImmediateDisplayInstaller> {
  const resolved = input.agentSessionClass
    ? { AgentSession: input.agentSessionClass, version: input.piVersion ?? "0.79.1" }
    : await resolveRunningPiAgentSession();

  assertSupportedPiVersion(resolved.version);
  installPatch(resolved.AgentSession);

  return {
    async appendImmediately(message) {
      if (typeof input.sendMessage !== "function") {
        throw new Error("unsupported Pi immediate-display runtime shape: pi.sendMessage is unavailable");
      }
      const requestId = randomUUID();
      const state = getPatchState();
      const timeoutMs = input.acknowledgementTimeoutMs ?? defaultAcknowledgementTimeoutMs;
      let timeout: ReturnType<typeof setTimeout> | undefined;
      const acknowledgement = new Promise<void>((resolve, reject) => {
        state.pending.set(requestId, { resolve, reject });
        timeout = setTimeout(() => {
          rejectPending(
            requestId,
            new Error(`Pi immediate-display acknowledgement timed out after ${timeoutMs}ms`),
          );
        }, timeoutMs);
      });
      try {
        input.sendMessage(message, {
          loreDisplayOnlyImmediate: true,
          loreImmediateDisplayRequestId: requestId,
        });
        await acknowledgement;
      } finally {
        if (timeout) {
          clearTimeout(timeout);
        }
        state.pending.delete(requestId);
      }
    },
  };
}

function installPatch(AgentSession: AgentSessionConstructor): void {
  const original = AgentSession.prototype.sendCustomMessage;
  if (typeof original !== "function") {
    throw new Error("unsupported Pi immediate-display runtime shape: AgentSession.sendCustomMessage is unavailable");
  }

  const state = getPatchState();
  if (state.patchedClasses.has(AgentSession.prototype)) {
    return;
  }

  AgentSession.prototype.sendCustomMessage = async function patchedSendCustomMessage(
    this: unknown,
    message: PiCustomMessage,
    options?: PiSendMessageOptions | PiImmediateDisplayOptions,
  ) {
    if (options?.loreDisplayOnlyImmediate !== true) {
      return (original as (this: unknown, message: PiCustomMessage, options?: PiSendMessageOptions) => unknown).call(
        this,
        message,
        options,
      );
    }
    const requestId = options.loreImmediateDisplayRequestId;

    try {
      const session = this as {
        sessionManager?: { appendCustomMessageEntry?: unknown };
        _emit?: unknown;
      };
      const appendCustomMessageEntry = session.sessionManager?.appendCustomMessageEntry;
      const emit = session._emit;
      if (typeof appendCustomMessageEntry !== "function" || typeof emit !== "function") {
        throw new Error("unsupported Pi immediate-display runtime shape: missing appendCustomMessageEntry or _emit");
      }

      const appMessage = {
        role: "custom",
        customType: message.customType,
        content: message.content,
        display: message.display,
        details: message.details,
        timestamp: Date.now(),
      };

      appendCustomMessageEntry.call(
        session.sessionManager,
        message.customType,
        message.content,
        message.display,
        message.details,
      );
      emit.call(session, { type: "message_start", message: appMessage });
      emit.call(session, { type: "message_end", message: appMessage });
      resolvePending(requestId);
    } catch (error) {
      rejectPending(requestId, error);
      throw error;
    }
  };

  state.patchedClasses.add(AgentSession.prototype);
}

async function resolveRunningPiAgentSession(): Promise<{ AgentSession: AgentSessionConstructor; version: string }> {
  const argvPath = process.argv[1];
  if (!argvPath) {
    throw new Error(`Lore extension cannot install immediate transcript rendering. Supported Pi versions: ${supportedRange}.`);
  }
  const packageRoot = await findRunningPiPackageRoot(realPathIfPossible(argvPath));
  if (!packageRoot) {
    throw new Error(`Lore extension cannot locate the running Pi package. Supported Pi versions: ${supportedRange}.`);
  }

  const packageJson = JSON.parse(await readFile(join(packageRoot, "package.json"), "utf8")) as {
    name?: unknown;
    version?: unknown;
  };
  const modulePath = join(packageRoot, "dist", "index.js");
  const module = (await import(pathToFileURL(modulePath).href)) as { AgentSession?: unknown };
  if (!module.AgentSession || typeof module.AgentSession !== "function") {
    throw new Error("unsupported Pi immediate-display runtime shape: AgentSession export is unavailable");
  }
  return {
    AgentSession: module.AgentSession as AgentSessionConstructor,
    version: typeof packageJson.version === "string" ? packageJson.version : "0.0.0",
  };
}

async function findRunningPiPackageRoot(startPath: string): Promise<string | undefined> {
  let cursor = dirname(startPath);
  for (let level = 0; level < 8; level += 1) {
    if (
      existsSync(join(cursor, "package.json")) &&
      existsSync(join(cursor, "dist", "index.js")) &&
      (await isPiCodingAgentPackage(cursor))
    ) {
      return cursor;
    }
    const parent = dirname(cursor);
    if (parent === cursor) {
      break;
    }
    cursor = parent;
  }
  return undefined;
}

async function isPiCodingAgentPackage(packageRoot: string): Promise<boolean> {
  try {
    const packageJson = JSON.parse(await readFile(join(packageRoot, "package.json"), "utf8")) as { name?: unknown };
    return packageJson.name === "@earendil-works/pi-coding-agent";
  } catch {
    return false;
  }
}

function getPatchState(): PatchState {
  const globalWithPatch = globalThis as typeof globalThis & { [patchMarker]?: PatchState };
  const state = globalWithPatch[patchMarker] ?? {
    patchedClasses: new WeakSet<object>(),
    pending: new Map<string, PendingDisplay>(),
  };
  globalWithPatch[patchMarker] = state;
  return state;
}

function resolvePending(requestId: string | undefined): void {
  if (!requestId) {
    return;
  }
  getPatchState().pending.get(requestId)?.resolve();
}

function rejectPending(requestId: string | undefined, error: unknown): void {
  if (!requestId) {
    return;
  }
  getPatchState().pending.get(requestId)?.reject(error);
}

function realPathIfPossible(path: string): string {
  try {
    return realpathSync(path);
  } catch {
    return path;
  }
}

function assertSupportedPiVersion(version: string): void {
  if (!isVersionInSupportedRange(version)) {
    throw new Error(
      `Lore extension cannot install immediate transcript rendering for Pi ${version}. Supported Pi versions: ${supportedRange}.`,
    );
  }
}

function isVersionInSupportedRange(version: string): boolean {
  const parsed = parseVersion(version);
  if (!parsed) {
    return false;
  }
  return compareVersions(parsed, [0, 79, 1]) >= 0 && compareVersions(parsed, [0, 80, 0]) < 0;
}

function parseVersion(version: string): [number, number, number] | undefined {
  const match = /^(\d+)\.(\d+)\.(\d+)/.exec(version);
  if (!match) {
    return undefined;
  }
  return [Number(match[1]), Number(match[2]), Number(match[3])];
}

function compareVersions(left: [number, number, number], right: [number, number, number]): number {
  for (let index = 0; index < left.length; index += 1) {
    const diff = left[index] - right[index];
    if (diff !== 0) {
      return diff;
    }
  }
  return 0;
}

export const __test = {
  patchMarker,
  isVersionInSupportedRange,
};
