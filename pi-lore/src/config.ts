import { existsSync, readFileSync } from "node:fs";
import { join, resolve } from "node:path";
import type { LoreConfig, PiHost } from "./types.ts";
import { extensionDataDir } from "./util.ts";

type RawLoreConfig = Partial<{
  command: unknown;
  args: unknown;
  env: unknown;
  cwd: unknown;
  startupTimeoutMs: unknown;
  defaultToolTimeoutMs: unknown;
  toolTimeoutMs: unknown;
  summaryTimeoutMs: unknown;
  maxInlineDiffBytes: unknown;
  allowToolOverride: unknown;
  stateDir: unknown;
}>;

const defaults = {
  command: "stack",
  args: ["exec", "lore-mcp"] as string[],
  env: {
    LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE: "true",
    LORE_MCP_TOOL_ENABLED_NOTIFY_KNOWLEDGE_RESET: "false",
  } as Record<string, string>,
  startupTimeoutMs: 30_000,
  defaultToolTimeoutMs: 1_000_000,
  toolTimeoutMs: {
    reloadHomeModules: 300_000,
    runTestSuite: 900_000,
  } as Record<string, number>,
  summaryTimeoutMs: 1_000_000,
  maxInlineDiffBytes: 50_000,
  allowToolOverride: false,
};

export function loadLoreConfig(host: PiHost = {}): LoreConfig {
  const projectDir = resolve(String(host.projectDir ?? host.cwd ?? process.cwd()));
  const hostConfig = normalizeHostConfig(host.getConfig?.("lore") ?? host.getConfig?.());
  const projectConfig = host.projectTrusted === false ? {} : readProjectConfig(projectDir);
  const merged = deepMerge(defaults, deepMerge(projectConfig, hostConfig));

  const command = requireString(merged.command, "command");
  const args = requireStringArray(merged.args, "args");
  const env = requireStringRecord(merged.env, "env");
  const startupTimeoutMs = requirePositiveInteger(merged.startupTimeoutMs, "startupTimeoutMs");
  const defaultToolTimeoutMs = requirePositiveInteger(merged.defaultToolTimeoutMs, "defaultToolTimeoutMs");
  const toolTimeoutMs = requireNumberRecord(merged.toolTimeoutMs, "toolTimeoutMs");
  const summaryTimeoutMs = requirePositiveInteger(merged.summaryTimeoutMs, "summaryTimeoutMs");
  const maxInlineDiffBytes = requirePositiveInteger(merged.maxInlineDiffBytes, "maxInlineDiffBytes");
  const allowToolOverride = requireBoolean(merged.allowToolOverride, "allowToolOverride");
  const cwd = merged.cwd === undefined ? projectDir : resolve(projectDir, requireString(merged.cwd, "cwd"));
  const stateDir =
    merged.stateDir === undefined
      ? extensionDataDir(projectDir)
      : resolve(projectDir, requireString(merged.stateDir, "stateDir"));

  return {
    command,
    args,
    env,
    cwd,
    startupTimeoutMs,
    defaultToolTimeoutMs,
    toolTimeoutMs,
    summaryTimeoutMs,
    maxInlineDiffBytes,
    allowToolOverride,
    stateDir,
  };
}

function normalizeHostConfig(value: unknown): RawLoreConfig {
  if (!value || typeof value !== "object") {
    return {};
  }
  const obj = value as Record<string, unknown>;
  if ("lore" in obj && obj.lore && typeof obj.lore === "object") {
    return obj.lore as RawLoreConfig;
  }
  return obj as RawLoreConfig;
}

function readProjectConfig(projectDir: string): RawLoreConfig {
  for (const relative of [".pi/lore.config.json", ".pi/extensions/lore/config.json"]) {
    const path = join(projectDir, relative);
    if (!existsSync(path)) {
      continue;
    }
    try {
      return JSON.parse(readFileSync(path, "utf8")) as RawLoreConfig;
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      throw new Error(`Invalid Lore extension configuration at ${path}: ${message}`);
    }
  }
  return {};
}

function deepMerge(left: unknown, right: unknown): Record<string, unknown> {
  const result: Record<string, unknown> = isRecord(left) ? { ...left } : {};
  if (!isRecord(right)) {
    return result;
  }
  for (const [key, value] of Object.entries(right)) {
    if (isRecord(result[key]) && isRecord(value)) {
      result[key] = deepMerge(result[key], value);
    } else if (value !== undefined) {
      result[key] = value;
    }
  }
  return result;
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return Boolean(value) && typeof value === "object" && !Array.isArray(value);
}

function requireString(value: unknown, name: string): string {
  if (typeof value !== "string" || value.length === 0) {
    throw new Error(`Invalid Lore config: ${name} must be a non-empty string`);
  }
  return value;
}

function requireStringArray(value: unknown, name: string): string[] {
  if (!Array.isArray(value) || !value.every((item) => typeof item === "string")) {
    throw new Error(`Invalid Lore config: ${name} must be an array of strings`);
  }
  return value;
}

function requireStringRecord(value: unknown, name: string): Record<string, string> {
  if (!isRecord(value)) {
    throw new Error(`Invalid Lore config: ${name} must be an object`);
  }
  const out: Record<string, string> = {};
  for (const [key, item] of Object.entries(value)) {
    if (typeof item !== "string") {
      throw new Error(`Invalid Lore config: ${name}.${key} must be a string`);
    }
    out[key] = item;
  }
  return out;
}

function requireNumberRecord(value: unknown, name: string): Record<string, number> {
  if (!isRecord(value)) {
    throw new Error(`Invalid Lore config: ${name} must be an object`);
  }
  const out: Record<string, number> = {};
  for (const [key, item] of Object.entries(value)) {
    out[key] = requirePositiveInteger(item, `${name}.${key}`);
  }
  return out;
}

function requirePositiveInteger(value: unknown, name: string): number {
  if (!Number.isSafeInteger(value) || Number(value) <= 0) {
    throw new Error(`Invalid Lore config: ${name} must be a positive integer`);
  }
  return Number(value);
}

function requireBoolean(value: unknown, name: string): boolean {
  if (typeof value !== "boolean") {
    throw new Error(`Invalid Lore config: ${name} must be a boolean`);
  }
  return value;
}
