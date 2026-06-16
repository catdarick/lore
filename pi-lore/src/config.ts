import { resolve } from "node:path";
import type { LoreConfig, PiHost } from "./types.ts";
import { readProjectLoreConfig } from "./project-config.ts";
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
  tools: unknown;
  recovery: unknown;
  enabled: unknown;
}>;

const defaults = {
  args: [] as string[],
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
  tools: {
    disabled: [] as string[],
  },
  recovery: {
    compilation: true,
    tests: true,
  },
  enabled: true,
};

export function effectiveLoreProjectDir(config: LoreConfig, host: PiHost = {}): string {
  const startupCwd = config.cwd ?? resolve(String(host.projectDir ?? host.cwd ?? process.cwd()));
  return config.env.LORE_PROJECT_ROOT ? resolve(startupCwd, config.env.LORE_PROJECT_ROOT) : startupCwd;
}

export function loadLoreConfig(host: PiHost = {}): LoreConfig {
  const projectDir = resolve(String(host.projectDir ?? host.cwd ?? process.cwd()));
  const hostConfig = normalizeHostConfig(host.getConfig?.("lore") ?? host.getConfig?.());
  const projectConfig = host.projectTrusted === false ? {} : readProjectLoreConfig(projectDir);
  const merged = deepMerge(defaults, deepMerge(projectConfig, hostConfig));

  const command = merged.command === undefined ? undefined : requireString(merged.command, "command");
  const args = requireStringArray(merged.args, "args");
  const env = requireStringRecord(merged.env, "env");
  const startupTimeoutMs = requirePositiveInteger(merged.startupTimeoutMs, "startupTimeoutMs");
  const defaultToolTimeoutMs = requirePositiveInteger(merged.defaultToolTimeoutMs, "defaultToolTimeoutMs");
  const toolTimeoutMs = requireNumberRecord(merged.toolTimeoutMs, "toolTimeoutMs");
  const summaryTimeoutMs = requirePositiveInteger(merged.summaryTimeoutMs, "summaryTimeoutMs");
  const maxInlineDiffBytes = requirePositiveInteger(merged.maxInlineDiffBytes, "maxInlineDiffBytes");
  const allowToolOverride = requireBoolean(merged.allowToolOverride, "allowToolOverride");
  const tools = requireToolsConfig(merged.tools, "tools");
  const recovery = requireRecoveryConfig(merged.recovery, "recovery");
  const enabled = requireBoolean(merged.enabled, "enabled");
  const cwd = merged.cwd === undefined ? projectDir : resolve(projectDir, requireString(merged.cwd, "cwd"));
  const stateDir =
    merged.stateDir === undefined
      ? extensionDataDir(projectDir)
      : resolve(projectDir, requireString(merged.stateDir, "stateDir"));

  return {
    ...(command === undefined ? {} : { command }),
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
    tools,
    recovery,
    enabled,
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

function requireToolsConfig(value: unknown, name: string): LoreConfig["tools"] {
  if (!isRecord(value)) {
    throw new Error(`Invalid Lore config: ${name} must be an object`);
  }
  const disabled = value.disabled === undefined ? [] : requireStringArray(value.disabled, `${name}.disabled`);
  return { disabled: uniqueSorted(disabled) };
}

function requireRecoveryConfig(value: unknown, name: string): LoreConfig["recovery"] {
  if (!isRecord(value)) {
    throw new Error(`Invalid Lore config: ${name} must be an object`);
  }
  return {
    compilation: value.compilation === undefined ? true : requireBoolean(value.compilation, `${name}.compilation`),
    tests: value.tests === undefined ? true : requireBoolean(value.tests, `${name}.tests`),
  };
}

function uniqueSorted(values: string[]): string[] {
  return [...new Set(values)].sort();
}
