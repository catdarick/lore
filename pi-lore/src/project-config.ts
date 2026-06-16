import { existsSync, readFileSync, writeFileSync, mkdirSync, renameSync } from "node:fs";
import { dirname, isAbsolute, join, resolve } from "node:path";
import type { PiHost } from "./types.ts";

export const projectLoreConfigRelativePath = join(".pi", "lore.config.json");
export const legacyProjectLoreConfigRelativePath = join(".pi", "extensions", "lore", "config.json");

export type ProjectLoreConfigPatch = Record<string, unknown>;

export function projectDirFromHost(host: Pick<PiHost, "projectDir" | "cwd"> = {}): string {
  return resolve(String(host.projectDir ?? host.cwd ?? process.cwd()));
}

export function projectLoreConfigPath(projectDirOrHost: string | Pick<PiHost, "projectDir" | "cwd">): string {
  const projectDir = typeof projectDirOrHost === "string" ? resolve(projectDirOrHost) : projectDirFromHost(projectDirOrHost);
  return join(projectDir, projectLoreConfigRelativePath);
}

export function readProjectLoreConfig(projectDir: string): Record<string, unknown> {
  for (const relative of [projectLoreConfigRelativePath, legacyProjectLoreConfigRelativePath]) {
    const path = join(projectDir, relative);
    if (!existsSync(path)) continue;
    return readJsonObjectFile(path, "Invalid Lore extension configuration");
  }
  return {};
}

export function updateProjectLoreConfig(projectDirOrHost: string | PiHost, patch: ProjectLoreConfigPatch): string {
  const projectDir = typeof projectDirOrHost === "string" ? resolve(projectDirOrHost) : projectDirFromHost(projectDirOrHost);
  const path = projectLoreConfigPath(projectDir);
  const existing = existsSync(path) ? readJsonObjectFile(path, "Invalid Lore project config") : readProjectLoreConfig(projectDir);
  const next = deepMergeObject(existing, patch);
  removeUnsupportedLoreSettings(next);
  validateProjectLoreConfig(projectDir, next);
  mkdirSync(dirname(path), { recursive: true });
  const tmp = `${path}.${process.pid}.tmp`;
  writeFileSync(tmp, `${JSON.stringify(next, null, 2)}\n`, "utf8");
  renameSync(tmp, path);
  return path;
}

export function setLoreEnabled(projectDirOrHost: string | PiHost, enabled: boolean): string {
  return updateProjectLoreConfig(projectDirOrHost, { enabled });
}

export function normalizeLoreCommand(projectDirOrHost: string | Pick<PiHost, "projectDir" | "cwd">, input: string): string {
  const command = input.trim();
  if (command.length === 0) throw new Error("Lore command must not be empty");
  if (!isPathLikeCommand(command)) return command;
  const projectDir = typeof projectDirOrHost === "string" ? resolve(projectDirOrHost) : projectDirFromHost(projectDirOrHost);
  return resolve(projectDir, command);
}

export function setLoreCommand(projectDirOrHost: string | PiHost, input: string): string {
  const command = normalizeLoreCommand(projectDirOrHost, input);
  return updateProjectLoreConfig(projectDirOrHost, { command, args: [] });
}

export function deepMergeObject(left: Record<string, unknown>, right: Record<string, unknown>): Record<string, unknown> {
  const result: Record<string, unknown> = { ...left };
  for (const [key, value] of Object.entries(right)) {
    if (isPlainObject(result[key]) && isPlainObject(value)) result[key] = deepMergeObject(result[key] as Record<string, unknown>, value);
    else if (value !== undefined) result[key] = value;
  }
  return result;
}

function readJsonObjectFile(path: string, prefix: string): Record<string, unknown> {
  try {
    const parsed = JSON.parse(readFileSync(path, "utf8"));
    if (!isPlainObject(parsed)) throw new Error("expected an object");
    return parsed;
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`${prefix} at ${path}: ${message}`);
  }
}

function validateProjectLoreConfig(projectDir: string, config: Record<string, unknown>): void {
  void projectDir;
  if (config.enabled !== undefined && typeof config.enabled !== "boolean") throw new Error("Invalid Lore config: enabled must be a boolean");
  if (config.command !== undefined && (typeof config.command !== "string" || config.command.length === 0)) throw new Error("Invalid Lore config: command must be a non-empty string");
  if (config.tools !== undefined && !isPlainObject(config.tools)) throw new Error("Invalid Lore config: tools must be an object");
  if (config.recovery !== undefined && !isPlainObject(config.recovery)) throw new Error("Invalid Lore config: recovery must be an object");
  const recovery = config.recovery;
  if (isPlainObject(recovery)) {
    if (recovery.compilation !== undefined && typeof recovery.compilation !== "boolean") throw new Error("Invalid Lore config: recovery.compilation must be a boolean");
    if (recovery.tests !== undefined && typeof recovery.tests !== "boolean") throw new Error("Invalid Lore config: recovery.tests must be a boolean");
  }
  const tools = config.tools;
  if (isPlainObject(tools) && tools.disabled !== undefined && (!Array.isArray(tools.disabled) || !tools.disabled.every((item) => typeof item === "string"))) throw new Error("Invalid Lore config: tools.disabled must be an array of strings");
}

function removeUnsupportedLoreSettings(config: Record<string, unknown>): void {
  const tools = config.tools;
  if (isPlainObject(tools)) delete tools.enabled;
}

function isPlainObject(value: unknown): value is Record<string, unknown> {
  return Boolean(value) && typeof value === "object" && !Array.isArray(value);
}

function isPathLikeCommand(command: string): boolean {
  return isAbsolute(command) || command.includes("/") || command.includes("\\");
}
