import { execFile } from "node:child_process";
import { constants as fsConstants } from "node:fs";
import { access, mkdir, readFile, readlink, rm, lstat, writeFile } from "node:fs/promises";
import { basename, join, relative } from "node:path";
import { promisify } from "node:util";
import type { LoreConfig, RecoveryDiff } from "./types.ts";
import { ensureParent, sha256Bytes } from "./util.ts";

const execFileAsync = promisify(execFile);
const maxStoredFileBytes = 2_000_000;
const maxStoredBaselineBytes = 20_000_000;
const safeRecoveryIdPattern = /^lore-recovery-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

export class DiffCaptureError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "DiffCaptureError";
  }
}

type BaselineFile = {
  path: string;
  hash: string;
  contentBase64?: string;
  tooLarge?: boolean;
  symlinkTarget?: string;
};

type Baseline = {
  recoveryId: string;
  projectDir: string;
  files: BaselineFile[];
};

export async function captureRecoveryBaseline(config: LoreConfig, recoveryId: string): Promise<string> {
  const projectDir = config.cwd ?? process.cwd();
  assertSafeRecoveryId(recoveryId);
  const baselineDir = join(config.stateDir, "recoveries", recoveryId);
  await mkdir(baselineDir, { recursive: true });
  const files = await listProjectFiles(projectDir, config.stateDir);
  const baselineFiles: BaselineFile[] = [];
  let storedBytes = 0;
  for (const file of files) {
    const absolute = join(projectDir, file);
    const fileStat = await lstat(absolute).catch(() => undefined);
    if (!fileStat) {
      continue;
    }
    if (fileStat.isSymbolicLink()) {
      const target = await readlink(absolute);
      baselineFiles.push({ path: file, hash: `symlink:${sha256Bytes(Buffer.from(target, "utf8"))}`, symlinkTarget: target });
      continue;
    }
    if (!fileStat.isFile()) {
      continue;
    }
    const content = await readFile(absolute);
    const baselineFile: BaselineFile = {
      path: file,
      hash: sha256Bytes(content),
    };
    if (content.byteLength <= maxStoredFileBytes && storedBytes + content.byteLength <= maxStoredBaselineBytes) {
      baselineFile.contentBase64 = content.toString("base64");
      storedBytes += content.byteLength;
    } else {
      baselineFile.tooLarge = true;
    }
    baselineFiles.push(baselineFile);
  }
  const baseline: Baseline = { recoveryId, projectDir, files: baselineFiles };
  const path = baselinePath(config, recoveryId);
  await ensureParent(path);
  await writeFile(path, JSON.stringify(baseline), "utf8");
  return recoveryId;
}

export async function captureRecoveryDiff(config: LoreConfig, baselineId: string): Promise<RecoveryDiff> {
  const projectDir = config.cwd ?? process.cwd();
  assertSafeRecoveryId(baselineId);
  const path = baselinePath(config, baselineId);
  let baseline: Baseline;
  try {
    baseline = JSON.parse(await readFile(path, "utf8")) as Baseline;
  } catch (error) {
    throw new DiffCaptureError(`Unable to read recovery baseline: ${errorMessage(error)}`);
  }

  const currentFiles = new Map<string, { hash: string; content?: Buffer; tooLarge?: boolean }>();
  for (const file of await listProjectFiles(projectDir, config.stateDir)) {
    const absolute = join(projectDir, file);
    const fileStat = await lstat(absolute).catch(() => undefined);
    if (!fileStat) {
      continue;
    }
    if (fileStat.isSymbolicLink()) {
      const target = await readlink(absolute);
      currentFiles.set(file, { hash: `symlink:${sha256Bytes(Buffer.from(target, "utf8"))}` });
      continue;
    }
    if (!fileStat.isFile()) {
      continue;
    }
    const content = await readFile(absolute);
    currentFiles.set(file, {
      hash: sha256Bytes(content),
      content: content.byteLength <= maxStoredFileBytes ? content : undefined,
      tooLarge: content.byteLength > maxStoredFileBytes,
    });
  }

  const baselineFiles = new Map(baseline.files.map((file) => [file.path, file]));
  const changedPaths = [...new Set([...baselineFiles.keys(), ...currentFiles.keys()])]
    .filter((file) => baselineFiles.get(file)?.hash !== currentFiles.get(file)?.hash)
    .sort();

  const patches: string[] = [];
  let reliable = true;
  let reason: string | undefined;
  let diffCommandFailed = false;
  for (const file of changedPaths) {
    const before = baselineFiles.get(file);
    const after = currentFiles.get(file);
    if (before?.symlinkTarget || before?.tooLarge || after?.tooLarge || (before && !before.contentBase64) || (after && !after.content)) {
      reliable = false;
      reason = appendReason(reason, `Non-textual, symlinked, or large file omitted from inline diff: ${file}`);
      continue;
    }
    const patch = await diffOneFile(config, baselineId, file, before?.contentBase64, after?.content);
    if (!patch.reliable) {
      reliable = false;
      diffCommandFailed = true;
      reason = appendReason(reason, patch.reason ?? `Diff unavailable for ${file}`);
    }
    if (patch.text.trim().length > 0) {
      patches.push(patch.text);
    }
  }

  const fullPatch = patches.join("\n");
  const stats = diffStats(fullPatch, changedPaths.length);
  if (diffCommandFailed) {
    return {
      reliable: false,
      reason,
      changedPaths,
      stats,
      inlinePatch: fullPatch ? renderPatchExcerpt(fullPatch, config.maxInlineDiffBytes) : undefined,
      truncated: true,
    };
  }
  if (fullPatch.length > config.maxInlineDiffBytes) {
    const patchPath = join(config.stateDir, "recoveries", baselineId, "recovery.patch");
    await ensureParent(patchPath);
    await writeFile(patchPath, fullPatch, "utf8");
    return {
      reliable,
      reason,
      changedPaths,
      stats,
      inlinePatch: renderPatchExcerpt(fullPatch, config.maxInlineDiffBytes),
      patchPath,
      truncated: true,
    };
  }

  return {
    reliable,
    reason,
    changedPaths,
    stats,
    inlinePatch: fullPatch,
    truncated: false,
  };
}

function baselinePath(config: LoreConfig, baselineId: string): string {
  return join(recoveryArtifactsDir(config, baselineId), "baseline.json");
}

function recoveryArtifactsDir(config: LoreConfig, baselineId: string): string {
  return join(config.stateDir, "recoveries", baselineId);
}

export async function removeRecoveryArtifacts(config: LoreConfig, baselineId: string): Promise<void> {
  assertSafeRecoveryId(baselineId);
  await rm(recoveryArtifactsDir(config, baselineId), { recursive: true, force: true });
}

async function listProjectFiles(projectDir: string, stateDir: string): Promise<string[]> {
  try {
    const { stdout } = await execFileAsync("git", ["ls-files", "-co", "--exclude-standard", "-z"], {
      cwd: projectDir,
      maxBuffer: 20 * 1024 * 1024,
    });
    const stateRelative = relative(projectDir, stateDir);
    return stdout
      .split("\0")
      .filter(Boolean)
      .filter((file) => !file.startsWith(`${stateRelative}/`))
      .filter((file) => !file.includes("/node_modules/") && !file.startsWith("node_modules/"));
  } catch (error) {
    throw new DiffCaptureError(`Unable to list project files with git: ${errorMessage(error)}`);
  }
}

async function diffOneFile(
  config: LoreConfig,
  baselineId: string,
  file: string,
  beforeBase64?: string,
  after?: Buffer,
): Promise<{ text: string; reliable: boolean; reason?: string }> {
  const dir = join(config.stateDir, "recoveries", baselineId, "diff-inputs", sanitizePath(file));
  await mkdir(dir, { recursive: true });
  const beforePath = join(dir, `before-${basename(file) || "file"}`);
  const afterPath = join(dir, `after-${basename(file) || "file"}`);
  await writeFile(beforePath, beforeBase64 ? Buffer.from(beforeBase64, "base64") : Buffer.alloc(0));
  await writeFile(afterPath, after ?? Buffer.alloc(0));
  try {
    const result = await execFileAsync(
      "git",
      ["diff", "--no-index", "--no-ext-diff", "--no-color", "--", beforePath, afterPath],
      { maxBuffer: Math.max(maxStoredFileBytes * 8, config.maxInlineDiffBytes * 4) },
    );
    return { text: relabelPatch(result.stdout, file), reliable: true };
  } catch (error) {
    const maybe = error as { stdout?: string; code?: number };
    if (maybe.code === 1 && typeof maybe.stdout === "string") {
      return { text: relabelPatch(maybe.stdout, file), reliable: true };
    }
    return {
      text: "",
      reliable: false,
      reason: `diff unavailable for ${file}: ${errorMessage(error)}`,
    };
  }
}

function relabelPatch(patch: string, file: string): string {
  return patch
    .split("\n")
    .map((line) => {
      if (line.startsWith("diff --git ")) return `diff --git a/${file} b/${file}`;
      if (line.startsWith("--- ")) return `--- a/${file}`;
      if (line.startsWith("+++ ")) return `+++ b/${file}`;
      return line;
    })
    .join("\n");
}

function diffStats(patch: string, filesChanged: number): RecoveryDiff["stats"] {
  let additions = 0;
  let deletions = 0;
  for (const line of patch.split("\n")) {
    if (line.startsWith("+++") || line.startsWith("---")) {
      continue;
    }
    if (line.startsWith("+")) additions += 1;
    if (line.startsWith("-")) deletions += 1;
  }
  return { filesChanged, additions, deletions };
}

function sanitizePath(path: string): string {
  return path.replace(/[^A-Za-z0-9_.-]+/g, "_").slice(0, 180);
}

function assertSafeRecoveryId(value: string): void {
  if (!safeRecoveryIdPattern.test(value)) {
    throw new DiffCaptureError(`Unsafe recovery artifact id: ${value}`);
  }
}

function renderPatchExcerpt(patch: string, maxBytes: number): string {
  const maxChars = Math.max(0, maxBytes);
  if (patch.length <= maxChars) {
    return patch;
  }
  return `${patch.slice(0, maxChars)}\n\n[Patch excerpt truncated. Full patch retained in recovery artifacts.]`;
}

function appendReason(left: string | undefined, right: string): string {
  return left ? `${left}; ${right}` : right;
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

export async function pathExists(path: string): Promise<boolean> {
  try {
    await access(path, fsConstants.F_OK);
    return true;
  } catch {
    return false;
  }
}
