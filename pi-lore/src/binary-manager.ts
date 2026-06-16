import { createHash, randomBytes } from "node:crypto";
import { createReadStream, createWriteStream } from "node:fs";
import { promises as fs } from "node:fs";
import { homedir } from "node:os";
import { dirname, join } from "node:path";
import { pipeline } from "node:stream/promises";
import { createGunzip } from "node:zlib";
import { assetDownloadUrl, currentBinaryTarget, findManifestAsset, loadBundledBinaryManifest, targetString, type BinaryAsset, type BinaryManifest, type BinaryTarget } from "./binary-manifest.ts";
import { runCommand, type CommandRunner } from "./subprocess.ts";
import { probeProjectGhcVersion, type ProbeResult } from "./toolchain-probe.ts";

export type BinaryManagerOptions = {
  projectDir: string;
  cacheRoot?: string;
  manifest?: BinaryManifest;
  target?: BinaryTarget;
  run?: CommandRunner;
  env?: NodeJS.ProcessEnv;
  download?: Downloader;
  probe?: (projectDir: string, options?: { env?: NodeJS.ProcessEnv; timeoutMs?: number }) => Promise<ProbeResult>;
  timeoutMs?: number;
  downloadTimeoutMs?: number;
  onStatus?: (message: string) => void | Promise<void>;
};

export type Downloader = (url: string, destination: string, timeoutMs?: number) => Promise<void>;

export type ManagedBinaryPlan =
  | { kind: "ready"; path: string; provider: ProbeResult["provider"]; ghcVersion: string; target: BinaryTarget }
  | { kind: "downloadRequired"; provider: ProbeResult["provider"]; ghcVersion: string; target: BinaryTarget; manifest: BinaryManifest; asset: BinaryAsset; destination: string }
  | { kind: "unsupportedGhc"; provider: ProbeResult["provider"]; ghcVersion: string; target: BinaryTarget; loreVersion: string; supportedGhcVersions: string[] };

export async function planManagedLoreBinary(options: BinaryManagerOptions): Promise<ManagedBinaryPlan> {
  await options.onStatus?.("Detecting project GHC version…");
  const probe = options.probe ? await options.probe(options.projectDir, { env: options.env, timeoutMs: options.timeoutMs }) : await probeProjectGhcVersion({ projectDir: options.projectDir, run: options.run, env: options.env, timeoutMs: options.timeoutMs });
  const manifest = options.manifest ?? loadBundledBinaryManifest();
  const target = options.target ?? currentBinaryTarget();
  const asset = manifest.assets.find((candidate) => candidate.ghcVersion === probe.ghcVersion && candidate.platform === target.platform && candidate.arch === target.arch && candidate.libc === target.libc);
  if (!asset) {
    const supportedGhcVersions = [...new Set(manifest.assets.filter((candidate) => candidate.platform === target.platform && candidate.arch === target.arch && candidate.libc === target.libc).map((candidate) => candidate.ghcVersion))].sort();
    return { kind: "unsupportedGhc", provider: probe.provider, ghcVersion: probe.ghcVersion, target, loreVersion: manifest.loreVersion, supportedGhcVersions };
  }
  // Keep manifest asset validation centralized in the manifest module.
  findManifestAsset(manifest, probe.ghcVersion, target);
  const destination = binaryPath(options.cacheRoot ?? defaultCacheRoot(), manifest, asset, target);
  if (await exists(destination)) {
    if (await validateLoreCommandForProject({ command: destination, expectedLoreVersion: manifest.loreVersion, expectedGhcVersion: probe.ghcVersion, expectedTarget: target, run: options.run, env: options.env }).then(() => true, () => false)) {
      return { kind: "ready", path: destination, provider: probe.provider, ghcVersion: probe.ghcVersion, target };
    }
    await fs.rm(destination, { force: true });
  }
  return { kind: "downloadRequired", provider: probe.provider, ghcVersion: probe.ghcVersion, target, manifest, asset, destination };
}

export async function installManagedLoreBinary(plan: Extract<ManagedBinaryPlan, { kind: "downloadRequired" }>, options: BinaryManagerOptions): Promise<string> {
  await downloadInstallValidate({ ...options, manifest: plan.manifest, target: plan.target, asset: plan.asset, finalPath: plan.destination, ghcVersion: plan.ghcVersion });
  return plan.destination;
}

export async function validateLoreCommandForProject(options: { command: string; cwd?: string; expectedLoreVersion: string; expectedGhcVersion: string; expectedTarget: BinaryTarget; run?: CommandRunner; env?: NodeJS.ProcessEnv }): Promise<void> {
  await requireValidLoreCommand(options.command, options.expectedLoreVersion, options.expectedGhcVersion, options.expectedTarget, options.run, options.env, options.cwd);
}

export function defaultCacheRoot(): string {
  return join(process.env.XDG_CACHE_HOME || join(homedir(), ".cache"), "pi-lore");
}

export function binaryPath(cacheRoot: string, manifest: BinaryManifest, asset: BinaryAsset, target: BinaryTarget): string {
  return join(cacheRoot, "binaries", manifest.loreVersion, targetString(target), `ghc-${asset.ghcVersion}`, "lore-mcp");
}

type InstallOptions = BinaryManagerOptions & {
  manifest: BinaryManifest;
  target: BinaryTarget;
  asset: BinaryAsset;
  finalPath: string;
  ghcVersion: string;
};

async function downloadInstallValidate(options: InstallOptions): Promise<void> {
  const dir = dirname(options.finalPath);
  await fs.mkdir(dir, { recursive: true });
  const suffix = `${process.pid}-${randomBytes(8).toString("hex")}`;
  const downloadPath = join(dir, `lore-mcp.download-${suffix}`);
  const installPath = join(dir, `lore-mcp.install-${suffix}`);
  try {
    await (options.download ?? downloadFile)(assetDownloadUrl(options.manifest, options.asset), downloadPath, options.downloadTimeoutMs ?? Math.max(options.timeoutMs ?? 30_000, 120_000));
    const actual = await sha256File(downloadPath);
    if (actual.toLowerCase() !== options.asset.sha256.toLowerCase()) {
      throw new Error(["Downloaded Lore asset failed SHA-256 verification.", `Expected: ${options.asset.sha256}`, `Actual: ${actual}`, `Asset: ${options.asset.fileName}`].join("\n"));
    }
    await pipeline(createReadStream(downloadPath), createGunzip(), createWriteStream(installPath, { mode: 0o755 }));
    await fs.chmod(installPath, 0o755);
    await requireValidLoreCommand(installPath, options.manifest.loreVersion, options.ghcVersion, options.target, options.run, options.env);
    try {
      await fs.link(installPath, options.finalPath);
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error;
    } finally {
      await fs.rm(installPath, { force: true });
    }
    await requireValidLoreCommand(options.finalPath, options.manifest.loreVersion, options.ghcVersion, options.target, options.run, options.env);
  } finally {
    await fs.rm(downloadPath, { force: true }).catch(() => undefined);
    await fs.rm(installPath, { force: true }).catch(() => undefined);
  }
}

async function downloadFile(url: string, destination: string, timeoutMs = 120_000): Promise<void> {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeoutMs);
  try {
    const response = await fetch(url, { signal: controller.signal });
    if (!response.ok || !response.body) throw new Error(`Failed to download Lore binary: HTTP ${response.status} ${response.statusText}`);
    await pipeline(response.body, createWriteStream(destination));
  } catch (error) {
    if (controller.signal.aborted) throw new Error(`Failed to download Lore binary: timed out after ${timeoutMs}ms`);
    throw error;
  } finally {
    clearTimeout(timer);
  }
}

async function requireValidLoreCommand(command: string, loreVersion: string, ghcVersion: string, target: BinaryTarget, run?: CommandRunner, env?: NodeJS.ProcessEnv, cwd = dirname(command)): Promise<void> {
  const runner = run ?? runCommand;
  let result;
  try {
    result = await runner(command, ["--version-json"], { cwd, timeoutMs: 10_000, env });
  } catch (error) {
    throw new Error(`Could not run Lore command \`${command}\`: ${error instanceof Error ? error.message : String(error)}`);
  }
  if (result.exitCode !== 0) throw new Error(`Lore command metadata validation failed with exit code ${result.exitCode}:\n${result.stderr}\n${result.stdout}`);
  let parsed: unknown;
  try { parsed = JSON.parse(result.stdout); } catch (error) { throw new Error(`Lore command emitted invalid --version-json output: ${error instanceof Error ? error.message : String(error)}`); }
  const obj = parsed && typeof parsed === "object" ? parsed as Record<string, unknown> : {};
  const expectedTarget = targetString(target);
  if (obj.loreVersion !== loreVersion) throw new Error(`Lore binary version mismatch. Expected ${loreVersion}, got ${String(obj.loreVersion)}`);
  if (obj.ghcVersion !== ghcVersion) throw new Error(`Lore binary GHC version mismatch. Expected ${ghcVersion}, got ${String(obj.ghcVersion)}`);
  if (obj.target !== expectedTarget) throw new Error(`Lore binary target mismatch. Expected ${expectedTarget}, got ${String(obj.target)}`);
}

async function sha256File(path: string): Promise<string> {
  const hash = createHash("sha256");
  const stream = createReadStream(path);
  for await (const chunk of stream) hash.update(chunk as Buffer);
  return hash.digest("hex");
}

async function exists(path: string): Promise<boolean> {
  try { await fs.access(path); return true; } catch { return false; }
}
