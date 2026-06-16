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
  probe?: (projectDir: string) => Promise<ProbeResult>;
  timeoutMs?: number;
  downloadTimeoutMs?: number;
  onStatus?: (message: string) => void | Promise<void>;
};

export type Downloader = (url: string, destination: string, timeoutMs?: number) => Promise<void>;

export async function resolveManagedLoreBinary(options: BinaryManagerOptions): Promise<string> {
  await options.onStatus?.("Detecting project GHC version…");
  const probe = options.probe ? await options.probe(options.projectDir) : await probeProjectGhcVersion({ projectDir: options.projectDir, run: options.run, env: options.env, timeoutMs: options.timeoutMs });
  const manifest = options.manifest ?? loadBundledBinaryManifest();
  const target = options.target ?? currentBinaryTarget();
  const asset = findManifestAsset(manifest, probe.ghcVersion, target);
  const finalPath = binaryPath(options.cacheRoot ?? defaultCacheRoot(), manifest, asset, target);

  if (await exists(finalPath)) {
    if (await validateBinary(finalPath, manifest.loreVersion, probe.ghcVersion, target, options.run, options.env)) return finalPath;
    await fs.rm(finalPath, { force: true });
  }

  await options.onStatus?.(`Downloading Lore for GHC ${probe.ghcVersion}…`);
  await downloadInstallValidate({ ...options, manifest, target, asset, finalPath, ghcVersion: probe.ghcVersion });
  return finalPath;
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
    await requireValidBinary(installPath, options.manifest.loreVersion, options.ghcVersion, options.target, options.run, options.env);
    try {
      await fs.link(installPath, options.finalPath);
    } catch (error) {
      if ((error as NodeJS.ErrnoException).code !== "EEXIST") throw error;
    } finally {
      await fs.rm(installPath, { force: true });
    }
    await requireValidBinary(options.finalPath, options.manifest.loreVersion, options.ghcVersion, options.target, options.run, options.env);
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

async function validateBinary(path: string, loreVersion: string, ghcVersion: string, target: BinaryTarget, run?: CommandRunner, env?: NodeJS.ProcessEnv): Promise<boolean> {
  try {
    await requireValidBinary(path, loreVersion, ghcVersion, target, run, env);
    return true;
  } catch {
    return false;
  }
}

async function requireValidBinary(path: string, loreVersion: string, ghcVersion: string, target: BinaryTarget, run?: CommandRunner, env?: NodeJS.ProcessEnv): Promise<void> {
  const runner = run ?? runCommand;
  const result = await runner(path, ["--version-json"], { cwd: dirname(path), timeoutMs: 10_000, env });
  if (result.exitCode !== 0) throw new Error(`Lore binary metadata validation failed with exit code ${result.exitCode}:\n${result.stderr}\n${result.stdout}`);
  let parsed: unknown;
  try { parsed = JSON.parse(result.stdout); } catch (error) { throw new Error(`Lore binary emitted invalid --version-json output: ${error instanceof Error ? error.message : String(error)}`); }
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
