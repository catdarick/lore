import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

export const LORE_BINARY_REPOSITORY = { owner: "catdarick", name: "lore" } as const;

export type BinaryTarget = { platform: "linux"; arch: "x64"; libc: "gnu" };
export type RuntimeBinaryTarget = { platform: string; arch: string; glibcVersionRuntime?: string };
export type BinaryAsset = BinaryTarget & { ghcVersion: string; fileName: string; sha256: string };
export type BinaryManifest = { schemaVersion: 1; loreVersion: string; assets: BinaryAsset[] };

export function loadBundledBinaryManifest(): BinaryManifest {
  const here = dirname(fileURLToPath(import.meta.url));
  return validateBinaryManifest(JSON.parse(readFileSync(join(here, "..", "binaries.json"), "utf8")));
}

export function validateBinaryManifest(raw: unknown): BinaryManifest {
  if (!raw || typeof raw !== "object") throw new Error("Invalid Lore binary manifest: expected object");
  const obj = raw as Record<string, unknown>;
  if (obj.schemaVersion !== 1) throw new Error("Invalid Lore binary manifest: schemaVersion must be 1");
  if (typeof obj.loreVersion !== "string" || obj.loreVersion.length === 0) throw new Error("Invalid Lore binary manifest: loreVersion must be a non-empty string");
  if (!Array.isArray(obj.assets)) throw new Error("Invalid Lore binary manifest: assets must be an array");
  const seen = new Set<string>();
  const assets = obj.assets.map((asset, index) => validateAsset(asset, index));
  for (const asset of assets) {
    const key = assetKey(obj.loreVersion, asset);
    if (seen.has(key)) throw new Error(`Invalid Lore binary manifest: duplicate asset identity ${key}`);
    seen.add(key);
  }
  return { schemaVersion: 1, loreVersion: obj.loreVersion, assets };
}

export function currentBinaryTarget(runtime: RuntimeBinaryTarget = currentRuntimeBinaryTarget()): BinaryTarget {
  if (runtime.platform === "linux" && runtime.arch === "x64") {
    if (runtime.glibcVersionRuntime) return { platform: "linux", arch: "x64", libc: "gnu" };
    throw new Error("No managed Lore binary is available for linux-x64 without glibc.\nConfigure an explicit `command` to use a custom Lore binary.");
  }
  throw new Error(`No managed Lore binary is available for ${runtime.platform}-${runtime.arch}.\nConfigure an explicit \`command\` to use a custom Lore binary.`);
}

function currentRuntimeBinaryTarget(): RuntimeBinaryTarget {
  const report = process.report.getReport();
  const glibcVersionRuntime = typeof report.header.glibcVersionRuntime === "string" ? report.header.glibcVersionRuntime : undefined;
  return { platform: process.platform, arch: process.arch, ...(glibcVersionRuntime ? { glibcVersionRuntime } : {}) };
}

export function targetString(target: BinaryTarget): string {
  return `${target.platform}-${target.arch}-${target.libc}`;
}

export function findManifestAsset(manifest: BinaryManifest, ghcVersion: string, target: BinaryTarget): BinaryAsset {
  const asset = manifest.assets.find((candidate) =>
    candidate.ghcVersion === ghcVersion && candidate.platform === target.platform && candidate.arch === target.arch && candidate.libc === target.libc,
  );
  if (asset) return asset;
  const supported = manifest.assets
    .filter((candidate) => candidate.platform === target.platform && candidate.arch === target.arch && candidate.libc === target.libc)
    .map((candidate) => candidate.ghcVersion)
    .filter((value, index, values) => values.indexOf(value) === index)
    .sort();
  throw new Error([
    `Lore does not provide a managed binary for GHC ${ghcVersion} on ${targetString(target)}.`,
    "",
    "Supported GHC versions for this target:",
    ...(supported.length === 0 ? ["- none"] : supported.map((version) => `- ${version}`)),
    "",
    "Configure an explicit `command` to use a custom binary.",
  ].join("\n"));
}

export function assetDownloadUrl(manifest: BinaryManifest, asset: BinaryAsset): string {
  return `https://github.com/${LORE_BINARY_REPOSITORY.owner}/${LORE_BINARY_REPOSITORY.name}/releases/download/v${manifest.loreVersion}/${asset.fileName}`;
}

function validateAsset(raw: unknown, index: number): BinaryAsset {
  if (!raw || typeof raw !== "object") throw new Error(`Invalid Lore binary manifest: assets[${index}] must be an object`);
  const obj = raw as Record<string, unknown>;
  for (const field of ["ghcVersion", "platform", "arch", "libc", "fileName", "sha256"]) {
    if (typeof obj[field] !== "string" || (obj[field] as string).length === 0) throw new Error(`Invalid Lore binary manifest: assets[${index}].${field} must be a non-empty string`);
  }
  if (obj.platform !== "linux" || obj.arch !== "x64" || obj.libc !== "gnu") throw new Error(`Invalid Lore binary manifest: unsupported target in assets[${index}]`);
  if (!/^[0-9a-f]{64}$/i.test(obj.sha256 as string)) throw new Error(`Invalid Lore binary manifest: assets[${index}].sha256 must be a SHA-256 hex digest`);
  if (/^0{64}$/.test(obj.sha256 as string)) throw new Error(`Invalid Lore binary manifest: assets[${index}].sha256 must not be the zero placeholder digest`);
  return obj as BinaryAsset;
}

function assetKey(loreVersion: string, asset: BinaryAsset): string {
  return `${loreVersion}|${asset.ghcVersion}|${asset.platform}|${asset.arch}|${asset.libc}`;
}
