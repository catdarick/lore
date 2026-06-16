import assert from "node:assert/strict";
import { test } from "node:test";
import { currentBinaryTarget, findManifestAsset, validateBinaryManifest, type BinaryManifest } from "../src/binary-manifest.ts";
const target = { platform: "linux", arch: "x64", libc: "gnu" } as const;
const manifest: BinaryManifest = { schemaVersion: 1, loreVersion: "0.1.0.0", assets: [
  { ...target, ghcVersion: "9.6.5", fileName: "a.gz", sha256: "a".repeat(64) },
  { ...target, ghcVersion: "9.8.4", fileName: "b.gz", sha256: "b".repeat(64) },
] };

test("exact manifest match succeeds and patch mismatch fails", () => {
  assert.equal(findManifestAsset(manifest, "9.6.5", target).fileName, "a.gz");
  assert.throws(() => findManifestAsset(manifest, "9.6.6", target), /9\.6\.5[\s\S]*9\.8\.4/);
});

test("unsupported architecture has filtered supported output", () => {
  assert.throws(() => findManifestAsset(manifest, "9.6.5", { platform: "linux", arch: "arm64", libc: "gnu" } as any), /none/);
});

test("manifest validation rejects duplicates and bad sha", () => {
  assert.throws(() => validateBinaryManifest({ ...manifest, assets: [manifest.assets[0], manifest.assets[0]] }), /duplicate/);
  assert.throws(() => validateBinaryManifest({ ...manifest, assets: [{ ...manifest.assets[0], sha256: "nope" }] }), /SHA-256/);
  assert.throws(() => validateBinaryManifest({ ...manifest, assets: [{ ...manifest.assets[0], sha256: "0".repeat(64) }] }), /zero placeholder/);
});


test("managed target detection requires glibc on Linux x64", () => {
  assert.deepEqual(currentBinaryTarget({ platform: "linux", arch: "x64", glibcVersionRuntime: "2.35" }), target);
  assert.throws(() => currentBinaryTarget({ platform: "linux", arch: "x64" }), /without glibc/);
});
