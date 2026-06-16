import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdtemp, readdir, readFile, writeFile, chmod, mkdir } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { gzipSync } from "node:zlib";
import { test } from "node:test";
import { binaryPath, installManagedLoreBinary, planManagedLoreBinary, type BinaryManagerOptions } from "../src/binary-manager.ts";
import type { BinaryManifest } from "../src/binary-manifest.ts";
import type { CommandRunner } from "../src/subprocess.ts";

const target = { platform: "linux", arch: "x64", libc: "gnu" } as const;
function manifestFor(bytes: Buffer): BinaryManifest { return { schemaVersion: 1, loreVersion: "0.1.0.0", assets: [{ ...target, ghcVersion: "9.6.5", fileName: "asset.gz", sha256: createHash("sha256").update(bytes).digest("hex") }] }; }
function script(meta = { loreVersion: "0.1.0.0", ghcVersion: "9.6.5", target: "linux-x64-gnu" }) { return Buffer.from(`#!/bin/sh\nprintf '%s\\n' '${JSON.stringify(meta)}'\n`); }
async function makeCache() { return mkdtemp(join(tmpdir(), "pi-lore-bin-")); }

async function installPlanned(options: BinaryManagerOptions): Promise<string> {
  const plan = await planManagedLoreBinary(options);
  assert.equal(plan.kind, "downloadRequired");
  if (plan.kind !== "downloadRequired") throw new Error(`Expected downloadRequired, got ${plan.kind}`);
  return installManagedLoreBinary(plan, options);
}
async function runner(): Promise<CommandRunner> { return async (command, args, options) => {
  const { spawn } = await import("node:child_process");
  return await new Promise((resolve, reject) => { let stdout="", stderr=""; const c=spawn(command,args,{cwd:options.cwd,stdio:["ignore","pipe","pipe"]}); c.stdout.on("data", b=>stdout+=b); c.stderr.on("data", b=>stderr+=b); c.on("error",reject); c.on("exit",(exitCode,signal)=>resolve({command,args,cwd:options.cwd,exitCode,signal,stdout,stderr})); });
}; }

test("existing valid cached binary is reused without downloading", async () => {
  const gz = gzipSync(script()); const manifest = manifestFor(gz); const cacheRoot = await makeCache(); const final = binaryPath(cacheRoot, manifest, manifest.assets[0], target);
  await mkdir(join(final, ".."), { recursive: true }).catch(()=>{});
  await mkdir(final.split("/lore-mcp")[0], { recursive: true }); await writeFile(final, script()); await chmod(final, 0o755);
  const plan = await planManagedLoreBinary({ projectDir: process.cwd(), cacheRoot, manifest, target, run: await runner(), probe: async () => ({ provider: "stack", ghcVersion: "9.6.5" }), download: async () => { throw new Error("network"); } });
  assert.equal(plan.kind, "ready");
  if (plan.kind !== "ready") throw new Error(`Expected ready, got ${plan.kind}`);
  assert.equal(plan.path, final);
});

test("missing binary is downloaded and installed", async () => {
  const gz = gzipSync(script()); const manifest = manifestFor(gz); const cacheRoot = await makeCache(); let downloads = 0;
  const path = await installPlanned({ projectDir: process.cwd(), cacheRoot, manifest, target, run: await runner(), probe: async () => ({ provider: "stack", ghcVersion: "9.6.5" }), download: async (_url, dest) => { downloads++; await writeFile(dest, gz); } });
  assert.equal(downloads, 1); assert.match(await readFile(path, "utf8"), /ghcVersion/);
});

test("checksum mismatch fails and leaves no final binary", async () => {
  const manifest = manifestFor(gzipSync(script())); const cacheRoot = await makeCache();
  await assert.rejects(installPlanned({ projectDir: process.cwd(), cacheRoot, manifest, target, run: await runner(), probe: async () => ({ provider: "stack", ghcVersion: "9.6.5" }), download: async (_u,d) => writeFile(d, Buffer.from("bad")) }), /SHA-256/);
  const final = binaryPath(cacheRoot, manifest, manifest.assets[0], target); await assert.rejects(() => readFile(final));
});

test("wrong binary metadata causes redownload once", async () => {
  const gz = gzipSync(script()); const manifest = manifestFor(gz); const cacheRoot = await makeCache(); const final = binaryPath(cacheRoot, manifest, manifest.assets[0], target);
  await mkdir(final.split("/lore-mcp")[0], { recursive: true }); await writeFile(final, script({ loreVersion: "0.1.0.0", ghcVersion: "9.8.4", target: "linux-x64-gnu" })); await chmod(final, 0o755);
  let downloads = 0; await installPlanned({ projectDir: process.cwd(), cacheRoot, manifest, target, run: await runner(), probe: async () => ({ provider: "stack", ghcVersion: "9.6.5" }), download: async (_u,d) => { downloads++; await writeFile(d, gz); } });
  assert.equal(downloads, 1);
});

test("temporary files are removed after failure", async () => {
  const manifest = manifestFor(gzipSync(script())); const cacheRoot = await makeCache();
  await assert.rejects(installPlanned({ projectDir: process.cwd(), cacheRoot, manifest, target, run: await runner(), probe: async () => ({ provider: "stack", ghcVersion: "9.6.5" }), download: async (_u,d) => writeFile(d, Buffer.from("bad")) }));
  const dir = join(cacheRoot, "binaries", manifest.loreVersion, "linux-x64-gnu", "ghc-9.6.5");
  assert.deepEqual((await readdir(dir).catch(() => [])).filter(n => n.includes("download") || n.includes("install")), []);
});
