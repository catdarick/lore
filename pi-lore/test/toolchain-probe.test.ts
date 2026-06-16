import assert from "node:assert/strict";
import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";
import { detectProjectProvider, parseGhcVersionOutput, probeProjectGhcVersion } from "../src/toolchain-probe.ts";
import type { CommandRunner } from "../src/subprocess.ts";

async function dir(files: string[]) { const d = await mkdtemp(join(tmpdir(), "pi-lore-probe-")); await Promise.all(files.map(f => writeFile(join(d, f), ""))); return d; }
const ok: CommandRunner = async (command, args, options) => ({ command, args, cwd: options.cwd, exitCode: 0, signal: null, stdout: "9.6.5\n", stderr: "" });

test("provider precedence matches Lore", async () => {
  assert.equal(await detectProjectProvider(await dir(["stack.yaml", "cabal.project"])), "stack");
  assert.equal(await detectProjectProvider(await dir(["cabal.project"])), "cabal");
  assert.equal(await detectProjectProvider(await dir(["package.yaml"])), "cabal");
  assert.equal(await detectProjectProvider(await dir(["demo.cabal"])), "cabal");
  await assert.rejects(detectProjectProvider(await dir(["a.cabal", "b.cabal"])), /Multiple root-level/);
  await assert.rejects(detectProjectProvider(await dir([])), /No supported project files/);
});

test("GHC probe commands are exact", async () => {
  const seen: string[] = [];
  const run: CommandRunner = async (command, args, options) => { seen.push([command, ...args].join(" ")); return ok(command, args, options); };
  await probeProjectGhcVersion({ projectDir: await dir(["stack.yaml"]), run });
  await probeProjectGhcVersion({ projectDir: await dir(["cabal.project"]), run });
  assert.deepEqual(seen, [
    "stack exec -- ghc --numeric-version",
    "cabal exec --write-ghc-environment-files=never -- ghc --numeric-version",
  ]);
});


test("GHC probe passes configured environment", async () => {
  let pathValue: string | undefined;
  const run: CommandRunner = async (command, args, options) => { pathValue = options.env?.PATH; return ok(command, args, options); };
  await probeProjectGhcVersion({ projectDir: await dir(["stack.yaml"]), run, env: { PATH: "/tmp/stack" } });
  assert.equal(pathValue, "/tmp/stack");
});

test("parses only standalone version lines", () => {
  assert.deepEqual(parseGhcVersionOutput("9.6.5\n"), { ok: true, version: "9.6.5" });
  assert.deepEqual(parseGhcVersionOutput("warning for 9.8.4\n 9.6.5 \n"), { ok: true, version: "9.6.5" });
  assert.equal(parseGhcVersionOutput("warning mentions 9.6.5 only").ok, false);
  assert.equal(parseGhcVersionOutput("9.6.5\n9.8.4\n").ok, false);
});


test("timed out probe reports signal instead of null exit code", async () => {
  const run: CommandRunner = async (command, args, options) => ({ command, args, cwd: options.cwd, exitCode: null, signal: "SIGKILL", stdout: "", stderr: "Command timed out after 5ms" });
  await assert.rejects(probeProjectGhcVersion({ projectDir: await dir(["stack.yaml"]), run, timeoutMs: 5 }), /terminated by signal SIGKILL[\s\S]*timed out/);
});

test("non-zero exit includes stderr", async () => {
  const run: CommandRunner = async (command, args, options) => ({ command, args, cwd: options.cwd, exitCode: 1, signal: null, stdout: "", stderr: "missing" });
  await assert.rejects(probeProjectGhcVersion({ projectDir: await dir(["stack.yaml"]), run }), /Detected provider: stack[\s\S]*missing/);
});
