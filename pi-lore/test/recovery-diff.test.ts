import assert from "node:assert/strict";
import { execFile } from "node:child_process";
import { mkdtemp, readFile, rm, symlink, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { promisify } from "node:util";
import { test } from "node:test";
import { captureRecoveryBaseline, captureRecoveryDiff } from "../src/recovery-diff.ts";
import type { LoreConfig } from "../src/types.ts";

const execFileAsync = promisify(execFile);

test("large textual diffs are retained as artifacts and bounded in context", async () => {
  const projectDir = await mkdtemp(join(tmpdir(), "lore-large-diff-"));
  await execFileAsync("git", ["init"], { cwd: projectDir });
  const filePath = join(projectDir, "A.txt");
  await writeFile(filePath, "before\n", "utf8");
  const stateDir = join(projectDir, "state");
  const config: LoreConfig = {
    command: "unused",
    args: [],
    env: {},
    cwd: projectDir,
    startupTimeoutMs: 1_000,
    defaultToolTimeoutMs: 1_000,
    toolTimeoutMs: {},
    summaryTimeoutMs: 1_000,
    maxInlineDiffBytes: 100,
    allowToolOverride: false,
    stateDir,
    tools: { disabled: [] },
    recovery: { compilation: true, tests: true },
  };
  const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000020";
  await captureRecoveryBaseline(config, recoveryId);
  await writeFile(filePath, Array.from({ length: 2_000 }, (_, index) => `after ${index}`).join("\n"), "utf8");

  const diff = await captureRecoveryDiff(config, recoveryId);

  assert.equal(diff.reliable, true);
  assert.equal(diff.truncated, true);
  assert.ok(diff.patchPath);
  assert.ok((diff.inlinePatch ?? "").length <= 200);
  assert.match(await readFile(diff.patchPath!, "utf8"), /after 1999/);
  assert.equal(diff.stats.filesChanged, 1);
  assert.ok(diff.stats.additions > 0);
});

test("unchanged oversized file does not make changed text diff unreliable", async () => {
  const { config, projectDir } = await makeDiffFixture("unchanged-large");
  await writeFile(join(projectDir, "large.bin"), "x".repeat(2_000_001), "utf8");
  await writeFile(join(projectDir, "small.txt"), "before\n", "utf8");
  const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000021";
  await captureRecoveryBaseline(config, recoveryId);
  await writeFile(join(projectDir, "small.txt"), "after\n", "utf8");

  const diff = await captureRecoveryDiff(config, recoveryId);

  assert.equal(diff.reliable, true);
  assert.deepEqual(diff.changedPaths, ["small.txt"]);
});

test("changed small text remains diffable after many baseline files", async () => {
  const { config, projectDir } = await makeDiffFixture("many-small-files");
  for (let index = 0; index < 11; index += 1) {
    await writeFile(join(projectDir, `bulk-${index.toString().padStart(2, "0")}.txt`), "x".repeat(2_000_000), "utf8");
  }
  await writeFile(join(projectDir, "small.txt"), "before\n", "utf8");
  const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000025";
  await captureRecoveryBaseline(config, recoveryId);
  await writeFile(join(projectDir, "small.txt"), "after\n", "utf8");

  const diff = await captureRecoveryDiff(config, recoveryId);

  assert.equal(diff.reliable, true);
  assert.deepEqual(diff.changedPaths, ["small.txt"]);
  assert.match(diff.inlinePatch ?? "", /after/);
});

test("unchanged symlink does not make changed text diff unreliable", async () => {
  const { config, projectDir } = await makeDiffFixture("unchanged-symlink");
  await writeFile(join(projectDir, "target.txt"), "target\n", "utf8");
  await symlink("target.txt", join(projectDir, "link.txt"));
  await writeFile(join(projectDir, "small.txt"), "before\n", "utf8");
  const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000022";
  await captureRecoveryBaseline(config, recoveryId);
  await writeFile(join(projectDir, "small.txt"), "after\n", "utf8");

  const diff = await captureRecoveryDiff(config, recoveryId);

  assert.equal(diff.reliable, true);
  assert.deepEqual(diff.changedPaths, ["small.txt"]);
});

test("changed oversized file makes diff unreliable", async () => {
  const { config, projectDir } = await makeDiffFixture("changed-large");
  await writeFile(join(projectDir, "large.bin"), "x".repeat(2_000_001), "utf8");
  const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000023";
  await captureRecoveryBaseline(config, recoveryId);
  await writeFile(join(projectDir, "large.bin"), "y".repeat(2_000_001), "utf8");

  const diff = await captureRecoveryDiff(config, recoveryId);

  assert.equal(diff.reliable, false);
  assert.deepEqual(diff.changedPaths, ["large.bin"]);
  assert.match(diff.reason ?? "", /large|Non-textual/i);
});

test("changed symlink makes diff unreliable", async () => {
  const { config, projectDir } = await makeDiffFixture("changed-symlink");
  await writeFile(join(projectDir, "one.txt"), "one\n", "utf8");
  await writeFile(join(projectDir, "two.txt"), "two\n", "utf8");
  await symlink("one.txt", join(projectDir, "link.txt"));
  const recoveryId = "lore-recovery-00000000-0000-4000-8000-000000000024";
  await captureRecoveryBaseline(config, recoveryId);
  await rmLink(join(projectDir, "link.txt"));
  await symlink("two.txt", join(projectDir, "link.txt"));

  const diff = await captureRecoveryDiff(config, recoveryId);

  assert.equal(diff.reliable, false);
  assert.deepEqual(diff.changedPaths, ["link.txt"]);
  assert.match(diff.reason ?? "", /symlink/i);
});

async function makeDiffFixture(name: string): Promise<{ projectDir: string; config: LoreConfig }> {
  const projectDir = await mkdtemp(join(tmpdir(), `lore-${name}-`));
  await execFileAsync("git", ["init"], { cwd: projectDir });
  return {
    projectDir,
    config: {
      command: "unused",
      args: [],
      env: {},
      cwd: projectDir,
      startupTimeoutMs: 1_000,
      defaultToolTimeoutMs: 1_000,
      toolTimeoutMs: {},
      summaryTimeoutMs: 1_000,
      maxInlineDiffBytes: 1_000,
      allowToolOverride: false,
      stateDir: join(projectDir, "state"),
      tools: { disabled: [] },
      recovery: { compilation: true, tests: true },
    },
  };
}

async function rmLink(path: string): Promise<void> {
  await rm(path);
}
