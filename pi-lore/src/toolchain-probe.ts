import { promises as fs } from "node:fs";
import { join } from "node:path";
import { formatCommand, runCommand, tail, type CommandRunner } from "./subprocess.ts";

export type ProjectProvider = "stack" | "cabal";

export type ProbeResult = {
  provider: ProjectProvider;
  ghcVersion: string;
};

export type ProbeOptions = {
  projectDir: string;
  run?: CommandRunner;
  env?: NodeJS.ProcessEnv;
  timeoutMs?: number;
};

export async function detectProjectProvider(projectDir: string): Promise<ProjectProvider> {
  // Keep deliberately identical to Lore.Internal.ProjectProvider.detectProjectProvider.
  if (await exists(join(projectDir, "stack.yaml"))) return "stack";
  if (await exists(join(projectDir, "cabal.project"))) return "cabal";
  if (await exists(join(projectDir, "package.yaml"))) return "cabal";
  const entries = await fs.readdir(projectDir);
  const cabalFiles = entries.filter((entry) => entry.endsWith(".cabal"));
  if (cabalFiles.length === 1) return "cabal";
  if (cabalFiles.length > 1) {
    throw new Error("Multiple root-level *.cabal files were found without a cabal.project file. Please add cabal.project to define package selection explicitly.");
  }
  throw new Error("No supported project files were found. Expected one of: stack.yaml, cabal.project, package.yaml, or a single *.cabal file at the project root.");
}

export async function probeProjectGhcVersion(options: ProbeOptions): Promise<ProbeResult> {
  const provider = await detectProjectProvider(options.projectDir);
  const command = provider === "stack" ? "stack" : "cabal";
  const args = provider === "stack"
    ? ["exec", "--", "ghc", "--numeric-version"]
    : ["exec", "--write-ghc-environment-files=never", "--", "ghc", "--numeric-version"];
  const runner = options.run ?? runCommand;
  let result;
  try {
    result = await runner(command, args, { cwd: options.projectDir, env: options.env, timeoutMs: options.timeoutMs });
  } catch (error) {
    throw new Error([
      "Failed to detect the project GHC version.",
      "",
      `Detected provider: ${provider}`,
      `Command: ${formatCommand(command, args)}`,
      `Working directory: ${options.projectDir}`,
      `Error: ${error instanceof Error ? error.message : String(error)}`,
    ].join("\n"));
  }
  if (result.exitCode !== 0) {
    const reason = result.exitCode === null ? `terminated by signal ${result.signal ?? "unknown"}` : `exit code ${result.exitCode}`;
    throw probeError(provider, command, args, options.projectDir, reason, result.stdout, result.stderr);
  }
  const ghcVersion = parseGhcVersionOutput(result.stdout);
  if (!ghcVersion.ok) {
    throw probeError(provider, command, args, options.projectDir, ghcVersion.error, result.stdout, result.stderr);
  }
  return { provider, ghcVersion: ghcVersion.version };
}

export function parseGhcVersionOutput(stdout: string): { ok: true; version: string } | { ok: false; error: string } {
  const versions = new Set<string>();
  for (const line of stdout.split(/\r?\n/)) {
    const match = /^\s*(\d+\.\d+(?:\.\d+)*)\s*$/.exec(line);
    if (match) versions.add(match[1]);
  }
  if (versions.size === 0) return { ok: false, error: "no standalone GHC version line found in stdout" };
  if (versions.size > 1) return { ok: false, error: `multiple distinct GHC versions found in stdout: ${[...versions].join(", ")}` };
  return { ok: true, version: [...versions][0] };
}

function probeError(provider: ProjectProvider, command: string, args: string[], cwd: string, reason: string, stdout: string, stderr: string): Error {
  return new Error([
    "Failed to detect the project GHC version.",
    "",
    `Detected provider: ${provider}`,
    `Command: ${formatCommand(command, args)}`,
    `Working directory: ${cwd}`,
    `Error: ${reason}`,
    `stderr:\n${tail(stderr)}`,
    `stdout:\n${tail(stdout)}`,
  ].join("\n"));
}

async function exists(path: string): Promise<boolean> {
  try { await fs.access(path); return true; } catch { return false; }
}
