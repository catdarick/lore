import { readFileSync } from "node:fs";
import { loadBundledBinaryManifest } from "../src/binary-manifest.ts";

const packageJson = JSON.parse(readFileSync(new URL("../package.json", import.meta.url), "utf8")) as { version?: unknown };
if (typeof packageJson.version !== "string" || packageJson.version.length === 0) {
  throw new Error("pi-lore package version must be a non-empty string");
}

const manifest = loadBundledBinaryManifest();
const expectedLoreVersion = `${packageJson.version}.0`;
if (manifest.loreVersion !== expectedLoreVersion) {
  throw new Error(`pi-lore ${packageJson.version} must bundle Lore ${expectedLoreVersion}, got ${manifest.loreVersion}`);
}
if (manifest.assets.length === 0) {
  throw new Error("pi-lore cannot be published with an empty managed binary manifest");
}
