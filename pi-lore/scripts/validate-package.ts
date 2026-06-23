import { readFileSync } from "node:fs";
import { loadBundledBinaryManifest } from "../src/binary-manifest.ts";

const packageJson = JSON.parse(readFileSync(new URL("../package.json", import.meta.url), "utf8")) as { version?: unknown };
if (typeof packageJson.version !== "string" || packageJson.version.length === 0) {
  throw new Error("pi-lore package version must be a non-empty string");
}

const readme = readFileSync(new URL("../README.md", import.meta.url), "utf8");
const parentRelativeReadmeLinks = [...readme.matchAll(/\[[^\]]+\]\(\.\.\/[^)\s]+(?:\s+"[^"]*")?\)/g)].map((match) => match[0]);
if (parentRelativeReadmeLinks.length > 0) {
  throw new Error(
    [
      "pi-lore README must not use parent-relative links because npm renders them under npmjs.com, not the GitHub repository.",
      "Use canonical GitHub URLs for docs outside pi-lore:",
      ...parentRelativeReadmeLinks.map((link) => `- ${link}`),
    ].join("\n"),
  );
}

const manifest = loadBundledBinaryManifest();
const expectedLoreVersion = `${packageJson.version}.0`;
if (manifest.loreVersion !== expectedLoreVersion) {
  throw new Error(`pi-lore ${packageJson.version} must bundle Lore ${expectedLoreVersion}, got ${manifest.loreVersion}`);
}
if (manifest.assets.length === 0) {
  throw new Error("pi-lore cannot be published with an empty managed binary manifest");
}
