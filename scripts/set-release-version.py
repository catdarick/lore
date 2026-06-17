#!/usr/bin/env python3
"""Set the release version in every release metadata file."""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
LORE_VERSION_RE = re.compile(r"^\d+\.\d+\.\d+\.0$")
ANY_LORE_VERSION = r"\d+\.\d+\.\d+\.\d+"
ANY_NPM_VERSION = r"\d+\.\d+\.\d+"


@dataclass(frozen=True)
class VersionField:
    path: Path
    pattern: re.Pattern[str]
    expected: str
    target: str


def prepare_change(field: VersionField) -> tuple[Path, str, str]:
    original = field.path.read_text()
    matches = list(field.pattern.finditer(original))
    relative_path = field.path.relative_to(ROOT)

    if len(matches) != 1:
        raise RuntimeError(
            f"expected exactly one release-version field in {relative_path}, "
            f"found {len(matches)}"
        )

    current = matches[0].group("version")
    if current != field.expected:
        raise RuntimeError(
            f"inconsistent release version in {relative_path}: "
            f"expected {field.expected}, found {current}"
        )

    updated = field.pattern.sub(
        lambda match: f'{match.group("prefix")}{field.target}{match.group("suffix")}',
        original,
        count=1,
    )
    return field.path, original, updated


def main() -> int:
    if len(sys.argv) != 2 or not LORE_VERSION_RE.fullmatch(sys.argv[1]):
        print(
            "usage: set-release-version.py <major.minor.patch.0>\n"
            "example: set-release-version.py 0.2.0.0",
            file=sys.stderr,
        )
        return 2

    target_lore_version = sys.argv[1]
    target_npm_version = target_lore_version.removesuffix(".0")

    package_yaml = ROOT / "lore-mcp/package.yaml"
    package_text = package_yaml.read_text()
    package_match = re.search(
        rf"(?m)^version:\s+(?P<version>{ANY_LORE_VERSION})\s*$", package_text
    )
    if package_match is None:
        raise RuntimeError("could not find lore-mcp version in lore-mcp/package.yaml")

    current_lore_version = package_match.group("version")
    if not LORE_VERSION_RE.fullmatch(current_lore_version):
        raise RuntimeError(
            "current lore-mcp version must have the form major.minor.patch.0, "
            f"found {current_lore_version}"
        )
    current_npm_version = current_lore_version.removesuffix(".0")

    fields = [
        VersionField(
            package_yaml,
            re.compile(
                rf"(?m)^(?P<prefix>version:\s+)(?P<version>{ANY_LORE_VERSION})(?P<suffix>\s*)$"
            ),
            current_lore_version,
            target_lore_version,
        ),
        VersionField(
            ROOT / "lore-mcp/lore-mcp.cabal",
            re.compile(
                rf"(?m)^(?P<prefix>version:\s+)(?P<version>{ANY_LORE_VERSION})(?P<suffix>\s*)$"
            ),
            current_lore_version,
            target_lore_version,
        ),
        VersionField(
            ROOT / "lore-mcp/src/Lore/Mcp/Version.hs",
            re.compile(
                rf'(?m)^(?P<prefix>loreVersionText\s*=\s*")(?P<version>{ANY_LORE_VERSION})(?P<suffix>"\s*)$'
            ),
            current_lore_version,
            target_lore_version,
        ),
        VersionField(
            ROOT / "pi-lore/package.json",
            re.compile(
                rf'(?m)^(?P<prefix>  "version": ")(?P<version>{ANY_NPM_VERSION})(?P<suffix>",)$'
            ),
            current_npm_version,
            target_npm_version,
        ),
        VersionField(
            ROOT / "pi-lore/src/mcp-client.ts",
            re.compile(
                rf'(?m)^(?P<prefix>\s*clientInfo: \{{ name: "pi-lore-extension", version: ")(?P<version>{ANY_NPM_VERSION})(?P<suffix>" \}},)$'
            ),
            current_npm_version,
            target_npm_version,
        ),
        VersionField(
            ROOT / "pi-lore/binaries.json",
            re.compile(
                rf'(?m)^(?P<prefix>  "loreVersion": ")(?P<version>{ANY_LORE_VERSION})(?P<suffix>",)$'
            ),
            current_lore_version,
            target_lore_version,
        ),
    ]

    changes = [prepare_change(field) for field in fields]
    if target_lore_version == current_lore_version:
        print(f"release version is already {target_lore_version}")
        return 0

    written: list[tuple[Path, str]] = []
    try:
        for path, original, updated in changes:
            path.write_text(updated)
            written.append((path, original))
    except OSError:
        for path, original in reversed(written):
            path.write_text(original)
        raise

    print(f"updated release version: {current_lore_version} -> {target_lore_version}")
    print(f"npm package version:      {current_npm_version} -> {target_npm_version}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1)
