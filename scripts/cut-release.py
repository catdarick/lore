#!/usr/bin/env python3
"""Cut a Lore release commit and tag."""

from __future__ import annotations

import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
VERSION_RE = re.compile(r"^\d+\.\d+\.\d+\.0$")
RELEASE_FILES = [
    "lore-mcp/package.yaml",
    "lore-mcp/lore-mcp.cabal",
    "lore-mcp/src/Lore/Mcp/Version.hs",
    "pi-lore/package.json",
    "pi-lore/src/mcp-client.ts",
    "pi-lore/binaries.json",
]


def git(*args: str, capture: bool = False) -> str:
    result = subprocess.run(
        ["git", *args],
        cwd=ROOT,
        check=True,
        text=True,
        stdout=subprocess.PIPE if capture else None,
        stderr=None,
    )
    return result.stdout.strip() if capture else ""


def run(*args: str) -> None:
    subprocess.run(args, cwd=ROOT, check=True)


def require_branch() -> str:
    branch = git("branch", "--show-current", capture=True)
    if not branch:
        raise RuntimeError("cannot cut a release from detached HEAD")
    return branch


def require_tag_absent(tag: str) -> None:
    result = subprocess.run(
        ["git", "rev-parse", "--verify", "--quiet", f"refs/tags/{tag}"],
        cwd=ROOT,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    if result.returncode == 0:
        raise RuntimeError(f"tag already exists: {tag}")


def changed_tracked_files() -> set[str]:
    unstaged = git("diff", "--name-only", capture=True)
    staged = git("diff", "--cached", "--name-only", capture=True)
    return {line for line in (unstaged + "\n" + staged).splitlines() if line}


def untracked_files() -> set[str]:
    output = git("ls-files", "--others", "--exclude-standard", capture=True)
    return {line for line in output.splitlines() if line}


def require_only_release_files_changed() -> None:
    release_file_set = set(RELEASE_FILES)
    unexpected_tracked = sorted(changed_tracked_files() - release_file_set)
    unexpected_untracked = sorted(untracked_files())
    if unexpected_tracked or unexpected_untracked:
        details = []
        details.extend(f"modified outside release set: {path}" for path in unexpected_tracked)
        details.extend(f"untracked file: {path}" for path in unexpected_untracked)
        raise RuntimeError(
            "release version bump changed unexpected files:\n" + "\n".join(details)
        )

def main() -> int:
    if len(sys.argv) not in (2, 3) or not VERSION_RE.fullmatch(sys.argv[1]):
        print(
            "usage: cut-release.py <major.minor.patch.0> [remote]\n"
            "example: cut-release.py 1.0.4.0 origin",
            file=sys.stderr,
        )
        return 2

    version = sys.argv[1]
    remote = sys.argv[2] if len(sys.argv) == 3 else "origin"
    tag = f"v{version}"

    branch = require_branch()
    require_tag_absent(tag)
    git("remote", "get-url", remote, capture=True)
    require_only_release_files_changed()

    run(sys.executable, str(ROOT / "scripts/set-release-version.py"), version)
    require_only_release_files_changed()

    files_to_commit = changed_tracked_files() & set(RELEASE_FILES)
    if not files_to_commit:
        raise RuntimeError(
            f"release version is already {version}; refusing to create an empty release commit"
        )

    git("diff", "--check")
    git("add", "--", *RELEASE_FILES)
    git("commit", "-m", f"Release {tag}")
    git("tag", "-a", tag, "-m", f"Release {tag}")
    git("push", remote, f"HEAD:{branch}")
    git("push", remote, tag)

    print(f"cut release {tag} from branch {branch} and pushed to {remote}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, subprocess.CalledProcessError) as error:
        print(f"error: {error}", file=sys.stderr)
        raise SystemExit(1)
