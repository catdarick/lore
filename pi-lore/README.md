# pi-lore

`pi-lore` starts Lore automatically in managed mode when no explicit `command` is configured.

## Managed Lore binary download

On startup the extension detects the project provider using the same precedence as Lore:

1. `stack.yaml` -> Stack
2. `cabal.project` -> Cabal
3. `package.yaml` -> Cabal
4. exactly one root `*.cabal` file -> Cabal

It then probes the exact project GHC version with Stack or Cabal, selects a matching Linux x64 GNU `lore-mcp` asset from the bundled `binaries.json`, verifies the SHA-256 checksum, validates `lore-mcp --version-json`, and caches the executable globally.

Lore links against the GHC API, so the full GHC version must match exactly. For example, a project using `9.6.5` will not use a `9.6.7` binary.

Supported managed target in this first version:

- `linux-x64-gnu`

Supported GHC versions are the entries listed in `binaries.json` for that target.

The cache is shared across projects:

- `$XDG_CACHE_HOME/pi-lore` when `XDG_CACHE_HOME` is set
- otherwise `~/.cache/pi-lore`

Reload Pi after changing a project resolver/compiler so the extension can select the corresponding binary.

## Manual command override

Set `command` to bypass managed probing and downloading entirely:

```json
{
  "command": "/custom/build/lore-mcp",
  "args": []
}
```

This is the right path for local development, custom builds, or unsupported platforms/GHC versions. Managed mode never falls back to `lore-mcp` on `PATH` after resolution fails.
