#!/usr/bin/env bash
set -euo pipefail

MEMORY_MODE=e2e stack bench lore:lore-memory-bench --ba "+RTS -T -s"
