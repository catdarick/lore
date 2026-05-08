#!/usr/bin/env bash
set -euo pipefail

stack bench lore:lore-memory-bench --ba "+RTS -T"
