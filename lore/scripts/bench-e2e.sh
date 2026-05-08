#!/usr/bin/env bash
set -euo pipefail

BENCH_MODE=e2e stack bench lore:lore-bench --benchmark-arguments "--match prefix e2e"
