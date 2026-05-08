#!/usr/bin/env bash
set -euo pipefail

BENCH_MODE=smoke stack bench lore:lore-bench --benchmark-arguments "-L 0.1 -n 1"
