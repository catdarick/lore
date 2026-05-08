#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd -- "${SCRIPT_DIR}/.." && pwd)"
OUTPUT_DIR="${PROJECT_DIR}/bench-results"
STAMP="$(date -u +"%Y%m%d-%H%M%S")"
COMMIT_HASH="$(git -C "${PROJECT_DIR}" rev-parse --short=8 HEAD 2>/dev/null || echo "nogit")"
OUTPUT_JSON="${OUTPUT_DIR}/lore-bench-${STAMP}-${COMMIT_HASH}.json"
LATEST_JSON="${OUTPUT_DIR}/lore-bench-latest.json"

mkdir -p "${OUTPUT_DIR}"
cd "${PROJECT_DIR}"
stack bench lore:lore-bench --benchmark-arguments "--json ${OUTPUT_JSON}"
cp -f "${OUTPUT_JSON}" "${LATEST_JSON}"
echo "Saved benchmark JSON: ${OUTPUT_JSON}"
