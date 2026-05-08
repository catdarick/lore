#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd -- "${SCRIPT_DIR}/.." && pwd)"
RESULTS_DIR="${PROJECT_DIR}/bench-results/pipeline"

STAMP="$(date -u +"%Y%m%d-%H%M%S")"
COMMIT_HASH="$(git -C "${PROJECT_DIR}" rev-parse --short=8 HEAD 2>/dev/null || echo "nogit")"
RUN_ID="${STAMP}-${COMMIT_HASH}"
RUN_DIR="${RESULTS_DIR}/${RUN_ID}"

CPU_JSON="${RUN_DIR}/cpu.json"
CPU_LOG="${RUN_DIR}/cpu.log"
MEMORY_LOG="${RUN_DIR}/memory.log"
METADATA_FILE="${RUN_DIR}/run.txt"

BENCH_MODE_VALUE="${BENCH_MODE:-full}"
MEMORY_MODE_VALUE="${MEMORY_MODE:-full}"

mkdir -p "${RUN_DIR}"

cd "${PROJECT_DIR}"

echo "Running CPU benchmarks (BENCH_MODE=${BENCH_MODE_VALUE})"
BENCH_MODE="${BENCH_MODE_VALUE}" \
  stack bench lore:lore-bench --benchmark-arguments "--json ${CPU_JSON}" \
  2>&1 \
  | tee "${CPU_LOG}"

echo "Running memory benchmarks (MEMORY_MODE=${MEMORY_MODE_VALUE})"
MEMORY_MODE="${MEMORY_MODE_VALUE}" \
  stack bench lore:lore-memory-bench --ba "+RTS -T" \
  2>&1 \
  | tee "${MEMORY_LOG}"

cat > "${METADATA_FILE}" <<META
run_id=${RUN_ID}
created_at_utc=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
commit=${COMMIT_HASH}
bench_mode=${BENCH_MODE_VALUE}
memory_mode=${MEMORY_MODE_VALUE}
cpu_json=${CPU_JSON}
cpu_log=${CPU_LOG}
memory_log=${MEMORY_LOG}
META

ln -sfn "${RUN_ID}" "${RESULTS_DIR}/latest"

echo ""
echo "Pipeline run completed"
echo "  run_id:      ${RUN_ID}"
echo "  run_dir:     ${RUN_DIR}"
echo "  cpu_json:    ${CPU_JSON}"
echo "  memory_log:  ${MEMORY_LOG}"
