#!/usr/bin/env bash
set -euo pipefail

export LC_ALL=C
export LANG=C

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd -- "${SCRIPT_DIR}/.." && pwd)"
PIPELINE_DIR="${PROJECT_DIR}/bench-results/pipeline"

usage() {
  cat <<USAGE
Usage:
  $(basename "$0") <old-run-id|old-run-dir> <new-run-id|new-run-dir>

Examples:
  $(basename "$0") 20260508-210000-abcd1234 20260508-220000-abcd1234
  $(basename "$0") ${PIPELINE_DIR}/20260508-210000-abcd1234 ${PIPELINE_DIR}/20260508-220000-abcd1234
USAGE
}

resolve_run_dir() {
  local value="$1"
  if [[ -d "$value" ]]; then
    printf "%s\n" "$value"
    return 0
  fi

  if [[ -d "${PIPELINE_DIR}/${value}" ]]; then
    printf "%s\n" "${PIPELINE_DIR}/${value}"
    return 0
  fi

  return 1
}

extract_memory_tsv() {
  local log_file="$1"
  awk -F'\t' '$1 == "MEMORY_RESULT" && NF >= 7 {print $2"\t"$3"\t"$4"\t"$5"\t"$6"\t"$7}' "$log_file" | sort
}

if [[ $# -ne 2 ]]; then
  usage
  exit 1
fi

OLD_RUN_DIR="$(resolve_run_dir "$1" || true)"
NEW_RUN_DIR="$(resolve_run_dir "$2" || true)"

if [[ -z "$OLD_RUN_DIR" ]]; then
  echo "Could not resolve old run: $1" >&2
  exit 1
fi
if [[ -z "$NEW_RUN_DIR" ]]; then
  echo "Could not resolve new run: $2" >&2
  exit 1
fi

OLD_CPU_JSON="${OLD_RUN_DIR}/cpu.json"
NEW_CPU_JSON="${NEW_RUN_DIR}/cpu.json"
OLD_MEMORY_LOG="${OLD_RUN_DIR}/memory.log"
NEW_MEMORY_LOG="${NEW_RUN_DIR}/memory.log"

for path in "$OLD_CPU_JSON" "$NEW_CPU_JSON" "$OLD_MEMORY_LOG" "$NEW_MEMORY_LOG"; do
  if [[ ! -f "$path" ]]; then
    echo "Missing required artifact: $path" >&2
    exit 1
  fi
done

echo "Pipeline comparison"
echo "  old run: ${OLD_RUN_DIR}"
echo "  new run: ${NEW_RUN_DIR}"
echo ""
echo "=== CPU (Criterion mean) ==="
"${SCRIPT_DIR}/bench-compare.sh" "$OLD_CPU_JSON" "$NEW_CPU_JSON"

echo ""
echo "=== Memory (GHC.Stats) ==="
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

OLD_MEM_TSV="${TMP_DIR}/old-memory.tsv"
NEW_MEM_TSV="${TMP_DIR}/new-memory.tsv"
JOINED_TSV="${TMP_DIR}/joined-memory.tsv"

extract_memory_tsv "$OLD_MEMORY_LOG" > "$OLD_MEM_TSV"
extract_memory_tsv "$NEW_MEMORY_LOG" > "$NEW_MEM_TSV"

join -t $'\t' -j 1 "$OLD_MEM_TSV" "$NEW_MEM_TSV" > "$JOINED_TSV" || true

common_count="$(wc -l < "$JOINED_TSV" | tr -d ' ')"
old_count="$(wc -l < "$OLD_MEM_TSV" | tr -d ' ')"
new_count="$(wc -l < "$NEW_MEM_TSV" | tr -d ' ')"

echo "Comparing memory cases"
echo "  old: $OLD_MEMORY_LOG"
echo "  new: $NEW_MEMORY_LOG"
echo "  matched case names: ${common_count} (old=${old_count}, new=${new_count})"
echo
printf "%12s  %12s  %12s  %-10s  %-56s\n" "alloc(%)" "live(%)" "in-use(%)" "status" "case"
printf "%12s  %12s  %12s  %-10s  %-56s\n" "--------" "-------" "--------" "------" "----"

awk -F'\t' '
  function pct(old, new) {
    if (old == 0) return 0
    return ((new - old) / old) * 100
  }
  {
    name = $1
    oldAlloc = $2 + 0
    oldLiveAfter = $4 + 0
    oldMemAfter = $6 + 0

    newAlloc = $7 + 0
    newLiveAfter = $9 + 0
    newMemAfter = $11 + 0

    allocPct = pct(oldAlloc, newAlloc)
    livePct = pct(oldLiveAfter, newLiveAfter)
    memPct = pct(oldMemAfter, newMemAfter)

    status = "neutral"
    if (allocPct >= 15 || livePct >= 15 || memPct >= 15) status = "regress"
    else if (allocPct <= -15 || livePct <= -15 || memPct <= -15) status = "improve"

    printf "%+12.3f\t%+12.3f\t%+12.3f\t%-10s\t%-56s\n", allocPct, livePct, memPct, status, name
  }
' "$JOINED_TSV" | sort -t $'\t' -k1,1nr | awk -F'\t' '{printf "%12s  %12s  %12s  %-10s  %-56s\n", $1"%", $2"%", $3"%", $4, $5}'
