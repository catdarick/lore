#!/usr/bin/env bash
set -euo pipefail

export LC_ALL=C
export LANG=C

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd -- "${SCRIPT_DIR}/.." && pwd)"
RESULTS_DIR="${PROJECT_DIR}/bench-results"

usage() {
  cat <<USAGE
Usage:
  $(basename "$0") <old.json> <new.json>
  $(basename "$0")

If no files are provided, compares the latest two *valid* timestamped files:
  ${RESULTS_DIR}/lore-bench-*.json
USAGE
}

is_valid_criterion_json() {
  local f="$1"
  jq -e '.[0] == "criterion" and (.[2] | type == "array")' "$f" >/dev/null 2>&1
}

resolve_inputs() {
  if [[ $# -eq 2 ]]; then
    OLD_JSON="$1"
    NEW_JSON="$2"
    return 0
  fi

  if [[ $# -ne 0 ]]; then
    usage
    exit 1
  fi

  mapfile -t files < <(ls -1t "${RESULTS_DIR}"/lore-bench-[0-9]*.json 2>/dev/null || true)
  valid=()
  for f in "${files[@]}"; do
    if is_valid_criterion_json "$f"; then
      valid+=("$f")
    fi
    if [[ ${#valid[@]} -ge 2 ]]; then
      break
    fi
  done

  if [[ ${#valid[@]} -lt 2 ]]; then
    echo "Need at least two valid timestamped benchmark files under ${RESULTS_DIR}" >&2
    exit 1
  fi

  NEW_JSON="${valid[0]}"
  OLD_JSON="${valid[1]}"
}

resolve_inputs "$@"

if [[ ! -f "$OLD_JSON" ]]; then
  echo "Missing old JSON: $OLD_JSON" >&2
  exit 1
fi
if [[ ! -f "$NEW_JSON" ]]; then
  echo "Missing new JSON: $NEW_JSON" >&2
  exit 1
fi
if ! is_valid_criterion_json "$OLD_JSON"; then
  echo "Invalid criterion JSON: $OLD_JSON" >&2
  exit 1
fi
if ! is_valid_criterion_json "$NEW_JSON"; then
  echo "Invalid criterion JSON: $NEW_JSON" >&2
  exit 1
fi

TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

OLD_TSV="${TMP_DIR}/old.tsv"
NEW_TSV="${TMP_DIR}/new.tsv"
JOINED_TSV="${TMP_DIR}/joined.tsv"

jq -r '.[2][] | [.reportName, .reportAnalysis.anMean.estPoint] | @tsv' "$OLD_JSON" | sort > "$OLD_TSV"
jq -r '.[2][] | [.reportName, .reportAnalysis.anMean.estPoint] | @tsv' "$NEW_JSON" | sort > "$NEW_TSV"

join -t $'\t' -j 1 "$OLD_TSV" "$NEW_TSV" > "$JOINED_TSV"

common_count="$(wc -l < "$JOINED_TSV" | tr -d ' ')"
old_count="$(wc -l < "$OLD_TSV" | tr -d ' ')"
new_count="$(wc -l < "$NEW_TSV" | tr -d ' ')"

echo "Comparing benchmark means"
echo "  old: $OLD_JSON"
echo "  new: $NEW_JSON"
echo "  matched benchmark names: ${common_count} (old=${old_count}, new=${new_count})"
echo
printf "%12s  %-10s  %-50s  %14s  %14s\n" "delta(%)" "status" "benchmark" "old_mean(s)" "new_mean(s)"
printf "%12s  %-10s  %-50s  %14s  %14s\n" "--------" "------" "---------" "-----------" "-----------"

awk -F'\t' '
  {
    name = $1
    old = $2 + 0
    new = $3 + 0
    if (old == 0) {
      pct = 0
    } else {
      pct = ((new - old) / old) * 100
    }
    status = "neutral"
    if (pct >= 10) status = "regress"
    else if (pct <= -10) status = "improve"

    printf "%+12.3f\t%-10s\t%-50s\t%.9g\t%.9g\n", pct, status, name, old, new
  }
' "$JOINED_TSV" | sort -t $'\t' -k1,1nr | awk -F'\t' '{printf "%12s  %-10s  %-50s  %14s  %14s\n", $1"%", $2, $3, $4, $5}'
