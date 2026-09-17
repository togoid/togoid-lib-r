#!/usr/bin/env bash
#
# Run the whole scRNA-seq enrichment example.
#
# Usage:
#   ./run_pipeline.sh <10x-matrix-dir> [results-dir]
#
# Example:
#   ./run_pipeline.sh ../data/filtered_feature_bc_matrix results
#
set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <10x-matrix-dir> [results-dir]" >&2
    exit 1
fi

DATA_DIR="$1"
RESULTS_DIR="${2:-results}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RSCRIPT="${RSCRIPT:-Rscript}"

if [ ! -d "$DATA_DIR" ]; then
    echo "Error: $DATA_DIR is not a directory" >&2
    echo "Download a 10x filtered feature-barcode matrix first; see README.md." >&2
    exit 1
fi

mkdir -p "$RESULTS_DIR"

echo "Data:    $DATA_DIR"
echo "Results: $RESULTS_DIR"
echo

"$RSCRIPT" "$SCRIPT_DIR/01_clustering.R"     "$DATA_DIR" "$RESULTS_DIR"
"$RSCRIPT" "$SCRIPT_DIR/02_find_markers.R"   "$RESULTS_DIR"
"$RSCRIPT" "$SCRIPT_DIR/03_enrichment.R"     "$RESULTS_DIR"
"$RSCRIPT" "$SCRIPT_DIR/04_visualize_umap.R" "$RESULTS_DIR"

echo
echo "Pipeline finished. Figures:"
ls -1 "$RESULTS_DIR"/04_*.pdf 2>/dev/null || true
