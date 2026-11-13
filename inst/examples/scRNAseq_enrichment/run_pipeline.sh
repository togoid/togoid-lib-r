#!/usr/bin/env bash
#
# Run the whole scRNA-seq enrichment example.
#
# Usage:
#   ./run_pipeline.sh [source] [results-dir]
#
# `source` defaults to pbmc4k, which step 1 pulls through the TENxPBMCData
# package, so the whole example runs with no arguments and no manual download:
#
#   ./run_pipeline.sh
#
# Pass a directory instead to use your own 10x matrix:
#
#   ./run_pipeline.sh ../data/filtered_feature_bc_matrix results
#
set -euo pipefail

SOURCE="${1:-pbmc4k}"
RESULTS_DIR="${2:-results}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RSCRIPT="${RSCRIPT:-Rscript}"

if [ "$SOURCE" != "pbmc4k" ] && [ ! -d "$SOURCE" ]; then
    echo "Error: $SOURCE is neither 'pbmc4k' nor a directory" >&2
    echo "See README.md for the two ways to supply the counts." >&2
    exit 1
fi

mkdir -p "$RESULTS_DIR"

echo "Source:  $SOURCE"
echo "Results: $RESULTS_DIR"
echo

"$RSCRIPT" "$SCRIPT_DIR/01_clustering.R"     "$SOURCE" "$RESULTS_DIR"
"$RSCRIPT" "$SCRIPT_DIR/02_find_markers.R"   "$RESULTS_DIR"
"$RSCRIPT" "$SCRIPT_DIR/03_enrichment.R"     "$RESULTS_DIR"
"$RSCRIPT" "$SCRIPT_DIR/04_visualize_umap.R" "$RESULTS_DIR"

echo
echo "Pipeline finished. Figures:"
ls -1 "$RESULTS_DIR"/04_*.pdf 2>/dev/null || true
