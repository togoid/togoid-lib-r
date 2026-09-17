#!/usr/bin/env Rscript
#
# Step 2: find marker genes for every cluster.
#
# A Wilcoxon rank-sum test per cluster gives the gene symbols that feed TogoID
# in step 3. The gene lists are also written as plain text, one file per
# cluster, so they can be inspected or reused outside this pipeline.
#
# Usage:
#   Rscript 02_find_markers.R [results-dir]

suppressPackageStartupMessages({
  library(Seurat)
  library(togoid)
})

args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args) >= 1) args[1] else "results"

TOP_N <- 100
PVAL_CUTOFF <- 0.05
LOGFC_MIN <- 0.25

cat(strrep("=", 60), "\n")
cat("Step 2: marker genes per cluster\n")
cat(strrep("=", 60), "\n")

path <- file.path(results_dir, "01_clustered.rds")
cat(sprintf("Loading %s\n", path))
object <- readRDS(path)

cat("Testing differential expression (Wilcoxon)...\n")
markers_table <- FindAllMarkers(
  object,
  only.pos = TRUE,
  test.use = "wilcox",
  logfc.threshold = LOGFC_MIN,
  verbose = FALSE
)

markers <- togoid_markers_from_seurat(
  markers_table,
  pval_cutoff = PVAL_CUTOFF,
  logfc_min = LOGFC_MIN,
  top_n = TOP_N
)

for (cluster in names(markers)) {
  cat(sprintf("  cluster %s: %d marker genes\n", cluster, length(markers[[cluster]])))
}

# One plain-text list per cluster, for inspection and reuse.
lists_dir <- file.path(results_dir, "02_marker_gene_lists")
togoid_write_marker_lists(markers, lists_dir)
cat(sprintf("  wrote gene lists to %s\n", lists_dir))

# A single long-format table, which is what step 3 reads.
long <- do.call(rbind, lapply(names(markers), function(cluster) {
  data.frame(cluster = cluster, gene = markers[[cluster]], stringsAsFactors = FALSE)
}))
write.csv(long, file.path(results_dir, "02_markers.csv"), row.names = FALSE)
cat(sprintf("  wrote %s\n", file.path(results_dir, "02_markers.csv")))

# The full statistics, in case you want to revisit the thresholds.
write.csv(markers_table, file.path(results_dir, "02_marker_stats.csv"), row.names = FALSE)

cat("\nNext: 03_enrichment.R\n")
