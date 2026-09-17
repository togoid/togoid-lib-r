#!/usr/bin/env Rscript
#
# Step 4: draw the enrichment results on the UMAP embedding.
#
# Each figure has two panels: the usual cluster UMAP on the left, and the same
# embedding on the right with each cluster's enriched terms written around its
# centroid, sized by significance.
#
# This step needs neither Seurat nor the Seurat object - only the plain UMAP
# table from step 1 and the enrichment CSV from step 3.
#
# Usage:
#   Rscript 04_visualize_umap.R [results-dir] [top-n] [targets] [show-centroids]
#
# show-centroids: "TRUE" (default) or "FALSE" to hide the centroid markers.

suppressPackageStartupMessages({
  library(togoid)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args) >= 1) args[1] else "results"
top_n <- if (length(args) >= 2) as.integer(args[2]) else 3L
requested <- if (length(args) >= 3) {
  strsplit(args[3], ",", fixed = TRUE)[[1]]
} else {
  c("reactome", "go", "mondo")
}
show_centroids <- if (length(args) >= 4) as.logical(args[4]) else TRUE
if (is.na(show_centroids)) {
  stop("show-centroids must be TRUE or FALSE", call. = FALSE)
}

# A filled circle; see ?points for the other ggplot2 shape codes.
CENTROID_SHAPE <- 16
CENTROID_SIZE <- 2

FDR_CUTOFF <- 0.05
WIDTH <- 20
HEIGHT <- 8
DPI <- 200

titles <- c(
  reactome = "Enriched Reactome pathways",
  go = "Enriched GO biological processes",
  mondo = "Enriched MONDO diseases"
)

cat(strrep("=", 60), "\n")
cat("Step 4: UMAP visualisation of enrichment results\n")
cat(strrep("=", 60), "\n")

embedding <- togoid_umap_from_csv(file.path(results_dir, "01_umap.csv"))
cat(sprintf("Loaded %d cells from %s\n", nrow(embedding),
            file.path(results_dir, "01_umap.csv")))

save_figure <- function(figure, stem, width, height) {
  for (extension in c("pdf", "png")) {
    path <- file.path(results_dir, paste0(stem, ".", extension))
    ggsave(path, figure, width = width, height = height, dpi = DPI)
    cat(sprintf("  wrote %s\n", path))
  }
}

# A reference figure showing where the labels will be anchored.
save_figure(
  togoid_plot_umap_centroids(
    embedding,
    title = "PBMC clusters and centroids",
    centroid_shape = CENTROID_SHAPE
  ),
  "04_umap_centroids", width = 9, height = 8
)

for (name in requested) {
  path <- file.path(results_dir, sprintf("03_enrichment_%s_all.csv", name))
  if (!file.exists(path)) {
    cat(sprintf("\nTarget: %s - %s not found, skipping\n", name, path))
    next
  }

  cat(sprintf("\nTarget: %s\n", name))
  enrichment <- read.csv(path, stringsAsFactors = FALSE)
  cat(sprintf("  loaded %d enrichment rows\n", nrow(enrichment)))

  figure <- togoid_plot_umap_enrichment(
    embedding,
    enrichment,
    top_n = top_n,
    fdr_cutoff = FDR_CUTOFF,
    width = WIDTH,
    height = HEIGHT,
    title_left = "PBMC clusters (UMAP)",
    title_right = sprintf("%s (top %d per cluster)",
                          if (name %in% names(titles)) titles[[name]] else name,
                          top_n),
    show_centroids = show_centroids,
    centroid_shape = CENTROID_SHAPE,
    centroid_size = CENTROID_SIZE,
    verbose = TRUE
  )

  save_figure(figure, sprintf("04_umap_enrichment_%s_top%d", name, top_n),
              width = WIDTH, height = HEIGHT)
}

cat("\nDone.\n")
