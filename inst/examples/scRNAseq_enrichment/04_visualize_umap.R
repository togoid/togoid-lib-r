#!/usr/bin/env Rscript
#
# Step 4: draw the enrichment results on the UMAP embedding.
#
# Each figure has two panels: the usual cluster UMAP on the left, and the same
# embedding on the right with each cluster's enriched terms written around its
# centroid, sized by significance.
#
# Beside every figure this writes the same terms as a TSV, so the table and the
# picture always agree: a long table with one row per term, and a wide one with
# one row per cluster.
#
# This step needs neither Seurat nor the Seurat object - only the plain UMAP
# table from step 1 and the enrichment CSV from step 3.
#
# The tables carry a "#" header with the date the TogoID API was queried - taken
# from step 3's output and passed on - plus the options used here. File names
# carry a date stamp.
#
# Usage:
#   Rscript 04_visualize_umap.R [results-dir] [top-n] [targets] [show-centroids] [date-suffix]
#
# show-centroids: "TRUE" (default) or "FALSE". Hiding the markers draws them
#   transparently rather than skipping them, so the labels do not move.
# date-suffix: date stamp for output names (default: today); "" to omit it.

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
date_suffix <- if (length(args) >= 5) args[5] else format(Sys.Date(), "%Y%m%d")
suffix <- if (nzchar(date_suffix)) paste0("_", date_suffix) else ""

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

# Step 3 date-stamps its files, so pick the most recent match rather than
# guessing today's date; a plain unstamped name still works.
find_enrichment_file <- function(name) {
  for (pattern in c(sprintf("^03_enrichment_%s_all.*\\.tsv$", name),
                    sprintf("^03_enrichment_%s_all.*\\.csv$", name))) {
    matches <- sort(list.files(results_dir, pattern = pattern, full.names = TRUE))
    if (length(matches) > 0) {
      return(matches[length(matches)])
    }
  }
  NULL
}

# Write the terms a figure shows as long- and wide-format TSV tables. The
# filters are the same ones the figure used, so the two cannot disagree.
save_tables <- function(enrichment, stem, top_n, metadata) {
  selected <- togoid_select_terms(
    enrichment,
    top_n = top_n,
    fdr_cutoff = FDR_CUTOFF,
    max_label_chars = NULL   # keep full labels in the table
  )
  if (nrow(selected) == 0) {
    cat("  no terms passed the filters; skipping the tables\n")
    return(invisible(NULL))
  }

  long <- togoid_selected_terms_table(selected)

  # Carry step 3's provenance forward and add what this step chose.
  long <- as_togoid_enrichment(long, metadata)
  extra <- list(
    figure_generated_at = togoid_timestamp(),
    selection = sprintf("top %d terms per cluster", top_n),
    fdr_cutoff = FDR_CUTOFF,
    show_centroids = show_centroids
  )

  long_path <- file.path(results_dir, paste0(stem, ".tsv"))
  togoid_write_enrichment(long, long_path, extra_header = extra)
  cat(sprintf("  wrote %s\n", long_path))

  wide_path <- file.path(results_dir, paste0(stem, "_by_cluster.tsv"))
  wide <- togoid_cluster_table(long, top_n = top_n, alpha = FDR_CUTOFF)
  connection <- file(wide_path, open = "wt", encoding = "UTF-8")
  writeLines(
    togoid_enrichment_header(
      long, c(extra, list(table = sprintf("one row per cluster, top %d terms", top_n)))
    ),
    connection
  )
  write.table(wide, connection, sep = "\t", quote = FALSE, row.names = FALSE, na = "")
  close(connection)
  cat(sprintf("  wrote %s\n", wide_path))
}

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
  paste0("04_umap_centroids", suffix), width = 9, height = 8
)

for (name in requested) {
  path <- find_enrichment_file(name)
  if (is.null(path)) {
    cat(sprintf("\nTarget: %s - no step 3 output found, skipping\n", name))
    next
  }

  cat(sprintf("\nTarget: %s\n", name))

  # The "#" header carries the date the API was queried; read.delim skips it and
  # togoid_read_metadata() parses it, so this step can pass it on.
  metadata <- togoid_read_metadata(path)
  separator <- if (endsWith(path, ".tsv")) "\t" else ","
  enrichment <- utils::read.table(
    path, sep = separator, header = TRUE, comment.char = "#",
    quote = "\"", stringsAsFactors = FALSE
  )
  cat(sprintf("  loaded %d enrichment rows from %s\n", nrow(enrichment), path))
  if (!is.null(metadata$api_retrieved_at)) {
    cat(sprintf("  TogoID API retrieved on %s\n", metadata$api_retrieved_at))
  }

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

  stem <- sprintf("04_umap_enrichment_%s_top%d%s", name, top_n, suffix)
  save_figure(figure, stem, width = WIDTH, height = HEIGHT)
  save_tables(enrichment, stem, top_n = top_n, metadata = metadata)
}

cat("\nDone.\n")
