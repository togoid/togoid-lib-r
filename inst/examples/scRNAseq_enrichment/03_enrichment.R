#!/usr/bin/env Rscript
#
# Step 3: convert marker genes with TogoID and test them for enrichment.
#
# This is the step the TogoID library exists for. One call to togoid_gene_sets()
# takes gene symbols all the way to an annotation database - resolving symbols
# to NCBI Gene IDs, walking the route, and fetching term labels - and the same
# code serves Reactome, GO and MONDO by changing nothing but the route.
#
# Every output carries a "#" header with the date the TogoID API was queried and
# the options used, and its file name carries a date stamp, so a re-run against
# an updated API does not overwrite an earlier result.
#
# Usage:
#   Rscript 03_enrichment.R [results-dir] [targets] [date-suffix]
#
# Example:
#   Rscript 03_enrichment.R results reactome,go
#   Rscript 03_enrichment.R results reactome ""     # no date stamp

suppressPackageStartupMessages(library(togoid))

args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args) >= 1) args[1] else "results"
requested <- if (length(args) >= 2) {
  strsplit(args[2], ",", fixed = TRUE)[[1]]
} else {
  c("reactome", "go", "mondo")
}

TAXONOMY <- "9606"

# Date stamp for output file names; pass "" as the third argument to omit it.
date_suffix <- if (length(args) >= 3) args[3] else format(Sys.Date(), "%Y%m%d")

# Build a file name with an optional date stamp before the extension.
stamped <- function(name, extension) {
  if (nzchar(date_suffix)) {
    file.path(results_dir, sprintf("%s_%s.%s", name, date_suffix, extension))
  } else {
    file.path(results_dir, sprintf("%s.%s", name, extension))
  }
}

# Databases analysed by default, with the route and term-size bounds each one
# wants. GO and Reactome have large sets; MONDO's disease sets are much smaller,
# so a lower minimum keeps them testable.
routes <- togoid_enrichment_routes()
targets <- list(
  reactome = list(route = routes$reactome, term_filters = NULL,
                  min_set_size = 5, max_set_size = 500),
  go       = list(route = routes$go,
                  term_filters = list(go_aspect = "biological_process"),
                  min_set_size = 5, max_set_size = 500),
  mondo    = list(route = routes$mondo, term_filters = NULL,
                  min_set_size = 3, max_set_size = 500)
)

unknown <- setdiff(requested, names(targets))
if (length(unknown) > 0) {
  stop(sprintf("Unknown target(s): %s. Available: %s",
               paste(unknown, collapse = ", "),
               paste(names(targets), collapse = ", ")), call. = FALSE)
}

cat(strrep("=", 60), "\n")
cat("Step 3: TogoID conversion and enrichment analysis\n")
cat(strrep("=", 60), "\n")

long <- read.csv(file.path(results_dir, "02_markers.csv"), stringsAsFactors = FALSE)
markers <- split(as.character(long$gene), as.character(long$cluster))
cat(sprintf("Loaded %d marker genes across %d clusters\n",
            nrow(long), length(markers)))

all_genes <- sort(unique(as.character(long$gene)))
cat(sprintf("Unique marker genes (background): %d\n", length(all_genes)))

for (name in requested) {
  config <- targets[[name]]

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat(sprintf("Target: %s  (%s)\n", name, paste(config$route, collapse = " -> ")))
  cat(strrep("=", 60), "\n")

  gene_sets <- togoid_gene_sets(
    all_genes,
    route = config$route,
    taxonomy = TAXONOMY,
    term_filters = config$term_filters,
    verbose = TRUE
  )

  cache_path <- stamped(sprintf("03_genesets_%s", name), "json")
  togoid_save_gene_sets(gene_sets, cache_path)
  cat(sprintf("Saved gene sets to %s\n", cache_path))

  if (length(gene_sets) == 0) {
    cat(sprintf("No gene sets built for %s; skipping.\n", name))
    next
  }

  results <- togoid_enrich_clusters(
    markers,
    gene_sets,
    min_set_size = config$min_set_size,
    max_set_size = config$max_set_size,
    verbose = TRUE
  )

  all_path <- stamped(sprintf("03_enrichment_%s_all", name), "tsv")
  togoid_write_enrichment(results, all_path)
  cat(sprintf("Wrote %d rows to %s\n", nrow(results), all_path))

  significant <- togoid_significant(results, alpha = 0.05)
  sig_path <- stamped(sprintf("03_enrichment_%s_significant", name), "tsv")
  togoid_write_enrichment(significant, sig_path)
  cat(sprintf("Wrote %d significant rows to %s\n", nrow(significant), sig_path))

  summary_path <- stamped(sprintf("03_enrichment_%s_summary", name), "txt")
  writeLines(togoid_enrichment_summary(results), summary_path)
  cat(sprintf("Wrote %s\n", summary_path))

  # An RDS keeps the objects for further analysis in R without re-querying.
  saveRDS(
    list(gene_sets = gene_sets, results = results),
    stamped(sprintf("03_enrichment_%s", name), "rds")
  )
}

cat("\nNext: 04_visualize_umap.R\n")
