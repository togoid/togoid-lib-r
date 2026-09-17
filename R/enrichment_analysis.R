#' Over-representation analysis against a TogoID gene-set library
#'
#' @name enrichment-analysis
NULL

#' Column order used by every enrichment result
#'
#' @return A character vector of column names.
#' @export
togoid_enrichment_columns <- function() {
  c("cluster", "term_id", "term_label", "overlap_count", "term_size",
    "query_size", "background_size", "pvalue", "fdr", "fold_enrichment", "genes")
}

#' An empty enrichment result
#'
#' Keeping the column set identical whether or not anything was found means
#' downstream code never has to special-case the empty result.
#'
#' @param with_cluster Include the `cluster` column.
#'
#' @return A zero-row data frame.
#' @keywords internal
empty_enrichment <- function(with_cluster = FALSE) {
  result <- data.frame(
    cluster = character(0),
    term_id = character(0),
    term_label = character(0),
    overlap_count = integer(0),
    term_size = integer(0),
    query_size = integer(0),
    background_size = integer(0),
    pvalue = numeric(0),
    fdr = numeric(0),
    fold_enrichment = numeric(0),
    genes = character(0),
    stringsAsFactors = FALSE
  )
  if (!with_cluster) {
    result$cluster <- NULL
  }
  result
}

#' Test a gene list for over-representation
#'
#' @param query_genes Character vector of genes of interest, in the same
#'   spelling as the library's gene sets (gene symbols when the library was
#'   built from symbols).
#' @param gene_sets A `togoid_gene_sets` object.
#' @param background Character vector giving the universe of genes. Defaults to
#'   every gene in the library, i.e. every gene TogoID could annotate.
#' @param min_set_size Skip terms with fewer background genes than this.
#' @param max_set_size Skip terms with more background genes than this; `NULL`
#'   disables the upper bound.
#' @param min_overlap Skip terms with fewer overlapping genes than this.
#' @param cluster Optional cluster label recorded on every row.
#'
#' @return A data frame sorted by FDR, with the columns given by
#'   [togoid_enrichment_columns()].
#' @export
#'
#' @examples
#' \dontrun{
#' genes <- c("CD3D", "CD3E", "CD3G", "LCK", "ZAP70", "MS4A1", "CD79A")
#' sets <- togoid_reactome_gene_sets(genes)
#' togoid_enrich(c("CD3D", "CD3E", "LCK", "ZAP70"), sets, min_set_size = 3)
#' }
togoid_enrich <- function(query_genes,
                          gene_sets,
                          background = NULL,
                          min_set_size = 5,
                          max_set_size = 500,
                          min_overlap = 1,
                          cluster = NULL) {
  with_cluster <- !is.null(cluster)

  background_set <- if (is.null(background)) {
    togoid_gene_set_genes(gene_sets)
  } else {
    unique(as.character(background))
  }

  # Testing genes outside the universe would inflate the query size relative to
  # the background and bias every p-value downwards.
  query_set <- unique(as.character(query_genes))
  query_set <- query_set[query_set %in% background_set]

  background_size <- length(background_set)
  query_size <- length(query_set)

  if (background_size == 0 || query_size == 0 || length(gene_sets$sets) == 0) {
    return(empty_enrichment(with_cluster))
  }

  # Restrict each gene set to the background, so term sizes and the background
  # agree with one another.
  restricted <- lapply(gene_sets$sets, function(members) members[members %in% background_set])
  term_size <- lengths(restricted)

  keep <- term_size >= min_set_size
  if (!is.null(max_set_size)) {
    keep <- keep & term_size <= max_set_size
  }
  restricted <- restricted[keep]
  term_size <- term_size[keep]

  if (length(restricted) == 0) {
    return(empty_enrichment(with_cluster))
  }

  overlaps <- lapply(restricted, function(members) sort(members[members %in% query_set]))
  overlap_count <- lengths(overlaps)

  keep <- overlap_count >= min_overlap
  restricted <- restricted[keep]
  term_size <- term_size[keep]
  overlaps <- overlaps[keep]
  overlap_count <- overlap_count[keep]

  if (length(restricted) == 0) {
    return(empty_enrichment(with_cluster))
  }

  term_ids <- names(restricted)
  pvalue <- togoid_hypergeometric_pvalue(
    k = overlap_count,
    background_size = background_size,
    term_size = term_size,
    query_size = query_size
  )

  result <- data.frame(
    term_id = term_ids,
    term_label = togoid_term_labels(gene_sets, term_ids),
    overlap_count = as.integer(overlap_count),
    term_size = as.integer(term_size),
    query_size = as.integer(query_size),
    background_size = as.integer(background_size),
    pvalue = pvalue,
    fdr = togoid_fdr(pvalue),
    fold_enrichment = togoid_fold_enrichment(
      overlap_count, background_size, term_size, query_size
    ),
    genes = vapply(overlaps, paste, character(1), collapse = ","),
    stringsAsFactors = FALSE
  )

  if (with_cluster) {
    result$cluster <- as.character(cluster)
  }

  result <- result[order(result$fdr, result$pvalue, -result$fold_enrichment), , drop = FALSE]
  result <- result[, intersect(togoid_enrichment_columns(), names(result)), drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Run enrichment for several clusters against a shared background
#'
#' Sharing one background across clusters is what makes the FDR values
#' comparable between them.
#'
#' @param cluster_genes A named list mapping cluster label to its gene vector,
#'   or a data frame with a cluster column and a gene column.
#' @param gene_sets A `togoid_gene_sets` object.
#' @param background Shared universe of genes; defaults to the library's genes.
#' @param min_set_size Minimum term size.
#' @param max_set_size Maximum term size, or `NULL`.
#' @param min_overlap Minimum overlap required to report a term.
#' @param cluster_column Cluster column name when `cluster_genes` is a data frame.
#' @param gene_column Gene column name when `cluster_genes` is a data frame.
#' @param verbose Print a one-line progress report per cluster.
#'
#' @return A data frame with one row per tested term per cluster.
#' @export
#'
#' @examples
#' \dontrun{
#' clusters <- list(
#'   T = c("CD3D", "CD3E", "CD3G", "LCK", "ZAP70"),
#'   B = c("MS4A1", "CD79A", "CD79B", "CD19", "BLNK")
#' )
#' sets <- togoid_reactome_gene_sets(unlist(clusters, use.names = FALSE))
#' togoid_enrich_clusters(clusters, sets, min_set_size = 3)
#' }
togoid_enrich_clusters <- function(cluster_genes,
                                   gene_sets,
                                   background = NULL,
                                   min_set_size = 5,
                                   max_set_size = 500,
                                   min_overlap = 1,
                                   cluster_column = "cluster",
                                   gene_column = "gene",
                                   verbose = FALSE) {
  if (is.data.frame(cluster_genes)) {
    missing <- setdiff(c(cluster_column, gene_column), names(cluster_genes))
    if (length(missing) > 0) {
      stop(sprintf("cluster_genes is missing column(s): %s",
                   paste(missing, collapse = ", ")), call. = FALSE)
    }
    cluster_genes <- split(
      as.character(cluster_genes[[gene_column]]),
      as.character(cluster_genes[[cluster_column]])
    )
  }

  if (!is.list(cluster_genes) || is.null(names(cluster_genes))) {
    stop("cluster_genes must be a named list or a data frame", call. = FALSE)
  }

  background_set <- if (is.null(background)) {
    togoid_gene_set_genes(gene_sets)
  } else {
    unique(as.character(background))
  }

  # cluster_sort_key() already returns an ordering vector; ordering it again
  # would scramble the clusters.
  clusters <- names(cluster_genes)[cluster_sort_key(names(cluster_genes))]
  results <- list()

  for (cluster in clusters) {
    result <- togoid_enrich(
      cluster_genes[[cluster]],
      gene_sets,
      background = background_set,
      min_set_size = min_set_size,
      max_set_size = max_set_size,
      min_overlap = min_overlap,
      cluster = cluster
    )
    if (verbose) {
      significant <- sum(result$fdr < 0.05)
      cat(sprintf("  cluster %s: %d terms tested, %d significant (FDR < 0.05)\n",
                  cluster, nrow(result), significant))
    }
    results[[length(results) + 1]] <- result
  }

  combined <- do.call(rbind, results)
  if (is.null(combined) || nrow(combined) == 0) {
    return(empty_enrichment(with_cluster = TRUE))
  }
  rownames(combined) <- NULL
  combined
}

#' Order cluster labels numerically when possible
#'
#' @param clusters Character vector of cluster labels.
#'
#' @return An integer ordering vector.
#' @keywords internal
cluster_sort_key <- function(clusters) {
  numeric_value <- suppressWarnings(as.numeric(clusters))
  order(is.na(numeric_value), numeric_value, clusters)
}

#' Keep only significant enrichment rows
#'
#' @param enrichment An enrichment data frame.
#' @param alpha Cut-off value.
#' @param use Column to threshold: `"fdr"` (default) or `"pvalue"`.
#'
#' @return The filtered data frame.
#' @export
togoid_significant <- function(enrichment, alpha = 0.05, use = "fdr") {
  if (nrow(enrichment) == 0) {
    return(enrichment)
  }
  result <- enrichment[enrichment[[use]] < alpha, , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Keep the most significant terms of each cluster
#'
#' @param enrichment An enrichment data frame.
#' @param n Number of terms to keep per cluster.
#'
#' @return The filtered data frame.
#' @export
togoid_top_terms <- function(enrichment, n = 3) {
  if (nrow(enrichment) == 0) {
    return(enrichment)
  }
  if (!("cluster" %in% names(enrichment))) {
    ordered <- enrichment[order(enrichment$fdr, enrichment$pvalue), , drop = FALSE]
    result <- utils::head(ordered, n)
    rownames(result) <- NULL
    return(result)
  }

  parts <- lapply(split(enrichment, enrichment$cluster), function(part) {
    part <- part[order(part$fdr, part$pvalue), , drop = FALSE]
    utils::head(part, n)
  })
  result <- do.call(rbind, parts)
  rownames(result) <- NULL
  result
}

#' Build a readable per-cluster summary
#'
#' @param enrichment An enrichment data frame.
#' @param alpha FDR threshold used to count significant terms.
#' @param top Number of terms listed per cluster.
#'
#' @return A single character string, ready to print or write to a file.
#' @export
#'
#' @examples
#' \dontrun{
#' cat(togoid_enrichment_summary(results))
#' }
togoid_enrichment_summary <- function(enrichment, alpha = 0.05, top = 5) {
  lines <- c("Enrichment summary", strrep("=", 60), "")

  if (nrow(enrichment) == 0) {
    return(paste(c(lines, "No terms tested.", ""), collapse = "\n"))
  }

  has_cluster <- "cluster" %in% names(enrichment)
  groups <- if (has_cluster) {
    split(enrichment, enrichment$cluster)[
      unique(enrichment$cluster[cluster_sort_key(enrichment$cluster)])
    ]
  } else {
    list(query = enrichment)
  }

  for (name in names(groups)) {
    part <- groups[[name]]
    if (is.null(part)) next
    significant <- part[part$fdr < alpha, , drop = FALSE]
    significant <- significant[order(significant$fdr), , drop = FALSE]

    lines <- c(lines, sprintf("Cluster %s:", name))
    lines <- c(lines, sprintf("  terms tested: %d", nrow(part)))
    lines <- c(lines, sprintf("  significant (FDR < %s): %d", alpha, nrow(significant)))

    for (i in seq_len(min(top, nrow(significant)))) {
      row <- significant[i, ]
      lines <- c(lines, sprintf("    - %s [%s]", row$term_label, row$term_id))
      lines <- c(lines, sprintf("      FDR=%.2e  genes=%d/%d  fold=%.2f",
                                row$fdr, row$overlap_count, row$term_size,
                                row$fold_enrichment))
    }
    lines <- c(lines, "")
  }

  paste(lines, collapse = "\n")
}
