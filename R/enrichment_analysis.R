#' Over-representation analysis against a TogoID gene-set library
#'
#' @name enrichment-analysis
NULL

#' Column order used by every enrichment result
#'
#' @return A character vector of column names.
#' @export
#'
#' @examples
#' togoid_enrichment_columns()
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
  as_togoid_enrichment(result)
}

#' Tag a data frame as an enrichment result
#'
#' The result stays an ordinary data frame — every dplyr and base operation keeps
#' working — and only gains a `print` method that formats it for the console.
#'
#' @param x A data frame of enrichment results.
#' @param metadata Named list of provenance and options; kept as an attribute
#'   and written into the header of exported tables. `NULL` keeps what `x` has.
#'
#' @return The same data frame with class `togoid_enrichment` added.
#' @keywords internal
as_togoid_enrichment <- function(x, metadata = NULL) {
  if (!inherits(x, "togoid_enrichment")) {
    class(x) <- unique(c("togoid_enrichment", class(x)))
  }
  if (!is.null(metadata)) {
    attr(x, "togoid_metadata") <- metadata
  }
  x
}

#' Provenance and options recorded on an enrichment result
#'
#' @param enrichment An enrichment data frame.
#'
#' @return A named list; empty when the result carries no metadata.
#' @export
#'
#' @examples
#' # A result built by togoid_enrich_clusters() carries its provenance;
#' # a plain data frame carries none.
#' togoid_enrichment_metadata(data.frame(term_id = "R-HSA-1"))
togoid_enrichment_metadata <- function(enrichment) {
  attr(enrichment, "togoid_metadata") %||% list()
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

  # The options that shaped these numbers, recorded so a written table can say
  # how it was produced.
  metadata <- c(
    togoid_gene_set_provenance(gene_sets),
    list(
      min_set_size = min_set_size,
      max_set_size = if (is.null(max_set_size)) "none" else max_set_size,
      min_overlap = min_overlap,
      background_size = background_size,
      # togoid_enrich_clusters() resolves the default before calling in, so
      # compare the sets rather than trusting the argument being NULL.
      background = if (is.null(background) ||
                       setequal(background_set, togoid_gene_set_genes(gene_sets))) {
        "library genes"
      } else {
        "explicit"
      }
    )
  )

  if (background_size == 0 || query_size == 0 || length(gene_sets$sets) == 0) {
    return(as_togoid_enrichment(empty_enrichment(with_cluster), metadata))
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
    return(as_togoid_enrichment(empty_enrichment(with_cluster), metadata))
  }

  overlaps <- lapply(restricted, function(members) sort(members[members %in% query_set]))
  overlap_count <- lengths(overlaps)

  keep <- overlap_count >= min_overlap
  restricted <- restricted[keep]
  term_size <- term_size[keep]
  overlaps <- overlaps[keep]
  overlap_count <- overlap_count[keep]

  if (length(restricted) == 0) {
    return(as_togoid_enrichment(empty_enrichment(with_cluster), metadata))
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
  as_togoid_enrichment(result, metadata)
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

  metadata <- NULL
  for (result in results) {
    found <- togoid_enrichment_metadata(result)
    if (length(found) > 0) {
      metadata <- found
      break
    }
  }

  combined <- do.call(rbind, results)
  if (is.null(combined) || nrow(combined) == 0) {
    return(as_togoid_enrichment(empty_enrichment(with_cluster = TRUE), metadata))
  }
  rownames(combined) <- NULL
  # rbind() drops attributes, so re-attach the shared metadata.
  as_togoid_enrichment(combined, metadata)
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
#'
#' @examples
#' demo_enrichment <- data.frame(
#'   cluster = c("0", "0", "1"),
#'   term_id = c("R-HSA-202433", "R-HSA-156902", "R-HSA-983695"),
#'   term_label = c("Second messengers", "Peptide chain elongation",
#'                  "BCR activation"),
#'   overlap_count = c(5L, 3L, 4L),
#'   term_size = c(5L, 20L, 4L),
#'   query_size = c(10L, 10L, 8L),
#'   background_size = c(100L, 100L, 100L),
#'   pvalue = c(1e-6, 0.03, 1e-5),
#'   fdr = c(1e-5, 0.04, 1e-4),
#'   fold_enrichment = c(10, 1.5, 12.5),
#'   genes = c("CD3D,CD3E,CD3G,LCK,ZAP70", "RPL10,RPL11,RPS3",
#'             "MS4A1,CD79A,CD79B,CD19"),
#'   stringsAsFactors = FALSE
#' )
#'
#' togoid_significant(demo_enrichment)
#' togoid_significant(demo_enrichment, alpha = 0.01)
#' togoid_significant(demo_enrichment, alpha = 0.01, use = "pvalue")
togoid_significant <- function(enrichment, alpha = 0.05, use = "fdr") {
  if (nrow(enrichment) == 0) {
    return(enrichment)
  }
  result <- enrichment[enrichment[[use]] < alpha, , drop = FALSE]
  rownames(result) <- NULL
  as_togoid_enrichment(result, togoid_enrichment_metadata(enrichment))
}

#' Keep the most significant terms of each cluster
#'
#' @param enrichment An enrichment data frame.
#' @param n Number of terms to keep per cluster.
#'
#' @return The filtered data frame.
#' @export
#'
#' @examples
#' demo_enrichment <- data.frame(
#'   cluster = c("0", "0", "1"),
#'   term_id = c("R-HSA-202433", "R-HSA-156902", "R-HSA-983695"),
#'   term_label = c("Second messengers", "Peptide chain elongation",
#'                  "BCR activation"),
#'   overlap_count = c(5L, 3L, 4L),
#'   term_size = c(5L, 20L, 4L),
#'   query_size = c(10L, 10L, 8L),
#'   background_size = c(100L, 100L, 100L),
#'   pvalue = c(1e-6, 0.03, 1e-5),
#'   fdr = c(1e-5, 0.04, 1e-4),
#'   fold_enrichment = c(10, 1.5, 12.5),
#'   genes = c("CD3D,CD3E,CD3G,LCK,ZAP70", "RPL10,RPL11,RPS3",
#'             "MS4A1,CD79A,CD79B,CD19"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # One row per cluster: the best term of each.
#' togoid_top_terms(demo_enrichment, n = 1)
togoid_top_terms <- function(enrichment, n = 3) {
  if (nrow(enrichment) == 0) {
    return(enrichment)
  }
  if (!("cluster" %in% names(enrichment))) {
    ordered <- enrichment[order(enrichment$fdr, enrichment$pvalue), , drop = FALSE]
    result <- utils::head(ordered, n)
    rownames(result) <- NULL
    return(as_togoid_enrichment(result, togoid_enrichment_metadata(enrichment)))
  }

  parts <- lapply(split(enrichment, enrichment$cluster), function(part) {
    part <- part[order(part$fdr, part$pvalue), , drop = FALSE]
    utils::head(part, n)
  })
  result <- do.call(rbind, parts)
  rownames(result) <- NULL
  as_togoid_enrichment(result, togoid_enrichment_metadata(enrichment))
}

#' Format an enrichment result for display
#'
#' Shortens the columns that make the raw table unreadable in a console: p-values
#' and FDRs become scientific notation, long term labels and gene lists are
#' truncated, and the constant `background_size` column is dropped.
#'
#' @param x An enrichment data frame.
#' @param ... Ignored, present for S3 compatibility.
#' @param max_label Truncate term labels beyond this many characters.
#' @param max_genes Show at most this many gene symbols per row.
#'
#' @return A plain data frame of formatted character columns.
#' @export
format.togoid_enrichment <- function(x, ..., max_label = 52, max_genes = 6) {
  out <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  if (nrow(out) == 0) {
    return(out)
  }

  # query_size and background_size are constant within a query, and the overlap
  # only means anything next to the term size, so they are folded into one "k/M"
  # column. The full numbers stay in the underlying data frame.
  if (all(c("overlap_count", "term_size") %in% names(out))) {
    overlap <- sprintf("%d/%d", out$overlap_count, out$term_size)
    out <- out[, setdiff(names(out), c("overlap_count", "term_size")), drop = FALSE]
    after <- match("term_label", names(out))
    if (is.na(after)) after <- 0L
    out <- data.frame(
      out[, seq_len(after), drop = FALSE],
      overlap = overlap,
      out[, setdiff(seq_along(out), seq_len(after)), drop = FALSE],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  out$query_size <- NULL
  out$background_size <- NULL

  for (column in intersect(c("pvalue", "fdr"), names(out))) {
    out[[column]] <- sprintf("%.2e", out[[column]])
  }
  if ("fold_enrichment" %in% names(out)) {
    out$fold_enrichment <- sprintf("%.2f", out$fold_enrichment)
  }
  if ("term_label" %in% names(out)) {
    long <- nchar(out$term_label) > max_label
    out$term_label[long] <- paste0(substr(out$term_label[long], 1, max_label - 1), "\u2026")
  }
  if ("genes" %in% names(out)) {
    out$genes <- vapply(strsplit(out$genes, ",", fixed = TRUE), function(g) {
      g <- g[nzchar(g)]
      if (length(g) > max_genes) {
        paste0(paste(g[seq_len(max_genes)], collapse = ", "),
               sprintf(" (+%d)", length(g) - max_genes))
      } else {
        paste(g, collapse = ", ")
      }
    }, character(1))
  }

  out
}

#' Print an enrichment result
#'
#' Columns are dropped from the right when the console is too narrow to hold
#' them, so the table stays one row per line instead of wrapping. The data frame
#' itself is untouched — `as.data.frame(x)` still has every column.
#'
#' @param x An enrichment data frame.
#' @param ... Passed to [format.togoid_enrichment()].
#' @param n Rows to show; the rest are summarised in a footer. `Inf` shows all.
#' @param width Console width to fit into; defaults to `getOption("width")`.
#'
#' @return `x`, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' results <- togoid_enrich_clusters(clusters, gene_sets)
#' results          # a formatted table
#' print(results, n = Inf)
#' }
print.togoid_enrichment <- function(x, ..., n = 20, width = getOption("width")) {
  if (nrow(x) == 0) {
    cat("<togoid_enrichment> no enriched terms\n")
    return(invisible(x))
  }

  shown <- if (is.finite(n)) min(n, nrow(x)) else nrow(x)
  formatted <- format.togoid_enrichment(utils::head(x, shown), ...)
  width <- max(as.integer(width), 40L)

  table_width <- function(df) {
    sum(vapply(seq_along(df), function(i) {
      max(nchar(names(df)[i]), max(nchar(df[[i]]), 0L)) + 1L
    }, integer(1)))
  }

  # Drop columns until the table fits on one line per row, least informative
  # first: the cluster, term and its significance are what you came for.
  drop_order <- c("genes", "fold_enrichment", "term_id", "overlap", "pvalue")
  dropped <- character(0)
  for (column in drop_order) {
    if (table_width(formatted) <= width) {
      break
    }
    if (column %in% names(formatted)) {
      formatted <- formatted[, setdiff(names(formatted), column), drop = FALSE]
      dropped <- c(dropped, column)
    }
  }

  clusters <- if ("cluster" %in% names(x)) length(unique(x$cluster)) else 1L
  cat(sprintf("<togoid_enrichment> %d term(s) across %d cluster(s)\n",
              nrow(x), clusters))

  # print.data.frame wraps at getOption("width"), so honour the width argument.
  previous <- options(width = width)
  on.exit(options(previous), add = TRUE)
  print.data.frame(formatted, right = FALSE, row.names = FALSE)

  if (shown < nrow(x)) {
    cat(sprintf("... and %d more row(s); print(x, n = Inf) to see them all\n",
                nrow(x) - shown))
  }
  if (length(dropped) > 0) {
    cat(sprintf("Columns not shown (console too narrow): %s\n",
                paste(dropped, collapse = ", ")))
  }
  invisible(x)
}

#' Build a wide table with one row per cluster
#'
#' Where the enrichment result has one row per term, this has one row per cluster
#' with its best terms side by side — the shape you want when labelling clusters
#' or putting the result next to a UMAP figure.
#'
#' @param enrichment An enrichment data frame.
#' @param top_n Number of terms to include per cluster.
#' @param alpha FDR threshold used to pick and count the terms.
#'
#' @return A data frame with one row per cluster.
#' @export
#'
#' @examples
#' demo_enrichment <- data.frame(
#'   cluster = c("0", "0", "1"),
#'   term_id = c("R-HSA-202433", "R-HSA-156902", "R-HSA-983695"),
#'   term_label = c("Second messengers", "Peptide chain elongation",
#'                  "BCR activation"),
#'   overlap_count = c(5L, 3L, 4L),
#'   term_size = c(5L, 20L, 4L),
#'   query_size = c(10L, 10L, 8L),
#'   background_size = c(100L, 100L, 100L),
#'   pvalue = c(1e-6, 0.03, 1e-5),
#'   fdr = c(1e-5, 0.04, 1e-4),
#'   fold_enrichment = c(10, 1.5, 12.5),
#'   genes = c("CD3D,CD3E,CD3G,LCK,ZAP70", "RPL10,RPL11,RPS3",
#'             "MS4A1,CD79A,CD79B,CD19"),
#'   stringsAsFactors = FALSE
#' )
#'
#' togoid_cluster_table(demo_enrichment, top_n = 2)
togoid_cluster_table <- function(enrichment, top_n = 3, alpha = 0.05) {
  if (nrow(enrichment) == 0) {
    return(data.frame(cluster = character(0), n_tested = integer(0),
                      n_significant = integer(0), stringsAsFactors = FALSE))
  }

  plain <- as.data.frame(unclass(enrichment), stringsAsFactors = FALSE)
  if (!("cluster" %in% names(plain))) {
    plain$cluster <- "query"
  }

  clusters <- unique(plain$cluster)[cluster_sort_key(unique(plain$cluster))]
  rows <- lapply(clusters, function(cluster) {
    part <- plain[plain$cluster == cluster, , drop = FALSE]
    significant <- part[part$fdr < alpha, , drop = FALSE]
    significant <- significant[order(significant$fdr, significant$pvalue), , drop = FALSE]

    row <- data.frame(
      cluster = cluster,
      n_tested = nrow(part),
      n_significant = nrow(significant),
      stringsAsFactors = FALSE
    )
    for (index in seq_len(top_n)) {
      hit <- if (index <= nrow(significant)) significant[index, ] else NULL
      row[[sprintf("top%d_term_id", index)]] <- if (is.null(hit)) "" else hit$term_id
      row[[sprintf("top%d_term_label", index)]] <- if (is.null(hit)) "" else hit$term_label
      row[[sprintf("top%d_fdr", index)]] <- if (is.null(hit)) "" else sprintf("%.3e", hit$fdr)
    }
    row
  })

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Build the provenance header for a written table
#'
#' The TogoID API sits in front of annotation databases that are updated, so the
#' same analysis run a month later can legitimately give different numbers.
#' Recording when the API was queried, and with what options, is what makes a
#' result file interpretable later.
#'
#' @param enrichment An enrichment data frame.
#' @param extra Named list of further entries to append.
#'
#' @return A character vector of `#`-prefixed comment lines.
#' @export
#'
#' @examples
#' demo_enrichment <- data.frame(
#'   cluster = c("0", "0", "1"),
#'   term_id = c("R-HSA-202433", "R-HSA-156902", "R-HSA-983695"),
#'   term_label = c("Second messengers", "Peptide chain elongation",
#'                  "BCR activation"),
#'   overlap_count = c(5L, 3L, 4L),
#'   term_size = c(5L, 20L, 4L),
#'   query_size = c(10L, 10L, 8L),
#'   background_size = c(100L, 100L, 100L),
#'   pvalue = c(1e-6, 0.03, 1e-5),
#'   fdr = c(1e-5, 0.04, 1e-4),
#'   fold_enrichment = c(10, 1.5, 12.5),
#'   genes = c("CD3D,CD3E,CD3G,LCK,ZAP70", "RPL10,RPL11,RPS3",
#'             "MS4A1,CD79A,CD79B,CD19"),
#'   stringsAsFactors = FALSE
#' )
#'
#' cat(togoid_enrichment_header(demo_enrichment), sep = "\n")
togoid_enrichment_header <- function(enrichment, extra = list()) {
  info <- c(
    list(
      generated_at = togoid_timestamp(),
      togoid_version = as.character(utils::packageVersion("togoid"))
    ),
    togoid_enrichment_metadata(enrichment),
    extra
  )
  info$n_terms <- nrow(enrichment)
  if ("cluster" %in% names(enrichment) && nrow(enrichment) > 0) {
    info$n_clusters <- length(unique(enrichment$cluster))
  }

  # Metadata carried over from an earlier step repeats keys such as
  # generated_at; keep the first, which is the one generated just now.
  info <- info[!duplicated(names(info))]

  c(
    "# togoid enrichment results",
    vapply(names(info), function(key) {
      sprintf("# %s: %s", key, paste(as.character(info[[key]]), collapse = ", "))
    }, character(1), USE.NAMES = FALSE)
  )
}

#' Read the provenance header back from a written table
#'
#' @param path Path to a file written by [togoid_write_enrichment()].
#'
#' @return A named list of header values; empty when the file has no header.
#' @export
#'
#' @examples
#' demo_enrichment <- data.frame(
#'   cluster = c("0", "0", "1"),
#'   term_id = c("R-HSA-202433", "R-HSA-156902", "R-HSA-983695"),
#'   term_label = c("Second messengers", "Peptide chain elongation",
#'                  "BCR activation"),
#'   overlap_count = c(5L, 3L, 4L),
#'   term_size = c(5L, 20L, 4L),
#'   query_size = c(10L, 10L, 8L),
#'   background_size = c(100L, 100L, 100L),
#'   pvalue = c(1e-6, 0.03, 1e-5),
#'   fdr = c(1e-5, 0.04, 1e-4),
#'   fold_enrichment = c(10, 1.5, 12.5),
#'   genes = c("CD3D,CD3E,CD3G,LCK,ZAP70", "RPL10,RPL11,RPS3",
#'             "MS4A1,CD79A,CD79B,CD19"),
#'   stringsAsFactors = FALSE
#' )
#'
#' path <- tempfile(fileext = ".tsv")
#' togoid_write_enrichment(demo_enrichment, path)
#' togoid_read_metadata(path)$generated_at
#' unlink(path)
togoid_read_metadata <- function(path) {
  lines <- readLines(path, warn = FALSE)
  header <- lines[startsWith(lines, "#")]
  # Stop at the first non-comment line, so a "#" inside the data is ignored.
  first_data <- which(!startsWith(lines, "#"))
  if (length(first_data) > 0) {
    header <- lines[seq_len(first_data[1] - 1)]
  }

  metadata <- list()
  for (line in header) {
    text <- trimws(sub("^#", "", line))
    if (grepl(":", text, fixed = TRUE)) {
      key <- trimws(sub(":.*$", "", text))
      value <- trimws(sub("^[^:]*:", "", text))
      metadata[[key]] <- value
    }
  }
  metadata
}

#' Write an enrichment result to a delimited text file
#'
#' Tabs rather than commas by default: term labels routinely contain commas,
#' which a CSV has to quote and some spreadsheet imports then mis-parse.
#'
#' @param enrichment An enrichment data frame.
#' @param path Destination file path.
#' @param sep Field separator; tab by default.
#' @param header Write the `#` provenance header from
#'   [togoid_enrichment_header()]. Read such a file back with
#'   `read.delim(path, comment.char = "#")`, or pass `FALSE` for consumers that
#'   cannot skip comment lines.
#' @param extra_header Named list of further header entries.
#'
#' @return The path, invisibly.
#' @export
#'
#' @examples
#' demo_enrichment <- data.frame(
#'   cluster = c("0", "0", "1"),
#'   term_id = c("R-HSA-202433", "R-HSA-156902", "R-HSA-983695"),
#'   term_label = c("Second messengers", "Peptide chain elongation",
#'                  "BCR activation"),
#'   overlap_count = c(5L, 3L, 4L),
#'   term_size = c(5L, 20L, 4L),
#'   query_size = c(10L, 10L, 8L),
#'   background_size = c(100L, 100L, 100L),
#'   pvalue = c(1e-6, 0.03, 1e-5),
#'   fdr = c(1e-5, 0.04, 1e-4),
#'   fold_enrichment = c(10, 1.5, 12.5),
#'   genes = c("CD3D,CD3E,CD3G,LCK,ZAP70", "RPL10,RPL11,RPS3",
#'             "MS4A1,CD79A,CD79B,CD19"),
#'   stringsAsFactors = FALSE
#' )
#'
#' path <- tempfile(fileext = ".tsv")
#' togoid_write_enrichment(demo_enrichment, path)
#'
#' # The "#" header records the provenance; read the table past it.
#' head(readLines(path), 3)
#' read.delim(path, comment.char = "#")
#' unlink(path)
togoid_write_enrichment <- function(enrichment, path, sep = "\t",
                                    header = TRUE, extra_header = list()) {
  directory <- dirname(path)
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }

  # Write through one open connection: write.table(append = TRUE) would warn
  # about appending column names, and this keeps the file in a single pass.
  connection <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)

  if (isTRUE(header)) {
    writeLines(togoid_enrichment_header(enrichment, extra_header), connection)
  }
  utils::write.table(
    as.data.frame(unclass(enrichment), stringsAsFactors = FALSE),
    file = connection, sep = sep, quote = FALSE, row.names = FALSE, na = ""
  )
  invisible(path)
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
#' demo_enrichment <- data.frame(
#'   cluster = c("0", "0", "1"),
#'   term_id = c("R-HSA-202433", "R-HSA-156902", "R-HSA-983695"),
#'   term_label = c("Second messengers", "Peptide chain elongation",
#'                  "BCR activation"),
#'   overlap_count = c(5L, 3L, 4L),
#'   term_size = c(5L, 20L, 4L),
#'   query_size = c(10L, 10L, 8L),
#'   background_size = c(100L, 100L, 100L),
#'   pvalue = c(1e-6, 0.03, 1e-5),
#'   fdr = c(1e-5, 0.04, 1e-4),
#'   fold_enrichment = c(10, 1.5, 12.5),
#'   genes = c("CD3D,CD3E,CD3G,LCK,ZAP70", "RPL10,RPL11,RPS3",
#'             "MS4A1,CD79A,CD79B,CD19"),
#'   stringsAsFactors = FALSE
#' )
#'
#' cat(togoid_enrichment_summary(demo_enrichment))
togoid_enrichment_summary <- function(enrichment, alpha = 0.05, top = 5) {
  lines <- c(togoid_enrichment_header(enrichment), "",
             "Enrichment summary", strrep("=", 60), "")

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
