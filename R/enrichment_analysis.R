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
  as_togoid_enrichment(result)
}

#' Tag a data frame as an enrichment result
#'
#' The result stays an ordinary data frame — every dplyr and base operation keeps
#' working — and only gains a `print` method that formats it for the console.
#'
#' @param x A data frame of enrichment results.
#'
#' @return The same data frame with class `togoid_enrichment` added.
#' @keywords internal
as_togoid_enrichment <- function(x) {
  if (!inherits(x, "togoid_enrichment")) {
    class(x) <- unique(c("togoid_enrichment", class(x)))
  }
  x
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
  as_togoid_enrichment(result)
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
  as_togoid_enrichment(combined)
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
  as_togoid_enrichment(result)
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
    return(as_togoid_enrichment(result))
  }

  parts <- lapply(split(enrichment, enrichment$cluster), function(part) {
    part <- part[order(part$fdr, part$pvalue), , drop = FALSE]
    utils::head(part, n)
  })
  result <- do.call(rbind, parts)
  rownames(result) <- NULL
  as_togoid_enrichment(result)
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
#' \dontrun{
#' togoid_cluster_table(results, top_n = 3)
#' }
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

#' Write an enrichment result to a delimited text file
#'
#' Tabs rather than commas by default: term labels routinely contain commas,
#' which a CSV has to quote and some spreadsheet imports then mis-parse.
#'
#' @param enrichment An enrichment data frame.
#' @param path Destination file path.
#' @param sep Field separator; tab by default.
#'
#' @return The path, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' togoid_write_enrichment(results, "enrichment.tsv")
#' }
togoid_write_enrichment <- function(enrichment, path, sep = "\t") {
  directory <- dirname(path)
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  utils::write.table(
    as.data.frame(unclass(enrichment), stringsAsFactors = FALSE),
    file = path, sep = sep, quote = FALSE, row.names = FALSE, na = ""
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
