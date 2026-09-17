#' Adapters between single-cell toolkits and the enrichment functions
#'
#' The enrichment code deliberately knows nothing about Seurat: it takes a plain
#' embedding data frame and a named list of gene vectors. These helpers do the
#' translation, and load Seurat only when called.
#'
#' @name enrichment-adapters
NULL

#' Extract a plotting-ready embedding from a Seurat object
#'
#' @param object A Seurat object with a computed reduction.
#' @param reduction Name of the reduction to use (default `"umap"`).
#' @param cluster_column Column in `object@meta.data` holding cluster labels, or
#'   `NULL` to use the object's active identities.
#'
#' @return A data frame with `umap_1`, `umap_2` and `cluster`, one row per cell.
#' @export
#'
#' @examples
#' \dontrun{
#' embedding <- togoid_umap_from_seurat(pbmc)
#' }
togoid_umap_from_seurat <- function(object, reduction = "umap", cluster_column = NULL) {
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Seurat is required for this adapter. Install with: install.packages(\"Seurat\")",
         call. = FALSE)
  }

  coordinates <- Seurat::Embeddings(object, reduction = reduction)
  if (ncol(coordinates) < 2) {
    stop(sprintf("reduction '%s' has fewer than 2 dimensions", reduction), call. = FALSE)
  }

  clusters <- if (is.null(cluster_column)) {
    as.character(Seurat::Idents(object))
  } else {
    if (!(cluster_column %in% names(object@meta.data))) {
      stop(sprintf("meta.data has no column '%s'", cluster_column), call. = FALSE)
    }
    as.character(object@meta.data[[cluster_column]])
  }

  result <- data.frame(
    umap_1 = as.numeric(coordinates[, 1]),
    umap_2 = as.numeric(coordinates[, 2]),
    cluster = clusters,
    stringsAsFactors = FALSE
  )
  rownames(result) <- rownames(coordinates)
  result
}

#' Extract per-cluster marker genes from FindAllMarkers output
#'
#' @param markers Data frame returned by `Seurat::FindAllMarkers()`, or any data
#'   frame with the same column names.
#' @param cluster_column Column holding the cluster label.
#' @param gene_column Column holding the gene symbol.
#' @param pval_column Column holding the adjusted p-value, or `NULL` to skip.
#' @param logfc_column Column holding the log fold change, or `NULL` to skip.
#' @param pval_cutoff Drop rows at or above this p-value.
#' @param logfc_min Drop rows below this log fold change.
#' @param top_n Keep at most this many genes per cluster; `NULL` keeps all.
#'
#' @return A named list mapping cluster label to its marker gene symbols, most
#'   significant first.
#' @export
#'
#' @examples
#' \dontrun{
#' markers <- Seurat::FindAllMarkers(pbmc, only.pos = TRUE)
#' gene_lists <- togoid_markers_from_seurat(markers)
#' }
togoid_markers_from_seurat <- function(markers,
                                       cluster_column = "cluster",
                                       gene_column = "gene",
                                       pval_column = "p_val_adj",
                                       logfc_column = "avg_log2FC",
                                       pval_cutoff = 0.05,
                                       logfc_min = 0.25,
                                       top_n = 100) {
  missing <- setdiff(c(cluster_column, gene_column), names(markers))
  if (length(missing) > 0) {
    stop(sprintf(
      "markers is missing column(s): %s. Found: %s",
      paste(missing, collapse = ", "), paste(names(markers), collapse = ", ")
    ), call. = FALSE)
  }

  filtered <- markers
  if (!is.null(pval_column) && !is.null(pval_cutoff) && pval_column %in% names(filtered)) {
    filtered <- filtered[!is.na(filtered[[pval_column]]) &
                           filtered[[pval_column]] < pval_cutoff, , drop = FALSE]
  }
  if (!is.null(logfc_column) && !is.null(logfc_min) && logfc_column %in% names(filtered)) {
    filtered <- filtered[!is.na(filtered[[logfc_column]]) &
                           filtered[[logfc_column]] >= logfc_min, , drop = FALSE]
  }
  if (nrow(filtered) == 0) {
    return(list())
  }

  if (!is.null(pval_column) && pval_column %in% names(filtered)) {
    filtered <- filtered[order(filtered[[pval_column]]), , drop = FALSE]
  }

  parts <- split(as.character(filtered[[gene_column]]),
                 as.character(filtered[[cluster_column]]))
  lapply(parts, function(genes) {
    genes <- unique(genes)
    if (is.null(top_n)) genes else utils::head(genes, top_n)
  })
}

#' Load an embedding exported as CSV
#'
#' Useful when the clustering was done elsewhere, for example in scanpy.
#'
#' @param path Path to the CSV file.
#' @param x_column Column holding the first dimension.
#' @param y_column Column holding the second dimension.
#' @param cluster_column Column holding the cluster label.
#'
#' @return A data frame with `umap_1`, `umap_2` and `cluster`.
#' @export
togoid_umap_from_csv <- function(path,
                                 x_column = "umap_1",
                                 y_column = "umap_2",
                                 cluster_column = "cluster") {
  table <- utils::read.csv(path, stringsAsFactors = FALSE)
  normalise_embedding(table, x_column, y_column, cluster_column)
}

#' Load per-cluster marker genes from a long-format CSV
#'
#' @param path Path to a CSV with a cluster column and a gene column.
#' @param cluster_column Column holding the cluster label.
#' @param gene_column Column holding the gene symbol.
#' @param ... Further arguments passed to [togoid_markers_from_seurat()].
#'
#' @return A named list mapping cluster label to its marker gene symbols.
#' @export
togoid_markers_from_csv <- function(path,
                                    cluster_column = "cluster",
                                    gene_column = "gene",
                                    ...) {
  table <- utils::read.csv(path, stringsAsFactors = FALSE)
  togoid_markers_from_seurat(
    table,
    cluster_column = cluster_column,
    gene_column = gene_column,
    ...
  )
}

#' Write one plain-text gene list per cluster
#'
#' @param markers Named list mapping cluster label to gene symbols.
#' @param directory Destination directory, created if needed.
#' @param prefix File name prefix; files are `<prefix><cluster>_markers.txt`.
#'
#' @return A named character vector of the paths written, invisibly.
#' @export
togoid_write_marker_lists <- function(markers, directory, prefix = "cluster_") {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  paths <- vapply(names(markers), function(cluster) {
    path <- file.path(directory, sprintf("%s%s_markers.txt", prefix, cluster))
    writeLines(markers[[cluster]], path)
    path
  }, character(1))
  invisible(paths)
}
