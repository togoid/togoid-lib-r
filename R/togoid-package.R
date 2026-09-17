#' @keywords internal
"_PACKAGE"

#' togoid: R Library for Biological Database ID Conversion and Annotation
#'
#' R library and CLI tool for biological database ID conversion and annotation
#' using TogoID (\url{https://togoid.dbcls.jp/}). Provides functions for
#' converting IDs between biological databases, retrieving annotations, and
#' converting labels to IDs.
#'
#' @section Main Classes:
#' \itemize{
#'   \item \code{\link{TogoIDConverter}}: Convert IDs between biological databases
#'   \item \code{\link{AnnotationsConverter}}: Get annotations and labels for IDs
#'   \item \code{\link{LabelConverter}}: Convert labels to IDs
#' }
#'
#' @section Enrichment Analysis:
#' \itemize{
#'   \item \code{\link{togoid_gene_sets}}: Turn any conversion route into gene sets
#'   \item \code{\link{togoid_enrich_clusters}}: Over-representation analysis per cluster
#'   \item \code{\link{togoid_plot_umap_enrichment}}: Draw the results on a UMAP
#' }
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{togoid_convert}}: Convert IDs between databases
#'   \item \code{\link{togoid_annotate}}: Get annotations for IDs
#'   \item \code{\link{togoid_label2id}}: Convert labels to IDs
#'   \item \code{\link{togoid_get_ortholog}}: Get orthologs
#'   \item \code{\link{togoid_search_databases}}: Search databases
#'   \item \code{\link{togoid_route}}: Find conversion routes
#' }
#'
#' @docType package
#' @name togoid-package
#' @aliases togoid
#'
#' @importFrom R6 R6Class
#' @importFrom rlang .data
NULL

## usethis namespace: start
## usethis namespace: end
NULL
