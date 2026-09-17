#' Ready-made routes for common annotation databases
#'
#' These are thin wrappers around [togoid_gene_sets()]. They exist for
#' convenience and as worked examples; any other TogoID route works exactly the
#' same way by passing `route = c(...)` directly.
#'
#' @name enrichment-presets
NULL

#' Named TogoID routes used by the enrichment presets
#'
#' `ncbigene` is the source in each case because gene symbols resolve to NCBI
#' Gene IDs through TogoID's label resolver.
#'
#' @return A named list of character vectors.
#' @export
#'
#' @examples
#' togoid_enrichment_routes()$reactome
togoid_enrichment_routes <- function() {
  list(
    reactome = c("ncbigene", "uniprot", "reactome_pathway"),
    go       = c("ncbigene", "uniprot", "go"),
    mondo    = c("ncbigene", "medgen", "mondo")
  )
}

#' Accepted GO aspects
#'
#' @return A character vector of the valid `go_aspect` values.
#' @export
togoid_go_aspects <- function() {
  c("biological_process", "molecular_function", "cellular_component")
}

#' Build Reactome pathway gene sets
#'
#' Route: `ncbigene -> uniprot -> reactome_pathway`.
#'
#' @param genes Character vector of gene symbols, or `route[1]` IDs when
#'   `id_source = NULL` is passed through `...`.
#' @param taxonomy Taxonomy ID for label resolution.
#' @param ... Further arguments passed to [togoid_gene_sets()].
#'
#' @return A `togoid_gene_sets` object.
#' @export
#'
#' @examples
#' \dontrun{
#' togoid_reactome_gene_sets(c("CD3D", "CD3E", "LCK", "ZAP70"))
#' }
togoid_reactome_gene_sets <- function(genes, taxonomy = "9606", ...) {
  togoid_gene_sets(
    genes,
    route = togoid_enrichment_routes()$reactome,
    taxonomy = taxonomy,
    ...
  )
}

#' Build Gene Ontology gene sets
#'
#' Route: `ncbigene -> uniprot -> go`.
#'
#' @param genes Character vector of gene symbols, or `route[1]` IDs when
#'   `id_source = NULL` is passed through `...`.
#' @param aspect Restrict to one GO aspect: `"biological_process"` (default),
#'   `"molecular_function"` or `"cellular_component"`. Use `NULL` to keep all.
#' @param taxonomy Taxonomy ID for label resolution.
#' @param ... Further arguments passed to [togoid_gene_sets()].
#'
#' @return A `togoid_gene_sets` object.
#' @export
#'
#' @examples
#' \dontrun{
#' togoid_go_gene_sets(c("CD3D", "CD3E", "LCK"), aspect = "biological_process")
#' }
togoid_go_gene_sets <- function(genes,
                                aspect = "biological_process",
                                taxonomy = "9606",
                                ...) {
  if (!is.null(aspect) && !(aspect %in% togoid_go_aspects())) {
    stop(sprintf(
      "aspect must be one of %s or NULL",
      paste(togoid_go_aspects(), collapse = ", ")
    ), call. = FALSE)
  }

  dots <- list(...)
  if (!is.null(aspect) && is.null(dots$term_filters)) {
    dots$term_filters <- list(go_aspect = aspect)
  }

  do.call(togoid_gene_sets, c(
    list(genes = genes, route = togoid_enrichment_routes()$go, taxonomy = taxonomy),
    dots
  ))
}

#' Build MONDO disease gene sets
#'
#' Route: `ncbigene -> medgen -> mondo`.
#'
#' Disease annotation covers far fewer genes than pathway annotation, so a lower
#' `min_set_size` is usually needed when testing these sets for enrichment.
#'
#' @param genes Character vector of gene symbols, or `route[1]` IDs when
#'   `id_source = NULL` is passed through `...`.
#' @param taxonomy Taxonomy ID for label resolution.
#' @param ... Further arguments passed to [togoid_gene_sets()].
#'
#' @return A `togoid_gene_sets` object.
#' @export
#'
#' @examples
#' \dontrun{
#' togoid_mondo_gene_sets(c("BRCA1", "TP53", "CD3D"))
#' }
togoid_mondo_gene_sets <- function(genes, taxonomy = "9606", ...) {
  togoid_gene_sets(
    genes,
    route = togoid_enrichment_routes()$mondo,
    taxonomy = taxonomy,
    ...
  )
}

#' Build gene sets from a preset name
#'
#' @param preset One of `"reactome"`, `"go"` or `"mondo"`.
#' @param genes Character vector of gene symbols.
#' @param ... Further arguments passed to the preset function.
#'
#' @return A `togoid_gene_sets` object.
#' @export
#'
#' @examples
#' \dontrun{
#' togoid_gene_sets_from_preset("reactome", c("CD3D", "CD3E", "LCK"))
#' }
togoid_gene_sets_from_preset <- function(preset, genes, ...) {
  builders <- list(
    reactome = togoid_reactome_gene_sets,
    go = togoid_go_gene_sets,
    mondo = togoid_mondo_gene_sets
  )
  if (!(preset %in% names(builders))) {
    stop(sprintf(
      paste0("Unknown preset '%s'. Available: %s. ",
             "For any other database, pass route = c(...) to togoid_gene_sets()."),
      preset, paste(names(builders), collapse = ", ")
    ), call. = FALSE)
  }
  builders[[preset]](genes, ...)
}
