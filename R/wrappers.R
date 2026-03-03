#' Convert IDs between biological databases
#'
#' Wrapper function for TogoIDConverter$convert()
#'
#' @param ids Character vector of source IDs
#' @param route Character vector of database names forming the conversion route
#' @param format Output format: "json", "list", "table", "dataframe", "tibble"
#' @param ... Additional parameters passed to TogoIDConverter$convert()
#'
#' @return Converted IDs in specified format
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert ncbigene IDs to ensembl_gene IDs
#' result <- togoid_convert(
#'   ids = c("1", "9"),
#'   route = c("ncbigene", "ensembl_gene"),
#'   format = "dataframe"
#' )
#'
#' # Use pipe
#' c("1", "9") |>
#'   togoid_convert(route = c("ncbigene", "ensembl_gene"))
#' }
togoid_convert <- function(ids, route, format = "dataframe", ...) {
  converter <- TogoIDConverter$new()
  converter$convert(ids = ids, route = route, format = format, ...)
}

#' Get orthologs through round-trip conversion
#'
#' Wrapper function for TogoIDConverter$get_ortholog()
#'
#' @param ids Character vector of source IDs
#' @param route Character vector: c(source_db, intermediate_db)
#' @param target_taxids Character vector of target taxonomy IDs
#' @param format Output format
#'
#' @return Ortholog mapping in specified format
#' @export
#'
#' @examples
#' \dontrun{
#' # Get mouse orthologs for human genes
#' orthologs <- togoid_get_ortholog(
#'   ids = c("672", "7157"),
#'   route = c("ncbigene", "homologene"),
#'   target_taxids = c("10090"),
#'   format = "dataframe"
#' )
#' }
togoid_get_ortholog <- function(ids, route, target_taxids, format = "dataframe") {
  converter <- TogoIDConverter$new()
  converter$get_ortholog(ids = ids, route = route, target_taxids = target_taxids, format = format)
}

#' Get annotations for IDs
#'
#' Wrapper function for AnnotationsConverter$execute_query()
#'
#' @param dataset Dataset name
#' @param ids Character vector of IDs
#' @param fields Character vector of field names to retrieve
#' @param filters Named list of filters (e.g., list(go_aspect = c("molecular_function")))
#' @param format Output format: "list" or "dataframe" (default: "dataframe")
#'
#' @return Named list (id -> field -> value) or data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' # Get gene labels and synonyms
#' result <- togoid_annotate(
#'   dataset = "ncbigene",
#'   ids = c("672", "7157"),
#'   fields = c("label", "gene_synonym")
#' )
#'
#' # With filter
#' result <- togoid_annotate(
#'   dataset = "go",
#'   ids = c("GO:0003674", "GO:0008150"),
#'   fields = c("label", "go_aspect"),
#'   filters = list(go_aspect = c("molecular_function"))
#' )
#' }
togoid_annotate <- function(dataset, ids, fields, filters = list(), format = "dataframe") {
  annotator <- AnnotationsConverter$new()
  annotator$execute_query(
    dataset_name = dataset,
    ids = ids,
    fields = fields,
    filters = filters,
    format = format
  )
}

#' List available annotation fields for a dataset
#'
#' Wrapper function for AnnotationsConverter$list_fields()
#'
#' @param dataset Dataset name
#'
#' @return Data frame with field information
#' @export
#'
#' @examples
#' \dontrun{
#' # List fields for ncbigene
#' fields <- togoid_list_fields("ncbigene")
#' print(fields)
#' }
togoid_list_fields <- function(dataset) {
  annotator <- AnnotationsConverter$new()
  annotator$list_fields(dataset)
}

#' Convert labels to IDs
#'
#' Wrapper function for LabelConverter$convert()
#'
#' @param labels Character vector of labels to convert
#' @param dataset Dataset name (e.g., "ncbigene", "chebi")
#' @param taxonomy Taxonomy ID (e.g., "9606" for human)
#' @param format Output format: "list" or "dataframe" (default: "dataframe")
#' @param ... Additional parameters passed to LabelConverter$convert()
#'
#' @return List of result lists or data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert gene symbols to ncbigene IDs
#' result <- togoid_label2id(
#'   labels = c("BRCA1", "TP53", "EGFR"),
#'   dataset = "ncbigene",
#'   taxonomy = "9606"
#' )
#' }
togoid_label2id <- function(labels, dataset, taxonomy = NULL, format = "dataframe", ...) {
  converter <- LabelConverter$new()
  converter$convert(labels = labels, dataset = dataset, taxonomy = taxonomy, format = format, ...)
}

#' Search databases by name
#'
#' Wrapper function for TogoIDConverter$search_databases()
#'
#' @param name Search query string
#'
#' @return List of matching databases
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for databases containing "uniprot"
#' dbs <- togoid_search_databases("uniprot")
#' }
togoid_search_databases <- function(name) {
  converter <- TogoIDConverter$new()
  converter$search_databases(name)
}

#' Find routes between databases
#'
#' Wrapper function for TogoIDConverter$route()
#'
#' @param src Source database name
#' @param dst Destination database name
#' @param max_hops Maximum number of hops (default: 3)
#'
#' @return List of possible routes
#' @export
#'
#' @examples
#' \dontrun{
#' # Find routes from ncbigene to uniprot
#' routes <- togoid_route("ncbigene", "uniprot")
#' }
togoid_route <- function(src, dst, max_hops = 3) {
  converter <- TogoIDConverter$new()
  converter$route(src, dst, max_hops)
}

#' Get list of target datasets reachable from a source dataset
#'
#' Wrapper function for TogoIDConverter$config_list_targets()
#'
#' @param source Source dataset name (e.g., "ncbigene")
#'
#' @return Character vector of target dataset names
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all datasets reachable from ncbigene in one hop
#' targets <- togoid_config_list_targets("ncbigene")
#' print(targets)
#' }
togoid_config_list_targets <- function(source) {
  converter <- TogoIDConverter$new()
  converter$config_list_targets(source)
}
