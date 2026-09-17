#' Build gene sets from a TogoID conversion route
#'
#' A TogoID route that ends in an annotation dataset turns a plain gene list
#' into a gene-set library: every target term reached by the route becomes a set
#' containing the input genes that map to it. Because the route is a parameter,
#' the same code produces Reactome pathway sets, GO term sets, MONDO disease
#' sets, or anything else TogoID can reach.
#'
#' @name enrichment-genesets
NULL

#' Strip a CURIE prefix from an identifier
#'
#' Since togoid-api PR #149 the `/convert` endpoint can format IDs with their
#' dataset prefix (`0005634` becomes `GO:0005634`). Annotation lookups are keyed
#' by raw local IDs, so identifiers are normalised before being used as join
#' keys. The function is a no-op on unprefixed IDs, which keeps the code correct
#' both before and after that change is deployed.
#'
#' @param id Character vector of identifiers.
#'
#' @return The identifiers with any single leading prefix removed.
#' @keywords internal
local_id <- function(id) {
  if (!is.character(id)) {
    return(id)
  }
  # A URI (containing "//") is left alone; so is anything without a colon.
  prefixed <- grepl(":", id, fixed = TRUE) & !grepl("//", id, fixed = TRUE)
  id[prefixed] <- sub("^[^:]*:", "", id[prefixed])
  id
}

#' Split a vector into batches
#'
#' @param x Vector to split.
#' @param size Maximum batch size.
#'
#' @return A list of vectors.
#' @keywords internal
batch_vector <- function(x, size) {
  if (length(x) == 0) {
    return(list())
  }
  split(x, ceiling(seq_along(x) / size))
}

#' Print a progress message when verbose
#'
#' @param verbose Logical.
#' @param ... Passed to `cat()`.
#'
#' @return Invisibly `NULL`.
#' @keywords internal
enrich_log <- function(verbose, ...) {
  if (isTRUE(verbose)) {
    cat(..., "\n", sep = "")
  }
  invisible(NULL)
}

#' Resolve labels to database identifiers
#'
#' @param labels Character vector of labels, e.g. gene symbols.
#' @param dataset Target dataset for the resolved IDs (default `"ncbigene"`).
#' @param taxonomy Taxonomy ID required by the dataset's label resolver
#'   (`"9606"` for human).
#' @param batch_size Number of labels sent per request.
#' @param verbose Print progress.
#'
#' @return A named character vector mapping label to identifier. Labels that
#'   could not be resolved are absent.
#' @export
#'
#' @examples
#' \dontrun{
#' togoid_map_labels(c("CD3D", "MS4A1", "LYZ"))
#' }
togoid_map_labels <- function(labels,
                              dataset = "ncbigene",
                              taxonomy = "9606",
                              batch_size = 100,
                              verbose = TRUE) {
  labels <- unique(as.character(labels))
  resolved <- character(0)
  batches <- batch_vector(labels, batch_size)
  failures <- 0

  enrich_log(verbose, sprintf(
    "Resolving %d labels to '%s' IDs...", length(labels), dataset
  ))

  for (i in seq_along(batches)) {
    enrich_log(verbose, sprintf("  batch %d/%d (%d labels)",
                                i, length(batches), length(batches[[i]])))
    result <- tryCatch(
      togoid_label2id(
        labels = batches[[i]],
        dataset = dataset,
        taxonomy = taxonomy,
        format = "dataframe"
      ),
      error = function(e) {
        failures <<- failures + 1
        warning(sprintf("label resolution: batch %d failed: %s", i,
                        conditionMessage(e)), call. = FALSE)
        NULL
      }
    )

    if (is.null(result) || nrow(result) == 0) {
      next
    }
    if (!all(c("input", "identifier") %in% names(result))) {
      next
    }

    # The R resolver reports unmatched labels as rows with match_type
    # "Unmatched" and an NA identifier, rather than omitting them. Those are not
    # resolutions, and keeping them would give unmapped genes an NA ID.
    identifiers <- as.character(result$identifier)
    result <- result[!is.na(identifiers) & nzchar(identifiers), , drop = FALSE]
    if (nrow(result) == 0) {
      next
    }

    # Keep the first hit per label: the resolver returns exact symbol matches
    # before synonym matches.
    result <- result[!duplicated(result$input), , drop = FALSE]
    new_ids <- stats::setNames(as.character(result$identifier),
                               as.character(result$input))
    new_ids <- new_ids[!(names(new_ids) %in% names(resolved))]
    resolved <- c(resolved, new_ids)
  }

  if (failures > 0 && failures == length(batches)) {
    # Every request failed, so the empty mapping says nothing about the input.
    stop(sprintf(
      "Label resolution to '%s' failed for all %d batch(es).",
      dataset, length(batches)
    ), call. = FALSE)
  }

  enrich_log(verbose, sprintf("  resolved %d/%d labels",
                              length(resolved), length(labels)))
  resolved
}

#' Fetch annotation fields for a set of terms
#'
#' @param term_ids Character vector of term identifiers.
#' @param dataset TogoID dataset the terms belong to.
#' @param fields Character vector of fields to retrieve.
#' @param batch_size Number of IDs per query.
#' @param verbose Print progress.
#'
#' @return A data frame with an `id` column plus the requested fields, or `NULL`
#'   when nothing could be retrieved.
#' @keywords internal
fetch_term_annotations <- function(term_ids, dataset, fields,
                                   batch_size = 100, verbose = TRUE) {
  term_ids <- unique(as.character(term_ids))
  if (length(term_ids) == 0) {
    return(NULL)
  }

  # Only ask for fields the dataset exposes; an unknown field makes GRASP reject
  # the whole query, which would lose the labels too.
  available <- tryCatch({
    listed <- togoid_list_fields(dataset)
    column <- intersect(c("field_name", "field", "name"), names(listed))
    if (is.data.frame(listed) && length(column) > 0) {
      as.character(listed[[column[1]]])
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(available)) {
    kept <- intersect(fields, available)
    if (length(kept) > 0) {
      fields <- kept
    }
  }

  batches <- batch_vector(term_ids, batch_size)
  collected <- list()

  enrich_log(verbose, sprintf(
    "Fetching annotations for %d '%s' terms...", length(term_ids), dataset
  ))

  for (i in seq_along(batches)) {
    enrich_log(verbose, sprintf("  batch %d/%d (%d terms)",
                                i, length(batches), length(batches[[i]])))
    result <- tryCatch(
      togoid_annotate(
        dataset = dataset,
        ids = batches[[i]],
        fields = fields,
        format = "dataframe"
      ),
      error = function(e) {
        warning(sprintf("annotation of '%s': batch %d failed: %s", dataset, i,
                        conditionMessage(e)), call. = FALSE)
        NULL
      }
    )
    if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
      collected[[length(collected) + 1]] <- result
    }
  }

  if (length(collected) == 0) {
    return(NULL)
  }

  annotations <- do.call(rbind, collected)
  # One row per term: annotation fields can be multi-valued, and the first value
  # is enough for a display label.
  id_column <- if ("id" %in% names(annotations)) "id" else names(annotations)[1]
  names(annotations)[names(annotations) == id_column] <- "id"
  annotations <- annotations[!duplicated(annotations$id), , drop = FALSE]

  enrich_log(verbose, sprintf("  annotated %d/%d terms",
                              nrow(annotations), length(term_ids)))
  annotations
}

#' Build a gene-set library from a TogoID route
#'
#' The route's first dataset is the identifier space of the input genes, and its
#' last dataset supplies the terms that become gene sets. For example,
#' `c("ncbigene", "uniprot", "reactome_pathway")` turns NCBI Gene IDs into
#' Reactome pathway sets via UniProt.
#'
#' @param genes Character vector of gene symbols (when `id_source = "symbol"`)
#'   or identifiers already in the route's first dataset (`id_source = NULL`).
#' @param route Character vector of at least two dataset names.
#' @param id_source `"symbol"` to resolve labels first, or `NULL` when `genes`
#'   are already `route[1]` identifiers.
#' @param taxonomy Taxonomy ID used for label resolution.
#' @param label_dataset Dataset to take term labels from; defaults to the last
#'   element of `route`.
#' @param label_field Annotation field holding the term label.
#' @param term_filters Named list of allowed annotation values used to filter
#'   terms, e.g. `list(go_aspect = "biological_process")`.
#' @param batch_size Batch size for label resolution and annotation queries.
#' @param convert_batch_size Batch size for conversion requests, which return
#'   many rows per input ID and so use a smaller batch.
#' @param verbose Print progress.
#'
#' @return An object of class `togoid_gene_sets`: a list with `sets` (named list
#'   of character vectors), `labels`, `id_map`, `unmapped`, `route` and
#'   `target_dataset`.
#' @export
#'
#' @examples
#' \dontrun{
#' genes <- c("CD3D", "CD3E", "LCK", "ZAP70", "MS4A1", "CD79A")
#'
#' # Reactome pathways
#' togoid_gene_sets(genes, route = c("ncbigene", "uniprot", "reactome_pathway"))
#'
#' # GO terms - only the route changes
#' togoid_gene_sets(genes, route = c("ncbigene", "uniprot", "go"))
#' }
togoid_gene_sets <- function(genes,
                             route,
                             id_source = "symbol",
                             taxonomy = "9606",
                             label_dataset = NULL,
                             label_field = "label",
                             term_filters = NULL,
                             batch_size = 100,
                             convert_batch_size = 50,
                             verbose = TRUE) {
  route <- as.character(route)
  if (length(route) < 2) {
    stop("route must contain at least a source and a target dataset", call. = FALSE)
  }

  source_dataset <- route[1]
  target_dataset <- route[length(route)]
  genes <- unique(as.character(genes))

  enrich_log(verbose, strrep("=", 60))
  enrich_log(verbose, sprintf("Building gene sets: %s",
                              paste(route, collapse = " -> ")))
  enrich_log(verbose, strrep("=", 60))

  # Step 1: bring the input genes into the route's source ID space.
  if (is.null(id_source)) {
    id_map <- stats::setNames(genes, genes)
    unmapped <- character(0)
  } else {
    id_map <- togoid_map_labels(
      genes,
      dataset = source_dataset,
      taxonomy = taxonomy,
      batch_size = batch_size,
      verbose = verbose
    )
    unmapped <- setdiff(genes, names(id_map))
  }

  if (length(id_map) == 0) {
    enrich_log(verbose, "No genes could be resolved; returning an empty library.")
    return(new_togoid_gene_sets(
      sets = list(), labels = character(0), id_map = id_map,
      unmapped = unmapped, route = route, target_dataset = target_dataset
    ))
  }

  # One source ID can come from several input symbols (synonyms), so keep a
  # reverse index rather than assuming a one-to-one relation.
  source_ids <- local_id(unname(id_map))
  id_to_genes <- split(names(id_map), source_ids)
  unique_source_ids <- sort(unique(source_ids))

  # Step 2: walk the route to collect (source ID, term ID) pairs.
  batches <- batch_vector(unique_source_ids, convert_batch_size)
  pairs <- list()
  failures <- 0

  enrich_log(verbose, sprintf(
    "Converting %d '%s' IDs to '%s' terms...",
    length(unique_source_ids), source_dataset, target_dataset
  ))

  for (i in seq_along(batches)) {
    enrich_log(verbose, sprintf("  batch %d/%d (%d IDs)",
                                i, length(batches), length(batches[[i]])))
    converted <- tryCatch(
      togoid_convert(ids = batches[[i]], route = route, format = "dataframe"),
      error = function(e) {
        failures <<- failures + 1
        warning(sprintf("route %s: batch %d failed: %s",
                        paste(route, collapse = " -> "), i,
                        conditionMessage(e)), call. = FALSE)
        NULL
      }
    )

    if (is.null(converted) || !is.data.frame(converted) || nrow(converted) == 0) {
      next
    }

    src <- local_id(as.character(converted[[1]]))
    term <- local_id(as.character(converted[[ncol(converted)]]))
    keep <- !is.na(term) & nzchar(term) & term != "None" &
      !is.na(src) & nzchar(src)
    if (any(keep)) {
      pairs[[length(pairs) + 1]] <- data.frame(
        src = src[keep], term = term[keep], stringsAsFactors = FALSE
      )
    }
  }

  if (failures > 0 && failures == length(batches)) {
    # A broken route fails every batch. Returning an empty library would look
    # exactly like a gene list with no annotations, so fail loudly instead; the
    # API's own error usually names working alternative routes.
    stop(sprintf(
      "Conversion along route %s failed for all %d batch(es).",
      paste(route, collapse = " -> "), length(batches)
    ), call. = FALSE)
  }

  if (length(pairs) == 0) {
    return(new_togoid_gene_sets(
      sets = list(), labels = character(0), id_map = id_map,
      unmapped = unmapped, route = route, target_dataset = target_dataset
    ))
  }

  pairs <- do.call(rbind, pairs)
  pairs <- pairs[!duplicated(pairs), , drop = FALSE]

  # Map the source IDs back to the input gene spellings.
  members <- lapply(seq_len(nrow(pairs)), function(i) id_to_genes[[pairs$src[i]]])
  expanded <- data.frame(
    term = rep(pairs$term, lengths(members)),
    gene = unlist(members, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  expanded <- expanded[!is.na(expanded$gene), , drop = FALSE]
  sets <- lapply(split(expanded$gene, expanded$term), function(g) sort(unique(g)))

  enrich_log(verbose, sprintf("  built %d raw gene sets", length(sets)))

  # Step 3: annotate the terms with labels and any filter fields.
  filter_fields <- names(term_filters)
  annotations <- fetch_term_annotations(
    names(sets),
    dataset = if (is.null(label_dataset)) target_dataset else label_dataset,
    fields = unique(c(label_field, filter_fields)),
    batch_size = batch_size,
    verbose = verbose
  )

  labels <- character(0)
  if (!is.null(annotations) && label_field %in% names(annotations)) {
    labels <- stats::setNames(
      as.character(annotations[[label_field]]),
      as.character(annotations$id)
    )
    labels <- labels[!is.na(labels) & nzchar(labels)]
  }

  # Step 4: apply annotation-based term filters, e.g. restrict GO to one aspect.
  if (length(filter_fields) > 0 && !is.null(annotations)) {
    allowed <- rep(TRUE, nrow(annotations))
    for (field in filter_fields) {
      if (field %in% names(annotations)) {
        allowed <- allowed & as.character(annotations[[field]]) %in% term_filters[[field]]
      } else {
        allowed <- allowed & FALSE
      }
    }
    keep_terms <- as.character(annotations$id[allowed])
    dropped <- length(sets) - length(intersect(names(sets), keep_terms))
    sets <- sets[names(sets) %in% keep_terms]
    labels <- labels[names(labels) %in% keep_terms]
    enrich_log(verbose, sprintf("  term filter kept %d sets (dropped %d)",
                                length(sets), dropped))
  }

  result <- new_togoid_gene_sets(
    sets = sets, labels = labels, id_map = id_map,
    unmapped = unmapped, route = route, target_dataset = target_dataset
  )
  enrich_log(verbose, sprintf("Done: %d terms, %d genes",
                              length(sets), length(togoid_gene_set_genes(result))))
  result
}

#' Construct a togoid_gene_sets object
#'
#' @param sets Named list of character vectors.
#' @param labels Named character vector of term labels.
#' @param id_map Named character vector mapping input gene to source ID.
#' @param unmapped Character vector of genes that did not resolve.
#' @param route Character vector, the conversion route.
#' @param target_dataset Name of the dataset the terms come from.
#'
#' @return An object of class `togoid_gene_sets`.
#' @keywords internal
new_togoid_gene_sets <- function(sets, labels, id_map, unmapped, route, target_dataset) {
  structure(
    list(
      sets = sets,
      labels = labels,
      id_map = id_map,
      unmapped = unmapped,
      route = route,
      target_dataset = target_dataset
    ),
    class = "togoid_gene_sets"
  )
}

#' @export
print.togoid_gene_sets <- function(x, ...) {
  cat(sprintf(
    "<togoid_gene_sets> target: %s | terms: %d | genes: %d\n",
    x$target_dataset, length(x$sets), length(togoid_gene_set_genes(x))
  ))
  cat(sprintf("  route: %s\n", paste(x$route, collapse = " -> ")))
  if (length(x$unmapped) > 0) {
    cat(sprintf("  unmapped genes: %d\n", length(x$unmapped)))
  }
  invisible(x)
}

#' @export
length.togoid_gene_sets <- function(x) length(x$sets)

#' All genes in a gene-set library
#'
#' @param gene_sets A `togoid_gene_sets` object.
#'
#' @return Character vector of the genes appearing in at least one set.
#' @export
togoid_gene_set_genes <- function(gene_sets) {
  if (length(gene_sets$sets) == 0) {
    return(character(0))
  }
  sort(unique(unlist(gene_sets$sets, use.names = FALSE)))
}

#' Look up term labels
#'
#' @param gene_sets A `togoid_gene_sets` object.
#' @param term_ids Character vector of term identifiers.
#'
#' @return Character vector of labels, falling back to the term ID when unknown.
#' @export
togoid_term_labels <- function(gene_sets, term_ids) {
  labels <- unname(gene_sets$labels[term_ids])
  ifelse(is.na(labels), term_ids, labels)
}

#' Filter gene sets by size
#'
#' @param gene_sets A `togoid_gene_sets` object.
#' @param min_size Minimum number of genes (inclusive).
#' @param max_size Maximum number of genes (inclusive), or `NULL`.
#'
#' @return A new `togoid_gene_sets` object.
#' @export
togoid_filter_gene_sets <- function(gene_sets, min_size = 1, max_size = NULL) {
  sizes <- lengths(gene_sets$sets)
  keep <- sizes >= min_size
  if (!is.null(max_size)) {
    keep <- keep & sizes <= max_size
  }
  kept <- gene_sets$sets[keep]
  new_togoid_gene_sets(
    sets = kept,
    labels = gene_sets$labels[names(gene_sets$labels) %in% names(kept)],
    id_map = gene_sets$id_map,
    unmapped = gene_sets$unmapped,
    route = gene_sets$route,
    target_dataset = gene_sets$target_dataset
  )
}

#' Convert a gene-set library to a data frame
#'
#' @param x A `togoid_gene_sets` object.
#' @param row.names Ignored, present for S3 compatibility.
#' @param optional Ignored, present for S3 compatibility.
#' @param ... Ignored.
#'
#' @return A data frame with `term_id`, `term_label`, `n_genes` and `genes`,
#'   largest set first.
#' @export
as.data.frame.togoid_gene_sets <- function(x, row.names = NULL, optional = FALSE, ...) {
  if (length(x$sets) == 0) {
    return(data.frame(
      term_id = character(0), term_label = character(0),
      n_genes = integer(0), genes = character(0),
      stringsAsFactors = FALSE
    ))
  }
  term_ids <- names(x$sets)
  result <- data.frame(
    term_id = term_ids,
    term_label = togoid_term_labels(x, term_ids),
    n_genes = lengths(x$sets),
    genes = vapply(x$sets, paste, character(1), collapse = ","),
    stringsAsFactors = FALSE
  )
  result <- result[order(-result$n_genes, result$term_id), , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Save a gene-set library to JSON
#'
#' Building a library for a few thousand genes is many API calls; saving it lets
#' the analysis be reproduced exactly with no network access.
#'
#' @param gene_sets A `togoid_gene_sets` object.
#' @param path Destination file path.
#'
#' @return The path, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' togoid_save_gene_sets(library, "reactome.json")
#' }
togoid_save_gene_sets <- function(gene_sets, path) {
  directory <- dirname(path)
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  payload <- list(
    route = as.list(gene_sets$route),
    target_dataset = gene_sets$target_dataset,
    labels = as.list(gene_sets$labels),
    id_map = as.list(gene_sets$id_map),
    unmapped = as.list(gene_sets$unmapped),
    sets = lapply(gene_sets$sets, as.list)
  )
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE), path)
  invisible(path)
}

#' Load a gene-set library from JSON
#'
#' Reads a file written by [togoid_save_gene_sets()], including one written by
#' the Python library's `GeneSetLibrary.save_json()`.
#'
#' @param path Path to the JSON file.
#'
#' @return A `togoid_gene_sets` object.
#' @export
togoid_load_gene_sets <- function(path) {
  payload <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  as_named_character <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(character(0))
    }
    stats::setNames(vapply(x, as.character, character(1)), names(x))
  }

  new_togoid_gene_sets(
    sets = lapply(payload$sets %||% list(),
                  function(g) sort(unique(vapply(g, as.character, character(1))))),
    labels = as_named_character(payload$labels),
    id_map = as_named_character(payload$id_map),
    unmapped = unlist(payload$unmapped %||% list(), use.names = FALSE) %||% character(0),
    route = unlist(payload$route %||% list(), use.names = FALSE) %||% character(0),
    target_dataset = payload$target_dataset %||% ""
  )
}
