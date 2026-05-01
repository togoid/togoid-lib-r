#' Label Converter Class
#'
#' R6 class for converting biological labels to database IDs
#' Supports both PubDictionaries and SPARQList APIs
#'
#' @field verbose Enable verbose logging
#' @field api_base_url TogoID API base URL
#'
#' @export
#' @importFrom R6 R6Class
LabelConverter <- R6::R6Class(
  "LabelConverter",

  public = list(
    #' @field verbose Verbose mode
    verbose = FALSE,

    #' @field api_base_url API base URL
    api_base_url = NULL,

    #' @description
    #' Initialize LabelConverter
    #'
    #' @param verbose Enable verbose logging
    #' @param api_base_url TogoID API base URL
    #'
    #' @return A new `LabelConverter` object
    initialize = function(verbose = FALSE, api_base_url = NULL) {
      self$verbose <- verbose

      default_url <- "https://api.togoid.dbcls.jp"
      self$api_base_url <- api_base_url %||%
        get_env_var("TOGOID_API_ENDPOINT", default_url)

      self$api_base_url <- sub("/$", "", self$api_base_url)

      private$dataset_cache <- NULL
      private$pubdict_base_url <- "https://pubdictionaries.org"
      private$sparqlist_base_url <- "https://dx.dbcls.jp/togoid/sparqlist/api"
    },

    #' @description
    #' Convert labels to IDs (auto-detects API)
    #'
    #' @param labels Character vector of labels to convert
    #' @param dataset Dataset name (e.g., "ncbigene", "chebi")
    #' @param taxonomy Taxonomy ID (e.g., "9606" for human)
    #' @param threshold Matching threshold for PubDictionaries (0-1)
    #' @param label_types Label types override as character vector
    #'   (e.g., c("symbol", "synonym")). Single comma-separated strings are
    #'   also accepted for backward compatibility but discouraged.
    #' @param dictionaries Dictionaries override for PubDictionaries
    #' @param format Output format: "list" or "dataframe" (default: "dataframe")
    #'
    #' @return List of result lists or data.frame
    convert = function(labels, dataset, taxonomy = NULL, threshold = 0.5,
                      label_types = NULL, dictionaries = NULL, format = "dataframe") {

      if (length(labels) == 0) {
        return(list())
      }

      # Get dataset configuration
      datasets <- private$get_dataset_config()

      if (!dataset %in% names(datasets)) {
        cli::cli_abort("Unknown dataset: {dataset}")
      }

      dataset_config <- datasets[[dataset]]

      # Check if taxonomy is required
      label_resolver <- dataset_config$label_resolver %||% list()
      taxonomy_required <- isTRUE(label_resolver$taxonomy)

      if (taxonomy_required && is.null(taxonomy)) {
        cli::cli_abort(c(
          "Taxonomy is required for dataset: {dataset}",
          "i" = "Please specify the 'taxonomy' parameter (e.g., taxonomy = '9606' for human)"
        ))
      }

      # Check if SPARQList should be used
      use_sparqlist <- private$should_use_sparqlist(dataset_config)

      # Get results based on API
      if (use_sparqlist) {
        label_resolver <- dataset_config$label_resolver
        sparqlist_endpoint <- label_resolver$sparqlist

        # Determine label types
        if (is.null(label_types)) {
          # Extract label_type values from label_types list
          lt_list <- label_resolver$label_types %||% list()
          if (length(lt_list) > 0) {
            label_types <- vapply(lt_list, function(x) x$label_type, character(1))
          } else {
            label_types <- c("symbol", "synonym")
          }
        } else {
          # Accept comma-joined strings for backward compatibility but split them
          if (length(label_types) == 1 && grepl(",", label_types)) {
            label_types <- trimws(strsplit(label_types, ",")[[1]])
          }
        }

        if (self$verbose) {
          cli::cli_alert_info("Using SPARQList API for dataset: {dataset}")
        }

        results <- self$convert_sparqlist(
          labels = labels,
          sparqlist = sparqlist_endpoint,
          label_types = label_types,
          taxonomy = taxonomy
        )

      } else {
        # Use PubDictionaries
        # Cache dict_list for later (used to map identifiers to label / preferred dict)
        label_resolver <- dataset_config$label_resolver %||% list()
        dict_list <- label_resolver$dictionaries %||% list()

        # Build dictionary -> label map and find preferred dictionary
        dictionary_to_label <- list()
        preferred_dictionary <- NULL
        preferred_label_column <- "name"
        if (is.list(dict_list) && length(dict_list) > 0 && is.list(dict_list[[1]])) {
          for (d in dict_list) {
            dname <- d$dictionary %||% ""
            if (nchar(dname) == 0) next
            dictionary_to_label[[dname]] <- d$label %||% dname
            if (isTRUE(d$preferred)) {
              preferred_dictionary <- dname
              preferred_label_column <- d$label %||% dname
            }
          }
        }

        if (is.null(dictionaries)) {
          if (is.list(dict_list) && length(dict_list) > 0 && is.list(dict_list[[1]])) {
            filtered_dicts <- dict_list
            # dict_list is a list of objects with 'dictionary' and 'label_type' fields
            if (!is.null(label_types)) {
              # Accept either a vector or a single comma-joined string
              if (is.character(label_types) && length(label_types) == 1 && grepl(",", label_types)) {
                requested_types <- trimws(strsplit(label_types, ",")[[1]])
              } else {
                requested_types <- as.character(label_types)
              }
              filtered_dicts <- Filter(function(d) {
                d_type <- d$label_type %||% ""
                d_type %in% requested_types
              }, dict_list)
            }
            dictionaries <- paste(sapply(filtered_dicts, function(d) d$dictionary), collapse = ",")
          } else if (is.character(dict_list)) {
            dictionaries <- paste(dict_list, collapse = ",")
          } else {
            dictionaries <- paste0("togoid_", dataset, "_label")
          }
        }

        if (self$verbose) {
          cli::cli_alert_info("Using PubDictionaries API for dataset: {dataset}")
        }

        results <- self$convert_pubdictionaries(
          labels = labels,
          dictionaries = dictionaries,
          tags = taxonomy,
          threshold = threshold,
          dictionary_to_label = dictionary_to_label,
          preferred_dictionary = preferred_dictionary,
          preferred_label_column = preferred_label_column
        )
      }

      # Convert to requested format
      if (format == "dataframe") {
        return(private$convert_to_dataframe(results))
      } else {
        return(results)
      }
    },

    #' @description
    #' Convert labels using PubDictionaries API
    #'
    #' Produces output in the same shape as the TogoID Web UI:
    #' columns are `input`, `match_type` (the human-readable dictionary
    #' label such as "Name" / "Exact synonym"), `<preferred_label_column>`
    #' (canonical label resolved through the preferred dictionary),
    #' `score`, `identifier`.
    #'
    #' @param labels Character vector of labels
    #' @param dictionaries Comma-separated dictionary names
    #' @param tags Taxonomy tags
    #' @param threshold Matching threshold (0-1)
    #' @param dictionary_to_label Named list mapping dictionary id -> human-readable label
    #' @param preferred_dictionary Dictionary id of the preferred (canonical-label) dictionary
    #' @param preferred_label_column Column name to use for the canonical label
    #'
    #' @return List of result lists
    convert_pubdictionaries = function(labels, dictionaries, tags = NULL,
                                       threshold = 0.5,
                                       dictionary_to_label = list(),
                                       preferred_dictionary = NULL,
                                       preferred_label_column = "name") {

      if (self$verbose) {
        cli::cli_alert_info("Converting {length(labels)} labels using PubDictionaries")
        cli::cli_alert_info("Dictionaries: {dictionaries}")
      }

      # Build params - all values must be character
      params <- list(
        labels = as.character(paste(labels, collapse = "|")),
        dictionaries = as.character(dictionaries),
        verbose = as.character("true"),
        use_ngram_similarity = as.character("true")
      )

      if (!is.null(tags)) {
        params$tags <- as.character(tags)
      }
      if (!is.null(threshold)) {
        params$threshold <- as.character(threshold)
      }

      # Make request
      url <- paste0(private$pubdict_base_url, "/find_ids.json")

      tryCatch(
        {
          req <- httr2::request(url)
          req <- httr2::req_url_query(req, .multi = "comma", !!!params)
          req <- httr2::req_timeout(req, 30)

          resp <- httr2::req_perform(req)
          find_ids_data <- httr2::resp_body_json(resp)

          # Collect identifiers from non-preferred dictionaries to look up canonical labels
          synonym_identifiers <- character()
          if (!is.null(preferred_dictionary)) {
            for (label in labels) {
              for (item in (find_ids_data[[label]] %||% list())) {
                d <- item$dictionary %||% ""
                if (nchar(d) > 0 && d != preferred_dictionary) {
                  synonym_identifiers <- c(synonym_identifiers, as.character(item$identifier %||% ""))
                }
              }
            }
            synonym_identifiers <- unique(synonym_identifiers[nzchar(synonym_identifiers)])
          }

          # Look up canonical labels for synonyms via find_terms (single batch request)
          canonical_lookup <- list()
          if (length(synonym_identifiers) > 0 && !is.null(preferred_dictionary)) {
            terms_url <- paste0(private$pubdict_base_url, "/find_terms.json")
            terms_params <- list(
              identifiers = paste(synonym_identifiers, collapse = "|"),
              dictionaries = preferred_dictionary
            )
            tryCatch({
              terms_req <- httr2::request(terms_url)
              terms_req <- httr2::req_url_query(terms_req, !!!terms_params)
              terms_req <- httr2::req_timeout(terms_req, 30)
              terms_resp <- httr2::req_perform(terms_req)
              terms_data <- httr2::resp_body_json(terms_resp)
              for (id_key in names(terms_data)) {
                entry <- terms_data[[id_key]]
                if (is.list(entry) && !is.null(entry$label)) {
                  canonical_lookup[[id_key]] <- as.character(entry$label)
                } else if (is.list(entry) && length(entry) > 0 && is.list(entry[[1]]) && !is.null(entry[[1]]$label)) {
                  canonical_lookup[[id_key]] <- as.character(entry[[1]]$label)
                }
              }
            }, error = function(e) {
              if (self$verbose) {
                cli::cli_alert_warning("find_terms lookup failed: {conditionMessage(e)}")
              }
            })
          }

          # Process results
          results <- list()

          for (label in labels) {
            table_base_data <- find_ids_data[[label]] %||% list()

            if (length(table_base_data) == 0) {
              if (self$verbose) {
                cli::cli_alert_warning("No results for: {label}")
              }

              row <- list(
                input = label,
                match_type = "Unmatched"
              )
              row[[preferred_label_column]] <- NA_character_
              row$score <- NA_real_
              row$identifier <- NA_character_
              results[[length(results) + 1]] <- row
            } else {
              # Add all results, not just the first one
              for (item in table_base_data) {
                dict_id <- item$dictionary %||% ""
                identifier_val <- as.character(item$identifier %||% NA)

                # Resolve human-readable match_type label
                match_label <- dictionary_to_label[[dict_id]] %||% dict_id
                if (length(match_label) == 0 || nchar(as.character(match_label)) == 0) {
                  match_label <- "PubDictionaries"
                }

                # Resolve canonical label via preferred dictionary lookup
                if (!is.null(preferred_dictionary) && dict_id == preferred_dictionary) {
                  canonical_label <- as.character(item$label %||% NA)
                } else if (!is.null(preferred_dictionary)) {
                  canonical_label <- canonical_lookup[[identifier_val]] %||% NA_character_
                } else {
                  canonical_label <- as.character(item$label %||% NA)
                }

                row <- list(
                  input = label,
                  match_type = as.character(match_label)
                )
                row[[preferred_label_column]] <- canonical_label
                row$score <- item$score %||% NA_real_
                row$identifier <- identifier_val
                results[[length(results) + 1]] <- row
              }
            }
          }

          return(results)
        },
        error = function(e) {
          cli::cli_abort(c(
            "PubDictionaries request failed",
            "x" = conditionMessage(e)
          ))
        }
      )
    },

    #' @description
    #' Convert labels using SPARQList API
    #'
    #' @param labels Character vector of labels
    #' @param sparqlist SPARQList endpoint name
    #' @param label_types Character vector of label types (e.g., c("symbol", "synonym"))
    #' @param taxonomy Taxonomy ID
    #'
    #' @return List of result lists
    convert_sparqlist = function(labels, sparqlist, label_types = c("symbol", "synonym"),
                                 taxonomy = NULL) {

      # Accept comma-joined strings for backward compatibility
      if (length(label_types) == 1 && grepl(",", label_types)) {
        label_types <- trimws(strsplit(label_types, ",")[[1]])
      }

      if (self$verbose) {
        cli::cli_alert_info("Converting {length(labels)} labels using SPARQList")
        cli::cli_alert_info("Endpoint: {sparqlist}")
        cli::cli_alert_info("Label types: {paste(label_types, collapse = ',')}")
      }

      # Build params - use plural parameter names as per Python implementation
      params <- list(
        labels = paste(labels, collapse = ","),
        label_types = paste(label_types, collapse = ",")
      )

      if (!is.null(taxonomy)) {
        params$taxon <- taxonomy
      }

      # Make request
      url <- paste0(private$sparqlist_base_url, "/", sparqlist)

      results <- list()

      tryCatch(
        {
          req <- httr2::request(url)
          req <- httr2::req_url_query(req, !!!params)
          req <- httr2::req_timeout(req, 30)

          if (self$verbose) {
            cli::cli_alert_info("Request URL: {req$url}")
          }

          resp <- httr2::req_perform(req)
          data <- httr2::resp_body_json(resp)

          # Process each label
          for (label in labels) {
            label_data <- data[[label]] %||% list()

            if (length(label_data) == 0) {
              if (self$verbose) {
                cli::cli_alert_warning("No results for: {label}")
              }

              results[[length(results) + 1]] <- list(
                input = label,
                match_type = "Unmatched",
                symbol = NA_character_,
                identifier = NA_character_,
                taxonomy = NA_character_
              )
            } else {
              # Add all matches, not just the first one
              for (match in label_data) {
                results[[length(results) + 1]] <- list(
                  input = label,
                  match_type = match$label_type %||% "exact",
                  symbol = match$preferred %||% NA_character_,
                  identifier = match$identifier %||% NA_character_,
                  taxonomy = taxonomy
                )
              }
            }
          }
        },
        error = function(e) {
          err_msg <- conditionMessage(e)
          warning(sprintf("SPARQList request failed - %s", err_msg))

          # Add error entries for all labels
          for (label in labels) {
            results[[length(results) + 1]] <<- list(
              input = label,
              match_type = "Error",
              symbol = NA_character_,
              identifier = NA_character_,
              taxonomy = NA_character_
            )
          }
        }
      )

      return(results)
    }
  ),

  private = list(
    #' Cached dataset configuration
    dataset_cache = NULL,

    #' PubDictionaries base URL
    pubdict_base_url = NULL,

    #' SPARQList base URL
    sparqlist_base_url = NULL,

    #' Get dataset configuration from API
    #'
    #' @return Dataset configuration list
    get_dataset_config = function() {
      if (!is.null(private$dataset_cache)) {
        return(private$dataset_cache)
      }

      if (self$verbose) {
        cli::cli_alert_info("Fetching dataset config from TogoID API")
      }

      response <- make_request(
        base_url = self$api_base_url,
        endpoint = "config/dataset",
        method = "GET"
      )

      private$dataset_cache <- response
      return(response)
    },

    #' Check if SPARQList should be used for dataset
    #'
    #' @param dataset_config Dataset configuration
    #'
    #' @return TRUE if SPARQList should be used
    should_use_sparqlist = function(dataset_config) {
      label_resolver <- dataset_config$label_resolver %||% list()
      return(!is.null(label_resolver$sparqlist))
    },

    #' Convert results list to data.frame
    #'
    #' @param results List of result lists
    #'
    #' @return data.frame
    convert_to_dataframe = function(results) {
      if (length(results) == 0) {
        return(data.frame())
      }

      # Convert list of lists to data.frame using bind_rows for robustness
      dfs <- lapply(results, function(result) {
        as.data.frame(result, stringsAsFactors = FALSE)
      })
      df <- dplyr::bind_rows(dfs)

      rownames(df) <- NULL
      return(df)
    }
  )
)
