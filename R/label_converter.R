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
    #' @param label_types Label types override (e.g., "symbol,synonym")
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
            label_type_values <- sapply(lt_list, function(x) x$label_type)
            label_types <- paste(label_type_values, collapse = ",")
          } else {
            label_types <- "symbol,synonym"
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
        if (is.null(dictionaries)) {
          label_resolver <- dataset_config$label_resolver %||% list()
          dictionaries <- label_resolver$dictionaries %||%
            paste0("togoid_", dataset, "_label")
        }

        if (self$verbose) {
          cli::cli_alert_info("Using PubDictionaries API for dataset: {dataset}")
        }

        results <- self$convert_pubdictionaries(
          labels = labels,
          dictionaries = dictionaries,
          tags = taxonomy,
          threshold = threshold
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
    #' @param labels Character vector of labels
    #' @param dictionaries Comma-separated dictionary names
    #' @param tags Taxonomy tags
    #' @param threshold Matching threshold (0-1)
    #' @param preferred_dictionary Preferred dictionary
    #'
    #' @return List of result lists
    convert_pubdictionaries = function(labels, dictionaries, tags = NULL,
                                       threshold = 0.5,
                                       preferred_dictionary = NULL) {

      if (self$verbose) {
        cli::cli_alert_info("Converting {length(labels)} labels using PubDictionaries")
        cli::cli_alert_info("Dictionaries: {dictionaries}")
      }

      # Build params
      params <- list(
        labels = paste(labels, collapse = "|"),
        dictionaries = dictionaries,
        verbose = "true"
      )

      if (!is.null(tags)) {
        params$tags <- tags
      }
      if (!is.null(threshold)) {
        params$threshold <- as.character(threshold)
      }

      # Make request
      url <- paste0(private$pubdict_base_url, "/find_ids.json")

      tryCatch(
        {
          req <- httr2::request(url)
          req <- httr2::req_url_query(req, !!!params)
          req <- httr2::req_timeout(req, 30)

          resp <- httr2::req_perform(req)
          find_ids_data <- httr2::resp_body_json(resp)

          # Process results
          results <- list()

          for (label in labels) {
            table_base_data <- find_ids_data[[label]] %||% list()

            if (length(table_base_data) == 0) {
              if (self$verbose) {
                cli::cli_alert_warning("No results for: {label}")
              }

              results[[length(results) + 1]] <- list(
                input = label,
                match_type = "Unmatched",
                identifier = NA_character_
              )
            } else {
              # Take first result
              first_result <- table_base_data[[1]]
              results[[length(results) + 1]] <- list(
                input = label,
                match_type = "PubDictionaries",
                identifier = first_result$identifier %||% NA_character_,
                score = first_result$score %||% NA_real_,
                dictionary = first_result$dictionary %||% NA_character_
              )
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
    #' @param label_types Comma-separated label types (e.g., "symbol,synonym")
    #' @param taxonomy Taxonomy ID
    #'
    #' @return List of result lists
    convert_sparqlist = function(labels, sparqlist, label_types = "symbol,synonym",
                                 taxonomy = NULL) {

      if (self$verbose) {
        cli::cli_alert_info("Converting {length(labels)} labels using SPARQList")
        cli::cli_alert_info("Endpoint: {sparqlist}")
        cli::cli_alert_info("Label types: {label_types}")
      }

      # Build params - use plural parameter names as per Python implementation
      params <- list(
        labels = paste(labels, collapse = ","),
        label_types = label_types
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
                identifier = NA_character_
              )
            } else {
              # Take first match
              first_match <- label_data[[1]]

              results[[length(results) + 1]] <- list(
                input = label,
                match_type = first_match$label_type %||% "exact",
                symbol = first_match$preferred %||% NA_character_,
                identifier = first_match$identifier %||% NA_character_,
                taxonomy = taxonomy
              )
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
              identifier = NA_character_,
              error = err_msg
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

      # Convert list of lists to data.frame
      df <- do.call(rbind, lapply(results, function(result) {
        as.data.frame(result, stringsAsFactors = FALSE)
      }))

      rownames(df) <- NULL
      return(df)
    }
  )
)
