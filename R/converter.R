#' TogoID Converter Class
#'
#' R6 class for ID conversion between biological databases using TogoID API
#'
#' @field api_base_url Base URL for the TogoID API
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble as_tibble
TogoIDConverter <- R6::R6Class(
  "TogoIDConverter",

  public = list(
    #' @field api_base_url API base URL
    api_base_url = NULL,

    #' @description
    #' Initialize TogoIDConverter
    #'
    #' @param api_base_url Base URL for API endpoint. If NULL, uses default TogoID API or TOGOID_API_ENDPOINT env var.
    #'
    #' @return A new `TogoIDConverter` object
    initialize = function(api_base_url = NULL) {
      default_url <- "https://api.togoid.dbcls.jp"
      self$api_base_url <- api_base_url %||%
        get_env_var("TOGOID_API_ENDPOINT", default_url)
      self$api_base_url <- sub("/$", "", self$api_base_url)
    },

    #' @description
    #' Convert IDs between databases
    #'
    #' @param ids Character vector of source IDs
    #' @param route Character vector of database names forming the conversion route
    #' @param format Output format: "json", "list", "table", "dataframe", "tibble"
    #' @param report Report type: "target", "pair", "full" (default: "full")
    #' @param annotate List of lists: list(list("dataset", "field")) to add annotation columns
    #' @param filter List of lists: list(list("dataset", "field", c("values"))) to filter results
    #' @param ... Additional parameters passed to API
    #'
    #' @return Converted IDs in specified format
    convert = function(ids, route, format = "dataframe", report = "full",
                      annotate = NULL, filter = NULL, ...) {

      # Validate inputs
      if (length(ids) == 0) {
        cli::cli_abort("IDs cannot be empty")
      }
      if (length(route) < 2) {
        cli::cli_abort("Route must contain at least 2 databases")
      }

      # Build API parameters
      params <- list(
        ids = paste(ids, collapse = ","),
        route = paste(route, collapse = ","),
        report = report,
        ...
      )

      # Make API request
      response <- make_request(
        base_url = self$api_base_url,
        endpoint = "convert",
        method = "GET",
        params = params
      )

      # Handle annotations and filters (if needed)
      if (!is.null(annotate) || !is.null(filter)) {
        response <- private$add_annotations(response, route, annotate, filter)
      }

      # Convert to requested format
      return(private$format_response(response, format, route, ids, annotate))
    },

    #' @description
    #' Get orthologs through round-trip conversion
    #'
    #' @param ids Character vector of source IDs
    #' @param route Character vector: c(source_db, intermediate_db)
    #' @param target_taxids Character vector of target taxonomy IDs
    #' @param format Output format
    #'
    #' @return Ortholog mapping in specified format
    get_ortholog = function(ids, route, target_taxids, format = "dataframe") {

      if (length(route) != 2) {
        cli::cli_abort("Route for ortholog must have exactly 2 databases")
      }

      # Build round-trip route: source -> intermediate -> source -> taxonomy
      full_route <- c(route[1], route[2], route[1], "taxonomy")

      # Internal function to get raw table data from API
      get_table_data <- function(id_vec) {
        params <- list(
          ids = paste(id_vec, collapse = ","),
          route = paste(full_route, collapse = ","),
          report = "full"
        )
        response <- make_request(
          base_url = self$api_base_url,
          endpoint = "convert",
          method = "GET",
          params = params
        )
        private$convert_to_table(response)
      }

      # Try batch conversion, fall back to individual IDs on error
      table_data <- tryCatch({
        get_table_data(ids)
      }, error = function(e) {
        # Fallback: convert one ID at a time
        all_results <- list()
        for (id in ids) {
          single <- tryCatch({
            get_table_data(id)
          }, error = function(e2) {
            list()
          })
          all_results <- c(all_results, single)
        }
        all_results
      })

      # Track which input IDs had any API results
      all_source_ids <- character()
      for (row in table_data) {
        if (is.list(row) && length(row) >= 1) {
          all_source_ids <- c(all_source_ids, as.character(row[[1]]))
        }
      }

      # Filter by target taxonomy IDs
      filtered <- list()
      found_ids <- character()
      if (length(table_data) > 0) {
        for (row in table_data) {
          if (is.list(row) && length(row) >= 4) {
            if (as.character(row[[4]]) %in% target_taxids) {
              filtered[[length(filtered) + 1]] <- row
              found_ids <- c(found_ids, as.character(row[[1]]))
            }
          }
        }
      }

      # Add NA rows for IDs that had no conversion results at all
      missing_ids <- setdiff(ids, unique(all_source_ids))
      for (mid in missing_ids) {
        filtered[[length(filtered) + 1]] <- list(mid, NA_character_, NA_character_, NA_character_)
      }

      return(private$format_ortholog_result(filtered, format))
    },

    #' @description
    #' Search databases by name
    #'
    #' @param name Search query string
    #'
    #' @return List of matching databases
    search_databases = function(name) {
      result <- make_request(
        base_url = self$api_base_url,
        endpoint = paste0("search/databases/", name),
        method = "GET"
      )
      return(result)
    },

    #' @description
    #' Search for databases containing an ID pattern
    #'
    #' @param id_string ID pattern to search
    #'
    #' @return List of matching databases
    search_id = function(id_string) {
      result <- make_request(
        base_url = self$api_base_url,
        endpoint = paste0("search/id/", id_string),
        method = "GET"
      )
      return(result)
    },

    #' @description
    #' Lookup which tables contain a specific ID
    #'
    #' @param id_string ID to lookup
    #'
    #' @return List of tables containing the ID
    lookup_id = function(id_string) {
      result <- make_request(
        base_url = self$api_base_url,
        endpoint = paste0("lookup/", id_string),
        method = "GET"
      )
      return(result)
    },

    #' @description
    #' Find conversion routes between databases
    #'
    #' @param src Source database name
    #' @param dst Destination database name
    #' @param max_hops Maximum number of hops (default: 3)
    #'
    #' @return List of possible routes
    route = function(src, dst, max_hops = 3) {
      params <- list(max_hops = max_hops)
      result <- make_request(
        base_url = self$api_base_url,
        endpoint = paste0("route/", src, "/", dst),
        method = "GET",
        params = params
      )
      return(result)
    },

    #' @description
    #' Count mappings between databases
    #'
    #' @param src Source database name
    #' @param dst Destination database name
    #' @param ids Optional character vector of IDs to count
    #' @param link Optional link parameter
    #'
    #' @return Count information
    count = function(src, dst, ids = NULL, link = NULL) {
      params <- list()
      if (!is.null(ids)) {
        params$ids <- paste(ids, collapse = ",")
      }
      if (!is.null(link)) {
        params$link <- link
      }

      result <- make_request(
        base_url = self$api_base_url,
        endpoint = paste0("count/", src, "-", dst),
        method = "GET",
        params = params
      )
      return(result)
    },

    #' @description
    #' Get dataset configuration
    #'
    #' @param name Optional dataset name. If NULL, returns all datasets.
    #'
    #' @return Dataset configuration
    config_dataset = function(name = NULL) {
      endpoint <- if (is.null(name)) {
        "config/dataset"
      } else {
        paste0("config/dataset/", name)
      }

      result <- make_request(
        base_url = self$api_base_url,
        endpoint = endpoint,
        method = "GET"
      )
      return(result)
    },

    #' @description
    #' Get relation configuration
    #'
    #' @param src Optional source database name
    #' @param dst Optional destination database name
    #'
    #' @return Relation configuration
    config_relation = function(src = NULL, dst = NULL) {
      endpoint <- if (is.null(src) && is.null(dst)) {
        "config/relation"
      } else if (!is.null(src) && !is.null(dst)) {
        paste0("config/relation/", src, "-", dst)
      } else {
        cli::cli_abort("Both src and dst must be provided, or both NULL")
      }

      result <- make_request(
        base_url = self$api_base_url,
        endpoint = endpoint,
        method = "GET"
      )
      return(result)
    },

    #' @description
    #' Get database descriptions
    #'
    #' @return Database descriptions
    config_descriptions = function() {
      result <- make_request(
        base_url = self$api_base_url,
        endpoint = "config/descriptions",
        method = "GET"
      )
      return(result)
    },

    #' @description
    #' Get database statistics
    #'
    #' @return Database statistics
    config_statistics = function() {
      result <- make_request(
        base_url = self$api_base_url,
        endpoint = "config/statistics",
        method = "GET"
      )
      return(result)
    },

    #' @description
    #' Get taxonomy list
    #'
    #' @return Taxonomy list
    config_taxonomy = function() {
      result <- make_request(
        base_url = self$api_base_url,
        endpoint = "config/taxonomy",
        method = "GET"
      )
      return(result)
    },

    #' @description
    #' Get list of datasets reachable from the specified source dataset in one hop
    #'
    #' @param source Source dataset name (e.g., "ncbigene")
    #'
    #' @return Character vector of target dataset names
    #'
    #' @examples
    #' \dontrun{
    #' converter <- TogoIDConverter$new()
    #' targets <- converter$config_list_targets(source = "ncbigene")
    #' print(targets)
    #' }
    config_list_targets = function(source) {
      # Get all relation configurations
      relations <- self$config_relation()

      targets <- character()

      # Parse each relation key
      for (relation_key in names(relations)) {
        # Split key by "-"
        parts <- strsplit(relation_key, "-")[[1]]

        # Check if it's a valid format (2 parts)
        if (length(parts) == 2) {
          src <- parts[1]
          dst <- parts[2]

          # If source matches (forward link), add target to list
          if (src == source) {
            targets <- c(targets, dst)
          }
          # If destination matches (reverse link), add source to list
          if (dst == source) {
            targets <- c(targets, src)
          }
        }
      }

      # Remove duplicates and sort
      return(sort(unique(targets)))
    }
  ),

  private = list(
    #' Format response to requested output format
    #'
    #' @param response API response
    #' @param format Requested format
    #' @param route Conversion route
    #' @param ids Source IDs
    #'
    #' @return Formatted response
    format_response = function(response, format, route, ids, annotate = NULL) {
      format <- normalize_format(format)

      expanded_route <- private$build_expanded_route(route, annotate)

      switch(format,
        "list" = return(private$convert_to_list(response, route, ids)),
        "dataframe" = return(private$convert_to_dataframe(response, route, expanded_route)),
        "tibble" = return(private$convert_to_tibble(response, route, expanded_route)),
        return(private$convert_to_dataframe(response, route, expanded_route)) # default
      )
    },

    #' Convert response to list format
    #'
    #' @param response API response
    #' @param route Conversion route
    #' @param ids Source IDs
    #'
    #' @return List with ids, route, and results
    convert_to_list = function(response, route, ids) {
      list(
        ids = ids,
        route = route,
        results = response
      )
    },

    #' Convert response to table (list of vectors)
    #'
    #' @param response API response
    #'
    #' @return List of character vectors
    convert_to_table = function(response) {
      if (is.list(response) && "results" %in% names(response)) {
        return(response$results)
      }
      return(response)
    },

    #' Convert response to data.frame
    #'
    #' @param response API response
    #' @param route Conversion route
    #'
    #' @return data.frame
    convert_to_dataframe = function(response, route, expanded_route = NULL) {
      table_data <- private$convert_to_table(response)

      if (length(table_data) == 0) {
        col_names <- if (!is.null(expanded_route)) expanded_route else route
        empty_df <- as.data.frame(matrix(ncol = length(col_names), nrow = 0))
        colnames(empty_df) <- col_names
        return(empty_df)
      }

      # Convert to data.frame
      if (is.list(table_data) && length(table_data) > 0) {
        # Replace NULL values with NA
        table_data_clean <- lapply(table_data, function(row) {
          lapply(row, function(elem) {
            if (is.null(elem)) NA_character_ else as.character(elem)
          })
        })

        # Determine the maximum length across all rows
        max_length <- max(sapply(table_data_clean, length))

        # Pad all rows to the maximum length
        table_data_padded <- lapply(table_data_clean, function(row) {
          if (length(row) < max_length) {
            c(row, rep(list(NA_character_), max_length - length(row)))
          } else {
            row
          }
        })

        # Convert each row to a data.frame row
        df <- do.call(rbind, lapply(table_data_padded, function(row) {
          as.data.frame(t(unlist(row)), stringsAsFactors = FALSE)
        }))

        # Create column names: prefer expanded_route if it matches
        ncols <- ncol(df)
        if (!is.null(expanded_route) && ncols == length(expanded_route)) {
          col_names <- expanded_route
        } else if (ncols == length(route)) {
          col_names <- route
        } else if (ncols < length(route)) {
          col_names <- route[(length(route) - ncols + 1):length(route)]
        } else {
          col_names <- c(route, paste0("col_", seq_len(ncols - length(route))))
        }

        colnames(df) <- col_names
        rownames(df) <- NULL
        return(df)
      }

      return(data.frame())
    },

    #' Convert response to tibble
    #'
    #' @param response API response
    #' @param route Conversion route
    #'
    #' @return tibble
    convert_to_tibble = function(response, route, expanded_route = NULL) {
      df <- private$convert_to_dataframe(response, route, expanded_route)
      return(tibble::as_tibble(df))
    },

    #' Format ortholog result
    #'
    #' @param filtered Filtered results
    #' @param format Output format
    #'
    #' @return Formatted ortholog result
    format_ortholog_result = function(filtered, format) {
      if (format == "dataframe" || format == "tibble") {
        if (length(filtered) == 0) {
          df <- data.frame(
            source_id = character(),
            intermediate_id = character(),
            target_id = character(),
            taxonomy_id = character(),
            stringsAsFactors = FALSE
          )
        } else {
          df <- do.call(rbind, lapply(filtered, function(row) {
            data.frame(
              source_id = as.character(row[[1]]),
              intermediate_id = as.character(row[[2]]),
              target_id = as.character(row[[3]]),
              taxonomy_id = as.character(row[[4]]),
              stringsAsFactors = FALSE
            )
          }))
        }

        if (format == "tibble") {
          return(tibble::as_tibble(df))
        }
        return(df)
      }

      return(filtered)
    },

    #' Build expanded route with annotation column names
    #'
    #' @param route Original conversion route
    #' @param annotate Annotation specifications
    #'
    #' @return Expanded route vector with annotation column names inserted
    build_expanded_route = function(route, annotate) {
      if (is.null(annotate)) return(route)

      # Group annotations by position in route
      insertions <- list()
      for (ann_spec in annotate) {
        dataset_name <- ann_spec[[1]]
        field_name <- ann_spec[[2]]
        pos <- which(route == dataset_name)[1]
        if (is.na(pos)) next
        key <- as.character(pos)
        if (is.null(insertions[[key]])) insertions[[key]] <- character()
        insertions[[key]] <- c(insertions[[key]], paste0(dataset_name, ".", field_name))
      }

      # Build expanded route
      expanded <- character()
      for (i in seq_along(route)) {
        expanded <- c(expanded, route[i])
        key <- as.character(i)
        if (key %in% names(insertions)) {
          expanded <- c(expanded, insertions[[key]])
        }
      }
      return(expanded)
    },

    #' Add annotations to conversion results
    #'
    #' @param response API response
    #' @param route Conversion route
    #' @param annotate Annotation specifications: list(list("dataset", "field"), ...)
    #' @param filter Filter specifications: list(list("dataset", "field", c("values")), ...)
    #'
    #' @return Response with annotations
    add_annotations = function(response, route, annotate, filter) {
      # Extract table data from response
      if (is.list(response) && "results" %in% names(response)) {
        table_data <- response$results
      } else {
        table_data <- private$convert_to_table(response)
      }

      if (length(table_data) == 0) {
        return(response)
      }

      # Initialize annotations converter
      ann_converter <- AnnotationsConverter$new(api_endpoint = self$api_base_url)

      # Build a mapping of dataset -> set of fields
      fields_to_fetch <- list()

      # Collect fields from annotate parameter
      if (!is.null(annotate)) {
        for (ann_spec in annotate) {
          dataset_name <- ann_spec[[1]]
          field_name <- ann_spec[[2]]

          if (!dataset_name %in% names(fields_to_fetch)) {
            fields_to_fetch[[dataset_name]] <- character()
          }
          fields_to_fetch[[dataset_name]] <- unique(c(fields_to_fetch[[dataset_name]], field_name))
        }
      }

      # Collect fields from filter parameter
      if (!is.null(filter)) {
        for (filter_spec in filter) {
          dataset_name <- filter_spec[[1]]
          field_name <- filter_spec[[2]]

          if (!dataset_name %in% names(fields_to_fetch)) {
            fields_to_fetch[[dataset_name]] <- character()
          }
          fields_to_fetch[[dataset_name]] <- unique(c(fields_to_fetch[[dataset_name]], field_name))
        }
      }

      # Fetch all required annotations
      annotations_cache <- list()

      for (dataset_name in names(fields_to_fetch)) {
        if (!dataset_name %in% route) {
          cli::cli_abort("Dataset '{dataset_name}' not found in route {route}")
        }

        # Collect all IDs for this dataset
        dataset_index <- which(route == dataset_name)[1]
        ids_to_annotate <- character()

        for (row in table_data) {
          if (is.list(row) && length(row) >= dataset_index) {
            elem <- row[[dataset_index]]
            if (!is.null(elem) && length(elem) > 0) {
              id_value <- as.character(elem)
              if (!is.na(id_value) && nchar(id_value) > 0) {
                ids_to_annotate <- c(ids_to_annotate, id_value)
              }
            }
          }
        }

        ids_to_annotate <- unique(ids_to_annotate)

        if (length(ids_to_annotate) == 0) {
          next
        }

        # Execute annotation query
        tryCatch({
          records <- ann_converter$execute_query(
            dataset_name = dataset_name,
            ids = ids_to_annotate,
            fields = fields_to_fetch[[dataset_name]],
            filters = list(),
            format = "list"
          )

          # Cache the results
          annotations_cache[[dataset_name]] <- records

        }, error = function(e) {
          cli::cli_alert_warning("Failed to get annotations for {dataset_name}: {conditionMessage(e)}")
        })
      }

      # Apply filters first, then add annotation columns
      annotated_table <- list()

      for (row in table_data) {
        if (!is.list(row)) next

        # Check if row passes all filters
        passes_filter <- TRUE

        if (!is.null(filter)) {
          for (filter_spec in filter) {
            dataset_name <- filter_spec[[1]]
            field_name <- filter_spec[[2]]
            allowed_values <- filter_spec[[3]]

            dataset_index <- which(route == dataset_name)[1]

            elem <- if (length(row) >= dataset_index) row[[dataset_index]] else NULL
            id_value <- NULL
            if (!is.null(elem) && length(elem) > 0) {
              id_value <- as.character(elem)
              if (is.na(id_value) || nchar(id_value) == 0) id_value <- NULL
            }

            if (!is.null(id_value) &&
                !is.null(annotations_cache[[dataset_name]]) &&
                !is.null(annotations_cache[[dataset_name]][[id_value]])) {
              annotation_value <- annotations_cache[[dataset_name]][[id_value]][[field_name]]

              if (is.list(annotation_value)) {
                # Check if any value matches
                if (!any(as.character(unlist(annotation_value)) %in% allowed_values)) {
                  passes_filter <- FALSE
                  break
                }
              } else {
                if (!as.character(annotation_value) %in% allowed_values) {
                  passes_filter <- FALSE
                  break
                }
              }
            } else {
              passes_filter <- FALSE
              break
            }
          }
        }

        if (!passes_filter) {
          next
        }

        # Add annotation columns
        if (!is.null(annotate)) {
          # Build a mapping of dataset_index -> list of annotation values
          annotations_to_insert <- list()

          for (ann_spec in annotate) {
            dataset_name <- ann_spec[[1]]
            field_name <- ann_spec[[2]]

            dataset_index <- which(route == dataset_name)[1]

            if (!dataset_index %in% names(annotations_to_insert)) {
              annotations_to_insert[[as.character(dataset_index)]] <- list()
            }

            idx_key <- as.character(dataset_index)
            elem <- if (length(row) >= dataset_index) row[[dataset_index]] else NULL
            id_value <- NULL
            if (!is.null(elem) && length(elem) > 0) {
              id_value <- as.character(elem)
              if (is.na(id_value) || nchar(id_value) == 0) id_value <- NULL
            }

            if (!is.null(id_value) &&
                !is.null(annotations_cache[[dataset_name]]) &&
                !is.null(annotations_cache[[dataset_name]][[id_value]])) {
              annotation_value <- annotations_cache[[dataset_name]][[id_value]][[field_name]]

              # Handle list values
              if (is.list(annotation_value) && length(annotation_value) > 0) {
                annotation_value <- paste(unlist(annotation_value), collapse = ", ")
              } else if (is.null(annotation_value)) {
                annotation_value <- NA_character_
              }

              annotations_to_insert[[idx_key]][[length(annotations_to_insert[[idx_key]]) + 1]] <- as.character(annotation_value)
            } else {
              annotations_to_insert[[idx_key]][[length(annotations_to_insert[[idx_key]]) + 1]] <- NA_character_
            }
          }

          # Build new row by inserting annotations after their dataset columns
          new_row <- list()
          for (i in seq_along(row)) {
            new_row[[length(new_row) + 1]] <- row[[i]]

            # Insert annotations for this column if any
            idx_key <- as.character(i)
            if (idx_key %in% names(annotations_to_insert)) {
              for (ann_val in annotations_to_insert[[idx_key]]) {
                new_row[[length(new_row) + 1]] <- ann_val
              }
            }
          }

          annotated_table[[length(annotated_table) + 1]] <- new_row
        } else {
          annotated_table[[length(annotated_table) + 1]] <- row
        }
      }

      # Update response with annotated results
      if (is.list(response) && "results" %in% names(response)) {
        response$results <- annotated_table
        return(response)
      } else {
        return(annotated_table)
      }
    }
  )
)
