#' Annotations Converter Class
#'
#' R6 class for retrieving annotations for IDs using GRASP GraphQL endpoint
#'
#' @field api_endpoint TogoID API endpoint
#' @field grasp_endpoint GRASP GraphQL endpoint
#' @field verbose Enable verbose logging
#' @field timeout Request timeout in seconds
#'
#' @export
#' @importFrom R6 R6Class
AnnotationsConverter <- R6::R6Class(
  "AnnotationsConverter",

  public = list(
    #' @field api_endpoint API endpoint
    api_endpoint = NULL,

    #' @field grasp_endpoint GRASP GraphQL endpoint
    grasp_endpoint = NULL,

    #' @field verbose Verbose mode
    verbose = FALSE,

    #' @field timeout Request timeout
    timeout = 30,

    #' @description
    #' Initialize AnnotationsConverter
    #'
    #' @param api_endpoint TogoID API endpoint
    #' @param grasp_endpoint GRASP GraphQL endpoint
    #' @param verbose Enable verbose logging
    #' @param timeout Request timeout in seconds
    #'
    #' @return A new `AnnotationsConverter` object
    initialize = function(api_endpoint = NULL,
                         grasp_endpoint = NULL,
                         verbose = FALSE,
                         timeout = 30) {

      self$api_endpoint <- api_endpoint %||%
        get_env_var("TOGOID_API_ENDPOINT", "https://api.togoid.dbcls.jp")

      self$grasp_endpoint <- grasp_endpoint %||%
        get_env_var("TOGOID_GRASP_ENDPOINT", "https://dx.dbcls.jp/grasp-dev-togoid")

      self$api_endpoint <- sub("/$", "", self$api_endpoint)
      self$grasp_endpoint <- sub("/$", "", self$grasp_endpoint)
      self$verbose <- verbose
      self$timeout <- timeout

      private$dataset_config <- NULL
    },

    #' @description
    #' Get dataset configuration
    #'
    #' @param dataset_name Dataset name
    #'
    #' @return Dataset configuration as list
    get_dataset = function(dataset_name) {
      if (is.null(private$dataset_config)) {
        private$dataset_config <- private$get_dataset_config()
      }

      if (!dataset_name %in% names(private$dataset_config)) {
        available <- paste(names(private$dataset_config), collapse = ", ")
        cli::cli_abort(c(
          "Unknown dataset: {dataset_name}",
          "i" = "Available datasets: {available}"
        ))
      }

      return(private$dataset_config[[dataset_name]])
    },

    #' @description
    #' List available annotation fields for a dataset
    #'
    #' @param dataset_name Dataset name
    #'
    #' @return Data frame with field information
    list_fields = function(dataset_name) {
      dataset <- self$get_dataset(dataset_name)
      annotations <- dataset$annotations %||% list()

      # Start with label field
      fields <- list(
        list(
          field_name = "label",
          label = "Label",
          is_list = FALSE,
          description = "Standard label field from GRASP GraphQL"
        )
      )

      # Add annotations from config
      for (annotation in annotations) {
        fields[[length(fields) + 1]] <- list(
          field_name = annotation$variable,
          label = annotation$label %||% annotation$variable,
          is_list = annotation$is_list %||% FALSE,
          numerical = annotation$numerical %||% FALSE
        )
      }

      # Convert to data frame
      df <- do.call(rbind, lapply(fields, function(f) {
        data.frame(
          field_name = f$field_name,
          label = f$label,
          is_list = f$is_list,
          stringsAsFactors = FALSE
        )
      }))

      return(df)
    },

    #' @description
    #' Execute GraphQL query to retrieve annotations
    #'
    #' @param dataset_name Dataset name
    #' @param ids Character vector of IDs
    #' @param fields Character vector of field names to retrieve
    #' @param filters Named list of filters
    #'
    #' @return Named list: id -> field -> value
    execute_query = function(dataset_name, ids, fields, filters = list()) {

      if (length(ids) == 0) {
        return(list())
      }

      # Build GraphQL query
      query_info <- private$build_query(dataset_name, fields, filters)
      query <- query_info$query
      variables_template <- query_info$variables

      # Execute query in batches (to avoid too large requests)
      batch_size <- 100
      all_records <- list()

      for (i in seq(1, length(ids), by = batch_size)) {
        end_idx <- min(i + batch_size - 1, length(ids))
        batch_ids <- ids[i:end_idx]

        if (self$verbose) {
          cli::cli_alert_info("Querying IDs {i}-{end_idx} of {length(ids)}")
        }

        # Prepare variables for this batch
        variables <- variables_template
        variables$id <- as.list(batch_ids)

        # Add filter variables if present
        for (filter_name in names(filters)) {
          variables[[filter_name]] <- as.list(filters[[filter_name]])
        }

        # Execute GraphQL request
        response <- private$execute_graphql(query, variables)

        # Parse response
        if (!is.null(response$data) && !is.null(response$data[[dataset_name]])) {
          records <- response$data[[dataset_name]]

          for (record in records) {
            id_value <- as.character(record$id)
            all_records[[id_value]] <- record
          }
        }
      }

      return(all_records)
    },

    #' @description
    #' Build table rows from query results
    #'
    #' @param dataset_label Dataset label
    #' @param fields Field names
    #' @param field_meta Field metadata
    #' @param records Query results
    #' @param filters Filters to apply
    #' @param compact Compact output format
    #'
    #' @return List of rows
    build_rows = function(dataset_label, fields, field_meta, records,
                         filters = list(), compact = FALSE) {

      rows <- list()

      for (id_value in names(records)) {
        record <- records[[id_value]]

        # Check filters
        passes_filters <- TRUE
        for (filter_name in names(filters)) {
          if (!filter_name %in% names(record)) {
            passes_filters <- FALSE
            break
          }

          allowed_values <- filters[[filter_name]]
          record_value <- record[[filter_name]]

          if (is.list(record_value)) {
            # Check if any value matches
            if (!any(as.character(unlist(record_value)) %in% allowed_values)) {
              passes_filters <- FALSE
              break
            }
          } else {
            if (!as.character(record_value) %in% allowed_values) {
              passes_filters <- FALSE
              break
            }
          }
        }

        if (!passes_filters) {
          next
        }

        # Build row
        row <- list(id = id_value)

        for (field_name in fields) {
          if (field_name == "id") {
            next
          }

          value <- record[[field_name]] %||% ""

          # Format list values
          if (is.list(value) && length(value) > 0) {
            if (compact) {
              row[[field_name]] <- paste(unlist(value), collapse = ", ")
            } else {
              row[[field_name]] <- unlist(value)
            }
          } else {
            row[[field_name]] <- as.character(value)
          }
        }

        rows[[length(rows) + 1]] <- row
      }

      return(rows)
    }
  ),

  private = list(
    #' Cached dataset configuration
    dataset_config = NULL,

    #' Get dataset configuration from API
    #'
    #' @return Dataset configuration
    get_dataset_config = function() {
      url <- paste0(self$api_endpoint, "/config/dataset")

      if (self$verbose) {
        cli::cli_alert_info("Fetching dataset configuration from {url}")
      }

      response <- make_request(
        base_url = self$api_endpoint,
        endpoint = "config/dataset",
        method = "GET",
        timeout = self$timeout
      )

      return(response)
    },

    #' Build GraphQL query
    #'
    #' @param dataset_name Dataset name
    #' @param fields Fields to select
    #' @param filters Filters to apply
    #'
    #' @return List with query and variables template
    build_query = function(dataset_name, fields, filters) {

      # Remove duplicates and "id"
      unique_fields <- unique(fields)
      unique_fields <- unique_fields[unique_fields != "id"]

      # Build variable definitions
      variable_defs <- c("$id: [String!]")
      argument_defs <- c("id: $id")

      if (length(filters) > 0) {
        for (filter_name in names(filters)) {
          variable_defs <- c(variable_defs, paste0("$", filter_name, ": [String!]"))
          argument_defs <- c(argument_defs, paste0(filter_name, ": $", filter_name))
        }
      }

      # Build selection set
      selection_set <- c("id", unique_fields)

      # Build query string
      query_lines <- c(
        paste0("query (", paste(variable_defs, collapse = ", "), ") {"),
        paste0("  ", dataset_name, "(", paste(argument_defs, collapse = ", "), ") {")
      )

      for (field in selection_set) {
        query_lines <- c(query_lines, paste0("    ", field))
      }

      query_lines <- c(query_lines, "  }", "}")

      query <- paste(query_lines, collapse = "\n")

      # Variables template
      variables <- list(id = list())

      list(query = query, variables = variables)
    },

    #' Execute GraphQL request
    #'
    #' @param query GraphQL query string
    #' @param variables GraphQL variables
    #'
    #' @return GraphQL response
    execute_graphql = function(query, variables) {

      body <- list(
        query = query,
        variables = variables
      )

      tryCatch(
        {
          req <- httr2::request(self$grasp_endpoint)
          req <- httr2::req_body_json(req, body)
          req <- httr2::req_timeout(req, self$timeout)

          resp <- httr2::req_perform(req)
          result <- httr2::resp_body_json(resp)

          # Check for GraphQL errors
          if (!is.null(result$errors)) {
            error_msgs <- sapply(result$errors, function(e) e$message)
            cli::cli_abort(c(
              "GraphQL query failed",
              "x" = paste(error_msgs, collapse = ", ")
            ))
          }

          return(result)
        },
        error = function(e) {
          cli::cli_abort(c(
            "GraphQL request failed",
            "x" = conditionMessage(e)
          ))
        }
      )
    }
  )
)
