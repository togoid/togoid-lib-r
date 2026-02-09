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
    #' @param report Report type: "target", "pair", "full"
    #' @param annotate List of lists: list(list("dataset", "field")) to add annotation columns
    #' @param filter List of lists: list(list("dataset", "field", c("values"))) to filter results
    #' @param ... Additional parameters passed to API
    #'
    #' @return Converted IDs in specified format
    convert = function(ids, route, format = "dataframe", report = "pair",
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
      return(private$format_response(response, format, route, ids))
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
    get_ortholog = function(ids, route, target_taxids, format = "table") {

      if (length(route) != 2) {
        cli::cli_abort("Route for ortholog must have exactly 2 databases")
      }

      # Build round-trip route: source -> intermediate -> source -> taxonomy
      full_route <- c(route[1], route[2], route[1], "taxonomy")

      # Convert with full route
      result <- self$convert(
        ids = ids,
        route = full_route,
        format = "table",
        report = "full"
      )

      # Filter by target taxonomy IDs
      if (is.list(result) && length(result) > 0) {
        # Result is a list of vectors/lists
        filtered <- Filter(function(row) {
          if (length(row) >= 4) {
            return(as.character(row[[4]]) %in% target_taxids)
          }
          return(FALSE)
        }, result)

        return(private$format_ortholog_result(filtered, format))
      }

      return(result)
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
    format_response = function(response, format, route, ids) {
      format <- normalize_format(format)

      switch(format,
        "json" = return(response),
        "list" = return(private$convert_to_list(response, route, ids)),
        "table" = return(private$convert_to_table(response)),
        "dataframe" = return(private$convert_to_dataframe(response)),
        "tibble" = return(private$convert_to_tibble(response)),
        return(response) # default
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
    #'
    #' @return data.frame
    convert_to_dataframe = function(response) {
      table_data <- private$convert_to_table(response)

      if (length(table_data) == 0) {
        return(data.frame())
      }

      # Convert to data.frame
      if (is.list(table_data) && length(table_data) > 0) {
        # Check first element to determine number of columns
        first_row <- table_data[[1]]
        ncols <- length(first_row)

        # Create column names
        col_names <- if (ncols == 2) {
          c("source_id", "target_id")
        } else {
          paste0("V", seq_len(ncols))
        }

        # Convert each row to a data.frame row
        df <- do.call(rbind, lapply(table_data, function(row) {
          as.data.frame(t(unlist(row)), stringsAsFactors = FALSE)
        }))

        colnames(df) <- col_names
        rownames(df) <- NULL
        return(df)
      }

      return(data.frame())
    },

    #' Convert response to tibble
    #'
    #' @param response API response
    #'
    #' @return tibble
    convert_to_tibble = function(response) {
      df <- private$convert_to_dataframe(response)
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

    #' Add annotations to conversion results (placeholder)
    #'
    #' @param response API response
    #' @param route Conversion route
    #' @param annotate Annotation specifications
    #' @param filter Filter specifications
    #'
    #' @return Response with annotations
    add_annotations = function(response, route, annotate, filter) {
      # TODO: Implement annotation functionality
      # This requires AnnotationsConverter class
      cli::cli_warn("Annotation functionality not yet implemented in R version")
      return(response)
    }
  )
)
