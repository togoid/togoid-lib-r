#' @importFrom httr2 request req_url_query req_body_json req_perform resp_body_json resp_body_string resp_status
#' @importFrom jsonlite fromJSON toJSON
NULL

#' Make HTTP Request to TogoID API
#'
#' Internal function to make HTTP requests with error handling
#'
#' @param base_url Base URL of the API
#' @param endpoint API endpoint path
#' @param method HTTP method ("GET" or "POST")
#' @param params Query parameters as named list
#' @param json_data JSON body data for POST requests
#' @param timeout Timeout in seconds (default: 30)
#'
#' @return Response data (list for JSON, character for text)
#' @keywords internal
make_request <- function(base_url, endpoint, method = "GET",
                        params = NULL, json_data = NULL, timeout = 30) {

  # Build URL
  url <- paste0(sub("/$", "", base_url), "/", sub("^/", "", endpoint))

  tryCatch(
    {
      req <- httr2::request(url)

      # Add query parameters
      if (!is.null(params) && length(params) > 0) {
        req <- httr2::req_url_query(req, !!!params)
      }

      # Add timeout
      req <- httr2::req_timeout(req, timeout)

      # Handle POST
      if (method == "POST" && !is.null(json_data)) {
        req <- httr2::req_body_json(req, json_data)
      }

      # Perform request
      resp <- httr2::req_perform(req)

      # Parse response
      content_type <- httr2::resp_header(resp, "Content-Type")

      if (grepl("application/json", content_type %||% "", fixed = TRUE)) {
        return(httr2::resp_body_json(resp))
      } else {
        return(httr2::resp_body_string(resp))
      }
    },
    error = function(e) {
      cli::cli_abort(c(
        "API request failed",
        "x" = "URL: {url}",
        "x" = "Error: {conditionMessage(e)}"
      ))
    }
  )
}

#' Get Environment Variable with Default
#'
#' @param var_name Environment variable name
#' @param default Default value if not set
#'
#' @return Environment variable value or default
#' @keywords internal
get_env_var <- function(var_name, default) {
  value <- Sys.getenv(var_name, unset = "")
  if (value == "") {
    return(default)
  }
  return(value)
}

#' Convert Python-style format to R
#'
#' Converts different format names to consistent R output types
#'
#' @param format Format name ("json", "dict", "table", "dataframe", "csv", "tsv")
#'
#' @return Standardized format name
#' @keywords internal
normalize_format <- function(format) {
  format <- tolower(format)
  switch(format,
    "json" = "list",
    "dict" = "list",
    "list" = "list",
    "table" = "dataframe",
    "dataframe" = "dataframe",
    "df" = "dataframe",
    "tibble" = "tibble",
    "csv" = "dataframe",
    "tsv" = "dataframe",
    "dataframe" # default
  )
}

#' Null coalescing operator
#'
#' @param x First value
#' @param y Second value (default)
#'
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
