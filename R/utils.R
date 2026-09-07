#' @importFrom httr2 request req_url_query req_body_form req_body_json req_method req_perform resp_body_json resp_body_string resp_status
#' @importFrom jsonlite fromJSON toJSON
NULL

#' Make HTTP Request to TogoID API
#'
#' Internal function to make HTTP requests with error handling
#'
#' @param base_url Base URL of the API
#' @param endpoint API endpoint path
#' @param method HTTP method ("GET" or "POST")
#' @param params Query parameters as named list (for GET) or form fields (for POST when form_data is NULL)
#' @param form_data Named list to send as application/x-www-form-urlencoded body (POST only)
#' @param json_data JSON body data for POST requests
#' @param timeout Timeout in seconds (default: 30)
#'
#' @return Response data (list for JSON, character for text)
#' @keywords internal
make_request <- function(base_url, endpoint, method = "GET",
                        params = NULL, form_data = NULL,
                        json_data = NULL, timeout = 30) {

  # Build URL
  url <- paste0(sub("/$", "", base_url), "/", sub("^/", "", endpoint))

  tryCatch(
    {
      req <- httr2::request(url)
      req <- httr2::req_method(req, method)
      req <- httr2::req_timeout(req, timeout)

      # Surface the API's own error message (it returns {"message": ...}) instead
      # of only the HTTP status.
      req <- httr2::req_error(req, body = function(resp) {
        tryCatch(httr2::resp_body_json(resp)$message, error = function(e) NULL)
      })

      if (method == "POST") {
        if (!is.null(json_data)) {
          req <- httr2::req_body_json(req, json_data)
        } else {
          # Prefer explicit form_data; fall back to params for backward compatibility.
          body <- if (!is.null(form_data)) form_data else params
          if (!is.null(body) && length(body) > 0) {
            req <- httr2::req_body_form(req, !!!body)
          }
        }
      } else {
        if (!is.null(params) && length(params) > 0) {
          req <- httr2::req_url_query(req, !!!params)
        }
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

#' Normalize format string to R output type
#'
#' Validates the requested output format and converts it to a canonical name.
#' Python-specific formats ("json", "table") are rejected with an error so that
#' R users use the R-native names ("list", "dataframe", "tibble").
#'
#' @param format Format name ("list", "dataframe", "tibble")
#'
#' @return Standardized format name
#' @keywords internal
normalize_format <- function(format) {
  format <- tolower(format)
  if (format %in% c("json", "table")) {
    cli::cli_abort(c(
      "Unsupported format: {format}",
      "i" = "Use one of: \"list\", \"dataframe\", \"tibble\"."
    ))
  }
  switch(format,
    "dict" = "list",
    "list" = "list",
    "dataframe" = "dataframe",
    "df" = "dataframe",
    "tibble" = "tibble",
    cli::cli_abort(c(
      "Unsupported format: {format}",
      "i" = "Use one of: \"list\", \"dataframe\", \"tibble\"."
    ))
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
