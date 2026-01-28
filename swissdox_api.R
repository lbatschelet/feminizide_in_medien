#' Swissdox@LiRI API Wrapper for R
#' 
#' A comprehensive R wrapper for the Swissdox@LiRI API that provides access to 
#' Swiss media content. This package allows users to submit queries, monitor 
#' their status, and download retrieved datasets.
#' 
#' @author Your Name
#' @references \url{https://liri.linguistik.uzh.ch/wiki/langtech/swissdox/api}

# Required packages
if (!require("httr", quietly = TRUE)) stop("Package 'httr' is required")
if (!require("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required") 
if (!require("yaml", quietly = TRUE)) stop("Package 'yaml' is required")
if (!require("readr", quietly = TRUE)) stop("Package 'readr' is required")

# Constants for validation
VALID_LANGUAGES <- c("de", "fr", "it", "en")
VALID_FORMATS <- c("TSV", "JSON")
VALID_MEDIA_CODES <- c(
  "ZWA", "ZWAS", "BLI", "BZB", "TGS", "BERN", "LUZ", "STGAL", "WELWO",
  "NZZO", "TA", "SONN", "CASH", "HAND", "BILZ", "FINA", "ZEIT"
)
VALID_COLUMNS <- c(
  "id", "pubtime", "medium_code", "medium_name", "rubric", "regional",
  "doctype", "doctype_description", "language", "char_count", "dateline",
  "head", "subhead", "article_link", "content_id", "content"
)

#' Swissdox API Client
#' 
#' @description
#' R6 class for interacting with the Swissdox@LiRI API. Provides methods for
#' submitting queries, checking status, and downloading datasets.
#' 
#' @details
#' The Swissdox@LiRI API allows access to Swiss media content through structured
#' queries. Users need valid API credentials to access the service.
#' 
#' @examples
#' \dontrun{
#' # Initialize client with credentials
#' client <- SwissdoxAPI$new(
#'   api_key = "your-api-key",
#'   api_secret = "your-api-secret"
#' )
#' 
#' # Create a simple query
#' query <- create_swissdox_query(
#'   content_terms = c("climate", "environment"),
#'   date_from = "2023-01-01",
#'   date_to = "2023-12-31"
#' )
#' 
#' # Submit query
#' result <- client$submit_query(query, "Climate Study")
#' 
#' # Check status
#' status <- client$get_query_status(result$queryId)
#' 
#' # Download when ready
#' if (status$status == "finished") {
#'   file_path <- client$download_dataset(
#'     basename(status$downloadUrl), 
#'     "data/"
#'   )
#' }
#' }
#' 
#' @export
SwissdoxAPI <- R6::R6Class("SwissdoxAPI",
  public = list(
    #' @field base_url API base URL
    base_url = "https://swissdox.linguistik.uzh.ch/api",
    
    #' Initialize Swissdox API client
    #' 
    #' @param api_key API key for authentication
    #' @param api_secret API secret for authentication
    #' @param base_url API base URL (default: official Swissdox URL)
    #' 
    #' @return New SwissdoxAPI instance
    initialize = function(api_key, api_secret, base_url = NULL) {
      if (missing(api_key) || missing(api_secret)) {
        stop("Both api_key and api_secret are required")
      }
      
      private$api_key <- api_key
      private$api_secret <- api_secret
      
      if (!is.null(base_url)) {
        self$base_url <- base_url
      }
      
      # Validate credentials
      private$validate_credentials()
    },
    
    #' Submit a query to the API
    #' 
    #' @param query YAML query string (use create_swissdox_query() to generate)
    #' @param name Query name for identification
    #' @param comment Optional comment for the query
    #' @param expiration_date Optional expiration date (YYYY-MM-DD)
    #' @param test If TRUE, only validates syntax without executing
    #' 
    #' @return List containing query response with queryId
    #' @export
    submit_query = function(query, name, comment = NULL, expiration_date = NULL, test = FALSE) {
      if (missing(query) || missing(name)) {
        stop("Both query and name are required")
      }
      
      # Validate YAML syntax
      tryCatch(
        yaml::yaml.load(query),
        error = function(e) stop("Invalid YAML syntax: ", e$message)
      )
      
      url <- paste0(self$base_url, "/query")
      
      body <- list(
        query = query,
        name = name,
        test = if (test) "1" else "0"
      )
      
      if (!is.null(comment)) body$comment <- comment
      if (!is.null(expiration_date)) body$expirationDate <- expiration_date
      
      response <- httr::POST(
        url,
        httr::add_headers(.headers = private$get_headers()),
        body = body,
        encode = "form"
      )
      
      private$handle_response(response)
    },
    
    #' Get status of all submitted queries
    #' 
    #' @return List of all queries with their current status
    #' @export
    get_all_queries = function() {
      url <- paste0(self$base_url, "/status")
      
      response <- httr::GET(
        url,
        httr::add_headers(.headers = private$get_headers())
      )
      
      private$handle_response(response)
    },
    
    #' Get status of a specific query
    #' 
    #' @param query_id Query ID returned from submit_query()
    #' 
    #' @return List containing query status and details
    #' @export
    get_query_status = function(query_id) {
      if (missing(query_id)) {
        stop("query_id is required")
      }
      
      url <- paste0(self$base_url, "/status/", query_id)
      
      response <- httr::GET(
        url,
        httr::add_headers(.headers = private$get_headers())
      )
      
      result <- private$handle_response(response)
      
      # API returns list, extract first element if it's a query status
      if (is.list(result) && length(result) > 0 && is.list(result[[1]])) {
        return(result[[1]])
      }
      
      result
    },
    
    #' Download a dataset
    #' 
    #' @param filename Filename from query status downloadUrl
    #' @param output_path Local path where file should be saved
    #' @param overwrite Whether to overwrite existing files (default: TRUE)
    #' 
    #' @return Path to downloaded file
    #' @export
    download_dataset = function(filename, output_path, overwrite = TRUE) {
      if (missing(filename) || missing(output_path)) {
        stop("Both filename and output_path are required")
      }
      
      url <- paste0(self$base_url, "/download/", filename)
      
      # Create directory if it doesn't exist
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
      
      response <- httr::GET(
        url,
        httr::add_headers(.headers = private$get_headers()),
        httr::write_disk(output_path, overwrite = overwrite)
      )
      
      if (httr::status_code(response) == 200) {
        message("Dataset downloaded successfully: ", output_path)
        return(output_path)
      } else {
        stop("Download failed: ", httr::status_code(response))
      }
    },
    
    #' Wait for query completion with progress updates
    #' 
    #' @param query_id Query ID to monitor
    #' @param check_interval Seconds between status checks (default: 30)
    #' @param max_wait_time Maximum wait time in seconds (default: Inf)
    #' @param verbose Whether to print status updates (default: TRUE)
    #' 
    #' @return Final query status when completed
    #' @export
    wait_for_completion = function(query_id, check_interval = 30, max_wait_time = Inf, verbose = TRUE) {
      if (missing(query_id)) {
        stop("query_id is required")
      }
      
      start_time <- Sys.time()
      last_status <- NULL
      
      repeat {
        status <- self$get_query_status(query_id)
        
        if (status$status %in% c("finished", "completed")) {
          if (verbose) {
            message("✓ Query completed successfully")
            if (!is.null(status$actualResults)) {
              message("  Articles found: ", status$actualResults)
            }
            if (!is.null(status$estimateResults)) {
              message("  Original estimate: ", status$estimateResults)
            }
          }
          return(status)
        } else if (status$status == "failed") {
          stop("Query failed: ", status$error %||% "Unknown error")
        }
        
        elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
        if (is.finite(max_wait_time) && elapsed > max_wait_time) {
          stop("Timeout reached. Query still running after ", max_wait_time, " seconds")
        }
        
        if (verbose && (is.null(last_status) || last_status != status$status)) {
          message("Status: ", status$status, " (", round(elapsed), "s elapsed)")
          if (!is.null(status$estimateResults)) {
            message("  Estimated results: ", status$estimateResults)
          }
          last_status <- status$status
        }
        
        Sys.sleep(check_interval)
      }
    },
    
    #' Load dataset directly as data frame
    #' 
    #' @param filename Filename from query status downloadUrl
    #' @param temp_download Whether to use temporary file (default: TRUE)
    #' @param encoding File encoding (default: "UTF-8")
    #' 
    #' @return data.frame with loaded dataset
    #' @export
    load_dataset = function(filename, temp_download = TRUE, encoding = "UTF-8") {
      if (missing(filename)) {
        stop("filename is required")
      }
      
      if (temp_download) {
        temp_file <- tempfile(fileext = if (grepl("\\.xz$", filename)) ".tsv.xz" else ".tsv")
        output_path <- temp_file
      } else {
        output_path <- file.path("data", filename)
      }
      
      # Download file
      self$download_dataset(filename, output_path, overwrite = TRUE)
      
      # Load as data frame
      if (grepl("\\.xz$", filename)) {
        # Compressed file
        data <- readr::read_tsv(output_path, locale = readr::locale(encoding = encoding),
                               show_col_types = FALSE, na = c("", "NA"))
      } else {
        # Regular TSV
        data <- readr::read_tsv(output_path, locale = readr::locale(encoding = encoding),
                               show_col_types = FALSE, na = c("", "NA"))
      }
      
      # Clean up temp file
      if (temp_download && file.exists(output_path)) {
        unlink(output_path)
      }
      
      # Convert date columns
      if ("pubtime" %in% names(data)) {
        data$pubtime <- as.POSIXct(data$pubtime)
        data$pub_date <- as.Date(data$pubtime)
        data$year <- format(data$pubtime, "%Y")
        data$month <- format(data$pubtime, "%Y-%m")
      }
      
      message("Dataset loaded: ", nrow(data), " rows, ", ncol(data), " columns")
      return(data)
    }
  ),
  
  private = list(
    api_key = NULL,
    api_secret = NULL,
    
    get_headers = function() {
      c(
        "X-API-Key" = private$api_key,
        "X-API-Secret" = private$api_secret,
        "Content-Type" = "application/x-www-form-urlencoded"
      )
    },
    
    validate_credentials = function() {
      if (is.null(private$api_key) || private$api_key == "" ||
          is.null(private$api_secret) || private$api_secret == "") {
        stop("Invalid API credentials provided")
      }
    },
    
    handle_response = function(response) {
      if (httr::status_code(response) == 200) {
        return(httr::content(response, "parsed"))
      } else {
        error_msg <- tryCatch(
          httr::content(response, "text", encoding = "UTF-8"),
          error = function(e) paste("HTTP", httr::status_code(response))
        )
        stop("API request failed: ", error_msg)
      }
    }
  )
)

#' Create a Swissdox query in YAML format
#' 
#' @description
#' Helper function to create properly formatted YAML queries for the Swissdox API.
#' Supports various search parameters and content filtering options.
#' 
#' @param content_terms Character vector of search terms (optional if content_groups is used)
#' @param content_groups List of content groups for AND-combined subqueries.
#'   Each element is a list with fields: operator ("OR" or "AND") and terms (character vector).
#' @param sources Character vector of media source codes (optional)
#' @param date_from Start date in YYYY-MM-DD format (default: "2000-01-01")
#' @param date_to End date in YYYY-MM-DD format (default: current date)
#' @param languages Character vector of language codes (default: "de")
#' @param exclude_terms Character vector of terms to exclude (optional)
#' @param max_results Maximum number of results (default: 1000)
#' @param result_format Result format, either "TSV" or "JSON" (default: "TSV")
#' @param columns Character vector of columns to include in results
#' @param content_operator Logical operator for content terms: "OR" or "AND" (default: "OR")
#' 
#' @return YAML-formatted query string ready for API submission
#' 
#' @examples
#' \dontrun{
#' # Simple query
#' query <- create_swissdox_query(
#'   content_terms = c("climate change", "global warming"),
#'   date_from = "2023-01-01",
#'   date_to = "2023-12-31"
#' )
#' 
#' # Complex query with exclusions
#' query <- create_swissdox_query(
#'   content_terms = c("artificial intelligence", "machine learning"),
#'   sources = c("ZWA", "TGS", "BLI"),
#'   exclude_terms = c("science fiction", "movie"),
#'   max_results = 5000,
#'   columns = c("id", "pubtime", "medium_name", "head", "content")
#' )
#' }
#' 
#' @export
create_swissdox_query <- function(content_terms = NULL,
                                 content_groups = NULL,
                                 sources = NULL,
                                 date_from = "2000-01-01",
                                 date_to = format(Sys.Date(), "%Y-%m-%d"),
                                 languages = "de",
                                 exclude_terms = NULL,
                                 max_results = 1000,
                                 result_format = "TSV",
                                 columns = c("id", "pubtime", "medium_name", "head", "content"),
                                 content_operator = "OR") {
  if ((is.null(content_terms) || length(content_terms) == 0) &&
      (is.null(content_groups) || length(content_groups) == 0)) {
    stop("Provide content_terms or content_groups")
  }
  
  # Validate parameters
  if (!result_format %in% VALID_FORMATS) {
    stop("result_format must be one of: ", paste(VALID_FORMATS, collapse = ", "))
  }
  
  if (!content_operator %in% c("OR", "AND")) {
    stop("content_operator must be either 'OR' or 'AND'")
  }
  
  # Validate languages
  invalid_langs <- setdiff(languages, VALID_LANGUAGES)
  if (length(invalid_langs) > 0) {
    warning("Unknown language codes: ", paste(invalid_langs, collapse = ", "),
            ". Valid codes: ", paste(VALID_LANGUAGES, collapse = ", "))
  }
  
  # Validate media sources
  if (!is.null(sources)) {
    invalid_sources <- setdiff(sources, VALID_MEDIA_CODES)
    if (length(invalid_sources) > 0) {
      warning("Unknown media codes: ", paste(invalid_sources, collapse = ", "),
              ". Valid codes: ", paste(VALID_MEDIA_CODES, collapse = ", "))
    }
  }
  
  # Validate columns
  invalid_cols <- setdiff(columns, VALID_COLUMNS)
  if (length(invalid_cols) > 0) {
    warning("Unknown column names: ", paste(invalid_cols, collapse = ", "),
            ". Valid columns: ", paste(VALID_COLUMNS, collapse = ", "))
  }
  
  # Validate date format
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date_from) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", date_to)) {
    stop("Dates must be in YYYY-MM-DD format")
  }
  
  # Build content query - match exact structure from API example
  if (!is.null(content_groups) && length(content_groups) > 0) {
    groups <- lapply(content_groups, function(group) {
      op <- if (!is.null(group$operator)) group$operator else "OR"
      terms <- if (!is.null(group$terms)) group$terms else character(0)
      if (!op %in% c("OR", "AND")) {
        stop("content_groups operator must be 'OR' or 'AND'")
      }
      if (length(terms) == 0) {
        stop("content_groups terms cannot be empty")
      }
      setNames(list(terms), op)
    })
    content_query <- list(AND = groups)
  } else if (content_operator == "OR") {
    content_query <- list(OR = content_terms)  # Direct vector, not list of lists
  } else {
    content_query <- list(AND = content_terms)
  }
  
  # Add exclusions if provided - create structure exactly like example:
  # AND: [OR: [term1, term2], NOT: exclude1, NOT: exclude2]
  if (!is.null(exclude_terms) && length(exclude_terms) > 0) {
    # Create NOT elements - each exclude term gets its own NOT
    exclude_items <- lapply(exclude_terms, function(x) setNames(list(x), "NOT"))
    
    # Build AND structure: first the main query, then each NOT as separate item
    and_items <- c(list(content_query), exclude_items)
    content_query <- list(AND = and_items)
  }
  
  # Build complete query structure
  query_structure <- list(
    query = list(
      dates = list(list(from = date_from, to = date_to)),
      languages = languages,
      content = content_query
    ),
    result = list(
      format = result_format,
      maxResults = as.integer(max_results),  # Ensure integer format
      columns = columns
    ),
    version = 1.2  # Numeric, not string
  )
  
  # Add sources if provided
  if (!is.null(sources) && length(sources) > 0) {
    query_structure$query$sources <- sources
  }
  
  # Build YAML manually matching exact API example format (4-space indentation)
  yaml_lines <- c(
    "query:",
    "    dates:",
    paste0("        - from: ", date_from),
    paste0("          to: ", date_to),
    paste0("    languages:"),
    paste0("        - ", paste(languages, collapse = "\n        - ")),
    "    content:"
  )
  
  # Add content structure exactly like API example
  if (!is.null(content_groups) && length(content_groups) > 0) {
    yaml_lines <- c(yaml_lines, "        AND:")
    for (group in content_groups) {
      op <- if (!is.null(group$operator)) group$operator else "OR"
      terms <- if (!is.null(group$terms)) group$terms else character(0)
      yaml_lines <- c(yaml_lines, paste0("            - ", op, ":"))
      for (term in terms) {
        yaml_lines <- c(yaml_lines, paste0("                - ", term))
      }
    }
    if (!is.null(exclude_terms) && length(exclude_terms) > 0) {
      for (exclude in exclude_terms) {
        yaml_lines <- c(yaml_lines, paste0("            - NOT: ", exclude))
      }
    }
  } else if (!is.null(exclude_terms) && length(exclude_terms) > 0) {
    yaml_lines <- c(yaml_lines, "        AND:")
    yaml_lines <- c(yaml_lines, paste0("            - ", content_operator, ":"))
    for (term in content_terms) {
      yaml_lines <- c(yaml_lines, paste0("                - ", term))
    }
    for (exclude in exclude_terms) {
      yaml_lines <- c(yaml_lines, paste0("            - NOT: ", exclude))
    }
  } else if (content_operator == "OR") {
    yaml_lines <- c(yaml_lines, "        OR:")
    for (term in content_terms) {
      yaml_lines <- c(yaml_lines, paste0("            - ", term))
    }
  } else {
    yaml_lines <- c(yaml_lines, "        AND:")
    for (term in content_terms) {
      yaml_lines <- c(yaml_lines, paste0("            - ", term))
    }
  }
  
  # Add sources if provided (before dates in API example)
  if (!is.null(sources) && length(sources) > 0) {
    # Insert sources after query: but before dates:
    sources_lines <- c("    sources:")
    for (source in sources) {
      sources_lines <- c(sources_lines, paste0("        - ", source))
    }
    # Insert at position 2 (after "query:")
    yaml_lines <- c(yaml_lines[1], sources_lines, yaml_lines[2:length(yaml_lines)])
  }
  
  # Add result section exactly like API example
  yaml_lines <- c(yaml_lines,
    "result:",
    paste0("    format: ", result_format),
    paste0("    maxResults: ", max_results),
    "    columns:"
  )
  
  for (col in columns) {
    yaml_lines <- c(yaml_lines, paste0("        - ", col))
  }
  
  # Add version exactly like API example
  yaml_lines <- c(yaml_lines, "version: 1.2")
  
  # Join lines
  paste(yaml_lines, collapse = "\n")
}

#' Execute complete Swissdox workflow
#' 
#' @description
#' Convenience function that combines query submission, waiting for completion,
#' and dataset download into a single operation.
#' 
#' @param client SwissdoxAPI client instance
#' @param query YAML query string (from create_swissdox_query())
#' @param name Query name for identification
#' @param comment Optional comment for the query
#' @param output_dir Directory to save downloaded file (default: "data")
#' @param wait_for_completion Whether to wait for query completion (default: TRUE)
#' @param check_interval Seconds between status checks (default: 30)
#' @param max_wait_time Maximum wait time in seconds (default: Inf)
#' 
#' @return List with query information and file path (if completed)
#' 
#' @examples
#' \dontrun{
#' client <- SwissdoxAPI$new("your-key", "your-secret")
#' 
#' query <- create_swissdox_query(
#'   content_terms = c("sustainability", "renewable energy"),
#'   date_from = "2023-01-01"
#' )
#' 
#' result <- execute_swissdox_workflow(
#'   client = client,
#'   query = query,
#'   name = "Sustainability Study 2023",
#'   comment = "Research on renewable energy coverage"
#' )
#' }
#' 
#' @export
execute_swissdox_workflow <- function(client,
                                     query,
                                     name,
                                     comment = NULL,
                                     return_data = FALSE,
                                     output_dir = "data",
                                     wait_for_completion = TRUE,
                                     check_interval = 30,
                                     max_wait_time = Inf) {
  
  if (!inherits(client, "SwissdoxAPI")) {
    stop("client must be a SwissdoxAPI instance")
  }
  
  if (missing(query) || missing(name)) {
    stop("Both query and name are required")
  }
  
  # Submit query
  message("Submitting query: ", name)
  result <- client$submit_query(query, name, comment)
  
  query_id <- result$queryId
  if (is.null(query_id) || query_id == "") {
    stop("No query ID received from API")
  }
  
  message("Query submitted successfully. ID: ", query_id)
  
  if (!wait_for_completion) {
    return(list(
      query_id = query_id,
      status = "submitted",
      message = "Query submitted but not waiting for completion"
    ))
  }
  
  # Wait for completion with enhanced progress
  message("Waiting for query completion...")
  final_status <- client$wait_for_completion(
    query_id, 
    check_interval = check_interval,
    max_wait_time = max_wait_time
  )
  
  # Handle completion
  if (!is.null(final_status$downloadUrl)) {
    filename <- basename(final_status$downloadUrl)
    
    result_list <- list(
      query_id = query_id,
      status = "completed",
      article_count = final_status$actualResults,
      filename = filename,
      download_url = final_status$downloadUrl
    )
    
    if (return_data) {
      # Load data directly as data frame
      message("Loading dataset as data frame...")
      data <- client$load_dataset(filename, temp_download = !file.exists(output_dir))
      result_list$data <- data
      result_list$message <- paste("Dataset loaded with", nrow(data), "articles")
    } else {
      # Download to file
      output_path <- file.path(output_dir, filename)
      message("Downloading dataset to file...")
      downloaded_file <- client$download_dataset(filename, output_path)
      result_list$file_path <- downloaded_file
      result_list$message <- paste("Dataset saved to", downloaded_file)
    }
    
    return(result_list)
  } else {
    warning("Query completed but no download URL available")
    return(list(
      query_id = query_id,
      status = final_status$status,
      article_count = final_status$actualResults %||% 0,
      message = "Query completed but no data available"
    ))
  }
}

#' Get available media codes
#' 
#' @return Character vector of valid media codes
#' @export
get_valid_media_codes <- function() {
  VALID_MEDIA_CODES
}

#' Get available languages
#' 
#' @return Character vector of valid language codes  
#' @export
get_valid_languages <- function() {
  VALID_LANGUAGES
}

#' Get available result columns
#' 
#' @return Character vector of valid column names
#' @export  
get_valid_columns <- function() {
  VALID_COLUMNS
}

# Utility function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || x == "") y else x