# Swissdox@LiRI API Wrapper für R

library(httr)
library(jsonlite)
library(yaml)
library(dotenv)

# Lade Umgebungsvariablen
if (file.exists("local.env")) {
  load_dot_env("local.env")
}
SwissdoxAPI <- R6::R6Class("SwissdoxAPI",
  public = list(
    api_key = NULL,
    api_secret = NULL,
    base_url = "https://swissdox.linguistik.uzh.ch/api",
    
    initialize = function(api_key = NULL, api_secret = NULL) {
      self$api_key <- api_key %||% Sys.getenv("SWISSDOX_API_KEY")
      self$api_secret <- api_secret %||% Sys.getenv("SWISSDOX_API_SECRET")
      
      if (self$api_key == "" || self$api_secret == "") {
        stop("API-Credentials fehlen. Setze SWISSDOX_API_KEY und SWISSDOX_API_SECRET in local.env")
      }
    },
    
    get_headers = function() {
      c(
        "X-API-Key" = self$api_key,
        "X-API-Secret" = self$api_secret,
        "Content-Type" = "application/x-www-form-urlencoded"
      )
    },
    
    submit_query = function(query, name, comment = NULL, expiration_date = NULL, test = FALSE) {
      url <- paste0(self$base_url, "/query")
      
      tryCatch(yaml::yaml.load(query), error = function(e) stop("Ungültiges YAML: ", e$message))
      body <- list(query = query, name = name, test = if (test) "1" else "0")
      if (!is.null(comment)) body$comment <- comment
      if (!is.null(expiration_date)) body$expirationDate <- expiration_date
      response <- httr::POST(url, httr::add_headers(.headers = self$get_headers()), 
                            body = body, encode = "form")
      
      if (httr::status_code(response) == 200) {
        result <- httr::content(response, "parsed")
        if (test) {
          message("✓ Query-Syntax OK")
        } else {
          query_id <- result$queryId %||% result$id
          if (!is.null(query_id) && query_id != "") {
            message("✓ Query gesendet. ID: ", query_id)
          } else {
            message("✓ Query gesendet (keine ID erhalten)")
          }
        }
        return(result)
      } else {
        response_text <- httr::content(response, "text", encoding = "UTF-8")
        stop("API-Fehler: ", httr::status_code(response), " - ", response_text)
      }
    },
    
    get_all_status = function() {
      url <- paste0(self$base_url, "/status")
      
      response <- httr::GET(
        url,
        httr::add_headers(.headers = self$get_headers())
      )
      
      if (httr::status_code(response) == 200) {
        return(httr::content(response, "parsed"))
      } else {
        stop("API-Fehler: ", httr::status_code(response), " - ", httr::content(response, "text"))
      }
    },
    
    get_query_status = function(query_id) {
      url <- paste0(self$base_url, "/status/", query_id)
      
      response <- httr::GET(
        url,
        httr::add_headers(.headers = self$get_headers())
      )
      
      if (httr::status_code(response) == 200) {
        result <- httr::content(response, "parsed")
        
        # API gibt Liste zurück, erstes Element ist die Query
        if (is.list(result) && length(result) > 0) {
          return(result[[1]])
        }
        return(result)
      } else {
        stop("API-Fehler: ", httr::status_code(response), " - ", httr::content(response, "text"))
      }
    },
    
    download_dataset = function(filename, output_path = NULL) {
      url <- paste0(self$base_url, "/download/", filename)
      
      if (is.null(output_path)) output_path <- file.path("data", filename)
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
      
      response <- httr::GET(
        url,
        httr::add_headers(.headers = self$get_headers()),
        httr::write_disk(output_path, overwrite = TRUE)
      )
      
      if (httr::status_code(response) == 200) {
        message("✓ Download: ", output_path)
        return(output_path)
      } else {
        stop("Download-Fehler: ", httr::status_code(response))
      }
    },
    
    wait_for_completion = function(query_id, check_interval = 30, max_wait_time = 3600) {
      start_time <- Sys.time()
      
      repeat {
        status <- self$get_query_status(query_id)
        
        if (status$status %in% c("completed", "finished")) {
          message("✓ Abfrage abgeschlossen")
          return(status)
        } else if (status$status == "failed") {
          stop("Abfrage fehlgeschlagen")
        }
        
        if (as.numeric(difftime(Sys.time(), start_time, units = "secs")) > max_wait_time) {
          stop("Timeout erreicht")
        }
        
        message("Status: ", status$status, " - warte...")
        Sys.sleep(check_interval)
      }
    }
  )
)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || x == "") y else x

# Debug-Funktion
debug_api_response <- function(client, query, name) {
  cat("=== DEBUG API RESPONSE ===\n")
  result <- client$submit_query(query = query, name = name, test = TRUE)
  cat("Response structure:\n")
  str(result)
  cat("==========================\n")
  return(result)
}
create_feminicide_query <- function(sources = c("ZWA", "ZWAS"), 
                                   date_from = "2023-01-01",
                                   date_to = "2023-12-31",
                                   languages = c("de", "fr"),
                                   max_results = 1000) {
  query_list <- list(
    query = list(
      sources = sources,
      dates = list(list(from = date_from, to = date_to)),
      languages = languages,
      content = list(AND = list(
        list(OR = list("Feminizid", "Femizid", "Frauenmord", "Femicide")),
        list(NOT = "Suizid")
      ))
    ),
    result = list(
      format = "TSV",
      maxResults = max_results,
      columns = c("id", "pubtime", "medium_name", "head", "content")
    ),
    version = "1.2"
  )
  yaml::as.yaml(query_list)
}

# Beispiel-Query aus der API-Dokumentation
create_example_query <- function() {
  query_yaml <- "
query:
    sources:
        - ZWA
        - ZWAS
    dates:
        - from: 2022-12-01
          to: 2022-12-31
    languages:
        - de
        - fr
    content:
        AND:
            - OR:
                - COVID
                - Corona
            - NOT: China
            - NOT: chin*
result:
    format: TSV
    maxResults: 100
    columns:
        - id
        - pubtime
        - medium_code
        - medium_name
        - rubric
        - regional
        - doctype
        - doctype_description
        - language
        - char_count
        - dateline
        - head
        - subhead
        - content_id
        - content
version: 1.2"
  return(query_yaml)
}

run_complete_workflow <- function(client, query, name, output_dir = "data") {
  result <- client$submit_query(query = query, name = name)
  
  query_id <- result$queryId %||% result$id
  if (is.null(query_id) || query_id == "") {
    stop("Keine Query-ID erhalten. Prüfen Sie die API-Response: ", str(result))
  }
  
  status <- client$wait_for_completion(query_id)
  
  if (!is.null(status$downloadUrl)) {
    filename <- basename(status$downloadUrl)
    return(client$download_dataset(filename, file.path(output_dir, filename)))
  } else {
    stop("Keine Download-URL verfügbar")
  }
}