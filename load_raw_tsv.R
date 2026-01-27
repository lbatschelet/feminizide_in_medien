#!/usr/bin/env Rscript

# Load raw TSV files from /data into the environment.
# Mimics the API output format and creates checkpoints
# so Notebook.qmd skips API calls.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

data_dir <- "data"
tsv_files <- list.files(data_dir, pattern = "\\.tsv(\\.xz)?$", full.names = TRUE)

if (length(tsv_files) == 0) {
  stop("No .tsv(.xz) files found under /data.")
}

split_tsv_files <- function(files) {
  files_lower <- tolower(basename(files))
  second_idx <- which(str_detect(files_lower, "second|secondpass|second-pass|second_pass"))
  if (length(second_idx) == 0) {
    if (length(files) == 2) {
      files_sorted <- sort(files)
      message("WARN: No second-pass marker found; using sorted order.")
      return(list(first = files_sorted[1], second = files_sorted[2]))
    }
    stop("Could not identify second-pass TSV by name.")
  }
  second_files <- files[second_idx]
  first_files <- files[setdiff(seq_along(files), second_idx)]
  if (length(first_files) == 0) {
    stop("No first-pass TSV files found after split.")
  }
  list(first = first_files, second = second_files)
}

save_checkpoint <- function(obj, prefix, dir = "data", format = "qs2", compress = "xz") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  if (format == "qs2") {
    if (!requireNamespace("qs2", quietly = TRUE)) {
      stop("qs2 package not installed, cannot save .qs2 checkpoint")
    }
    file <- file.path(dir, paste0(prefix, "_", timestamp, ".qs2"))
    qs2::qs_save(obj, file)
  } else {
    file <- file.path(dir, paste0(prefix, "_", timestamp, ".rds"))
    saveRDS(obj, file, compress = compress)
  }
  message("Saved checkpoint: ", basename(file))
  file
}

load_tsv_files <- function(files) {
  bind_rows(lapply(files, function(file) {
    read_tsv(file, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
  }))
}

picked <- split_tsv_files(tsv_files)

message("Loading first-pass TSV(s): ", paste(basename(picked$first), collapse = ", "))
media_data <- load_tsv_files(picked$first)

message("Loading second-pass TSV(s): ", paste(basename(picked$second), collapse = ", "))
media_data_second <- load_tsv_files(picked$second)

message("media_data: ", nrow(media_data), " rows")
message("media_data_second: ", nrow(media_data_second), " rows")

assign("media_data", media_data, envir = .GlobalEnv)
assign("media_data_second", media_data_second, envir = .GlobalEnv)

save_checkpoint(media_data, "feminizid_raw_data")
save_checkpoint(media_data_second, "feminizid_raw_data_secondpass")
