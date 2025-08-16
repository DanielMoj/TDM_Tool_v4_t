# R/antibiogram.R
# Antibiogram data management functions
# Handles MIC distributions from lab data

#' Read antibiogram CSV file with normalization
#' 
#' @param path Path to CSV file
#' @return Data frame with drug, mic, prob columns
#' @export
read_antibiogram_csv <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Antibiogram file not found: %s", path))
  }
  
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  
  # Validate required columns
  required <- c("drug", "mic", "prob")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
  }
  
  # Convert data types
  df$drug <- as.character(df$drug)
  df$mic <- as.numeric(df$mic)
  df$prob <- as.numeric(df$prob)
  
  # Remove invalid rows
  df <- df[!is.na(df$mic) & !is.na(df$prob) & df$prob > 0, ]
  
  # Normalize probabilities per drug
  if (nrow(df) > 0) {
    df <- do.call(rbind, lapply(split(df, df$drug), function(x) {
      x$prob <- x$prob / sum(x$prob)
      x
    }))
    rownames(df) <- NULL
  }
  
  df
}

#' Import antibiogram data to database
#' 
#' @param path Path to CSV file
#' @param source Source identifier
#' @param version Optional version string
#' @return Import result or NULL on failure
#' @export
antibiogram_import_to_db <- function(path, source = "upload", version = NULL) {
  # Validate input file
  if (!file.exists(path)) {
    stop("Import file not found: ", path)
  }
  
  # Read and normalize CSV
  df <- tryCatch({
    read_antibiogram_csv(path)
  }, error = function(e) {
    stop("Failed to read antibiogram CSV: ", e$message)
  })
  
  if (nrow(df) == 0) {
    warning("No valid data to import")
    return(NULL)
  }
  
  # Import to database using template
  result <- db_import_antibiogram(df, source = source, version = version)
  
  if (is.null(result)) {
    warning("Database import failed - check connection and logs")
    return(NULL)
  }
  
  message(sprintf("Successfully imported antibiogram data: %d rows", result$rows))
  result
}

#' Get list of drugs from database
#' 
#' @return Character vector of drug names
#' @export
antibiogram_drugs_db <- function() {
  # Use template function for safe DB access
  drugs <- db_list_antibiogram_drugs()
  
  if (is.null(drugs)) {
    warning("Could not retrieve drug list from database")
    return(character(0))
  }
  
  drugs
}

#' Get antibiogram data for a specific drug
#' 
#' @param drug Drug name
#' @return Data frame with mic and prob columns
#' @export
antibiogram_from_db <- function(drug) {
  if (!is.character(drug) || !nzchar(drug)) {
    stop("Valid drug name required")
  }
  
  # Use template function for safe DB access
  df <- db_get_antibiogram(drug)
  
  if (is.null(df)) {
    warning(sprintf("Could not retrieve antibiogram data for drug: %s", drug))
    return(data.frame(drug = character(0), mic = numeric(0), prob = numeric(0)))
  }
  
  # Ensure probabilities sum to 1
  if (nrow(df) > 0) {
    total_prob <- sum(df$prob)
    if (abs(total_prob - 1) > 0.01) {
      warning(sprintf("Probabilities for %s sum to %.3f, normalizing", drug, total_prob))
      df$prob <- df$prob / total_prob
    }
  }
  
  df
}

#' Get all antibiogram data from database
#' 
#' @return Data frame with all antibiogram data
#' @export
antibiogram_all_from_db <- function() {
  # Use template function for safe DB access
  df <- db_get_antibiogram(NULL)
  
  if (is.null(df)) {
    warning("Could not retrieve antibiogram data from database")
    return(data.frame(drug = character(0), mic = numeric(0), prob = numeric(0)))
  }
  
  df
}

#' Check if antibiogram data exists in database
#' 
#' @return Logical indicating if data exists
#' @export
antibiogram_exists_db <- function() {
  result <- db_has_antibiogram_data()
  isTRUE(result)
}

#' Get latest version info for antibiogram data
#' 
#' @return List with version information
#' @export
antibiogram_version_info <- function() {
  info <- db_get_latest_version("antibiogram")
  
  if (is.null(info)) {
    return(list(
      has_data = FALSE,
      version = NA_character_,
      updated = NA_character_,
      rows = 0
    ))
  }
  
  list(
    has_data = TRUE,
    version = info$version,
    updated = format(info$created_at, "%Y-%m-%d %H:%M:%S"),
    rows = info$meta$rows %||% 0,
    drugs = info$meta$drugs %||% 0
  )
}

#' Clean up old antibiogram entries
#' 
#' @return Number of cleaned entries
#' @export
antibiogram_cleanup <- function() {
  result <- db_cleanup_antibiogram()
  
  if (is.null(result)) {
    warning("Could not clean up antibiogram data")
    return(0)
  }
  
  result
}

#' Parse MIC distribution text (UI input format)
#' 
#' @param txt Text in format "mic1:prob1, mic2:prob2, ..."
#' @return Data frame with mic and prob columns
#' @export
parse_mic_distribution <- function(txt) {
  # Handle empty input
  if (is.null(txt) || !nzchar(txt)) {
    return(NULL)
  }
  
  # Split by comma
  parts <- trimws(unlist(strsplit(txt, ",")))
  
  # Parse each "mic:prob" pair
  vals <- lapply(parts, function(p) {
    kv <- trimws(unlist(strsplit(p, ":")))
    if (length(kv) != 2) {
      warning(sprintf("Invalid MIC:prob pair: %s", p))
      return(c(NA, NA))
    }
    c(as.numeric(kv[1]), as.numeric(kv[2]))
  })
  
  # Convert to matrix
  if (length(vals) == 0) return(NULL)
  vals <- do.call(rbind, vals)
  
  # Create data frame
  df <- data.frame(
    mic = vals[,1],
    prob = vals[,2]
  )
  
  # Remove invalid entries
  df <- df[!is.na(df$mic) & !is.na(df$prob) & df$prob > 0, ]
  
  # Normalize probabilities
  if (nrow(df) > 0) {
    df$prob <- df$prob / sum(df$prob)
  }
  
  df
}

#' Build code parameter for FHIR queries
#' 
#' @param codes Character vector of codes
#' @return URL-encoded parameter string
#' @export
.fhir_build_code_param <- function(codes) {
  if (length(codes) == 0) return("")
  
  # Format as comma-separated list
  param <- paste(codes, collapse = ",")
  
  # URL encode
  utils::URLencode(param)
}