# R/audit.R
# Audit logging functions with cryptographic integrity
# Implements append-only hashchain for tamper detection

# Initialize audit environment
.audit_env <- new.env(parent = emptyenv())
.audit_env$initialized <- FALSE
.audit_env$hash_chain <- NULL
.audit_env$log_path <- NULL

#' Initialize audit logging system
#' 
#' @param log_path Path to audit log file
#' @param create_if_missing Create log file if it doesn't exist
#' @return TRUE on success
#' @export
init_audit <- function(log_path = NULL, create_if_missing = TRUE) {
  
  # Use environment variable or default
  if (is.null(log_path)) {
    log_path <- Sys.getenv("TDMX_AUDIT_PATH", "audit/audit_log.csv")
  }
  
  # Create directory if needed
  log_dir <- dirname(log_path)
  if (!dir.exists(log_dir) && create_if_missing) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Initialize or verify existing log
  if (file.exists(log_path)) {
    # Verify integrity of existing log
    if (!verify_audit_integrity(log_path)) {
      warning("Audit log integrity check failed!")
      return(FALSE)
    }
    
    # Load last hash for chain
    existing <- utils::read.csv(log_path, stringsAsFactors = FALSE)
    if (nrow(existing) > 0) {
      .audit_env$hash_chain <- existing$hash[nrow(existing)]
    }
  } else if (create_if_missing) {
    # Create new log file with header
    header <- data.frame(
      timestamp = character(),
      actor = character(),
      action = character(),
      resource = character(),
      details = character(),
      ip_address = character(),
      session_id = character(),
      hash = character(),
      stringsAsFactors = FALSE
    )
    utils::write.csv(header, log_path, row.names = FALSE)
    .audit_env$hash_chain <- "GENESIS"
  } else {
    warning(sprintf("Audit log not found: %s", log_path))
    return(FALSE)
  }
  
  .audit_env$log_path <- log_path
  .audit_env$initialized <- TRUE
  
  message(sprintf("Audit logging initialized: %s", log_path))
  TRUE
}

#' Append entry to audit log with hashchain
#' 
#' @param actor User or system performing action
#' @param action Action performed
#' @param resource Resource affected
#' @param details Additional details (JSON string)
#' @param ip_address Client IP address
#' @param session_id Session identifier
#' @return TRUE on success
#' @export
audit_append_hashchain <- function(actor, action, resource = NA, 
                                 details = NA, ip_address = NA, 
                                 session_id = NA) {
  
  # Check initialization
  if (!.audit_env$initialized) {
    if (!init_audit()) {
      warning("Failed to initialize audit logging")
      return(FALSE)
    }
  }
  
  # Create timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  # Build entry
  entry <- data.frame(
    timestamp = timestamp,
    actor = as.character(actor),
    action = as.character(action),
    resource = if (is.na(resource)) NA_character_ else as.character(resource),
    details = if (is.na(details)) NA_character_ else as.character(details),
    ip_address = if (is.na(ip_address)) NA_character_ else as.character(ip_address),
    session_id = if (is.na(session_id)) NA_character_ else as.character(session_id),
    stringsAsFactors = FALSE
  )
  
  # Calculate hash for this entry
  entry_string <- paste(
    timestamp,
    actor,
    action,
    resource,
    details,
    ip_address,
    session_id,
    .audit_env$hash_chain,
    sep = "|"
  )
  
  entry$hash <- digest::digest(entry_string, algo = "sha256")
  
  # Append to file with lock
  lock_file <- paste0(.audit_env$log_path, ".lock")
  
  # Simple file locking mechanism
  max_wait <- 5
  wait_time <- 0
  while (file.exists(lock_file) && wait_time < max_wait) {
    Sys.sleep(0.1)
    wait_time <- wait_time + 0.1
  }
  
  # Create lock
  writeLines("locked", lock_file)
  
  tryCatch({
    # Append entry
    utils::write.table(
      entry,
      file = .audit_env$log_path,
      append = TRUE,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      quote = TRUE
    )
    
    # Update chain
    .audit_env$hash_chain <- entry$hash
    
    TRUE
    
  }, error = function(e) {
    warning(sprintf("Failed to write audit log: %s", e$message))
    FALSE
    
  }, finally = {
    # Remove lock
    unlink(lock_file)
  })
}

#' Verify integrity of audit log
#' 
#' @param log_path Path to audit log file
#' @return TRUE if integrity check passes
#' @export
verify_audit_integrity <- function(log_path = NULL) {
  
  if (is.null(log_path)) {
    log_path <- .audit_env$log_path
  }
  
  if (!file.exists(log_path)) {
    warning(sprintf("Audit log not found: %s", log_path))
    return(FALSE)
  }
  
  # Read log
  log_data <- utils::read.csv(log_path, stringsAsFactors = FALSE)
  
  if (nrow(log_data) == 0) {
    return(TRUE)  # Empty log is valid
  }
  
  # Verify hash chain
  previous_hash <- "GENESIS"
  
  for (i in seq_len(nrow(log_data))) {
    row <- log_data[i, ]
    
    # Rebuild hash
    entry_string <- paste(
      row$timestamp,
      row$actor,
      row$action,
      ifelse(is.na(row$resource), "NA", row$resource),
      ifelse(is.na(row$details), "NA", row$details),
      ifelse(is.na(row$ip_address), "NA", row$ip_address),
      ifelse(is.na(row$session_id), "NA", row$session_id),
      previous_hash,
      sep = "|"
    )
    
    calculated_hash <- digest::digest(entry_string, algo = "sha256")
    
    if (calculated_hash != row$hash) {
      warning(sprintf("Hash mismatch at row %d", i))
      return(FALSE)
    }
    
    previous_hash <- row$hash
  }
  
  TRUE
}

#' Query audit log
#' 
#' @param actor Filter by actor
#' @param action Filter by action
#' @param from_date Start date (Date or character)
#' @param to_date End date (Date or character)
#' @param limit Maximum number of records
#' @return Data frame of audit entries
#' @export
query_audit_log <- function(actor = NULL, action = NULL, 
                          from_date = NULL, to_date = NULL,
                          limit = 1000) {
  
  if (!.audit_env$initialized) {
    if (!init_audit()) {
      warning("Failed to initialize audit logging")
      return(NULL)
    }
  }
  
  # Read log
  log_data <- utils::read.csv(.audit_env$log_path, stringsAsFactors = FALSE)
  
  if (nrow(log_data) == 0) {
    return(log_data)
  }
  
  # Convert timestamps
  log_data$timestamp <- as.POSIXct(log_data$timestamp, tz = "UTC")
  
  # Apply filters
  if (!is.null(actor)) {
    log_data <- log_data[log_data$actor == actor, ]
  }
  
  if (!is.null(action)) {
    log_data <- log_data[log_data$action == action, ]
  }
  
  if (!is.null(from_date)) {
    from_date <- as.POSIXct(from_date, tz = "UTC")
    log_data <- log_data[log_data$timestamp >= from_date, ]
  }
  
  if (!is.null(to_date)) {
    to_date <- as.POSIXct(to_date, tz = "UTC")
    log_data <- log_data[log_data$timestamp <= to_date, ]
  }
  
  # Apply limit
  if (nrow(log_data) > limit) {
    log_data <- utils::tail(log_data, limit)
  }
  
  log_data
}

#' Write audit entry to database (fallback to CSV)
#' 
#' @param actor User performing action
#' @param action Action performed
#' @param resource Resource affected
#' @param details Additional details
#' @return TRUE on success
#' @export
audit_write_to_db <- function(actor, action, resource = NULL, details = NULL) {
  
  # Try database first
  if (exists("db_write_audit") && is.function(db_write_audit)) {
    result <- tryCatch({
      db_write_audit(actor, action, resource, details)
    }, error = function(e) {
      warning(sprintf("Database audit write failed: %s", e$message))
      NULL
    })
    
    if (!is.null(result)) {
      return(TRUE)
    }
  }
  
  # Fallback to CSV with hashchain
  audit_append_hashchain(
    actor = actor,
    action = action,
    resource = resource,
    details = details,
    ip_address = Sys.getenv("REMOTE_ADDR", NA),
    session_id = Sys.getenv("SESSION_ID", NA)
  )
}