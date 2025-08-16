# R/fhir_connection.R
# FHIR server connection management with circuit breaker pattern
# Handles retries, timeouts, and connection pooling

# Connection configuration
.fhir_conn_config <- new.env(parent = emptyenv())
.fhir_conn_config$max_retries <- 3
.fhir_conn_config$timeout <- 30
.fhir_conn_config$circuit_open <- FALSE
.fhir_conn_config$failure_count <- 0
.fhir_conn_config$failure_threshold <- 5
.fhir_conn_config$reset_timeout <- 60
.fhir_conn_config$last_failure_time <- NULL

#' Configure FHIR connection parameters
#' 
#' @param max_retries Maximum number of retry attempts
#' @param timeout Request timeout in seconds
#' @param failure_threshold Failures before opening circuit
#' @param reset_timeout Seconds before attempting reset
#' @export
configure_fhir_connection <- function(max_retries = 3, 
                                    timeout = 30,
                                    failure_threshold = 5,
                                    reset_timeout = 60) {
  
  .fhir_conn_config$max_retries <- max_retries
  .fhir_conn_config$timeout <- timeout
  .fhir_conn_config$failure_threshold <- failure_threshold
  .fhir_conn_config$reset_timeout <- reset_timeout
  
  invisible(TRUE)
}

#' Check if circuit breaker should be reset
#' 
#' @return TRUE if circuit can be reset
check_circuit_reset <- function() {
  if (!.fhir_conn_config$circuit_open) {
    return(TRUE)
  }
  
  if (is.null(.fhir_conn_config$last_failure_time)) {
    return(TRUE)
  }
  
  time_since_failure <- as.numeric(difftime(
    Sys.time(),
    .fhir_conn_config$last_failure_time,
    units = "secs"
  ))
  
  if (time_since_failure >= .fhir_conn_config$reset_timeout) {
    message("Circuit breaker: Attempting reset after timeout")
    .fhir_conn_config$circuit_open <- FALSE
    .fhir_conn_config$failure_count <- 0
    return(TRUE)
  }
  
  FALSE
}

#' Record connection failure
record_failure <- function() {
  .fhir_conn_config$failure_count <- .fhir_conn_config$failure_count + 1
  .fhir_conn_config$last_failure_time <- Sys.time()
  
  if (.fhir_conn_config$failure_count >= .fhir_conn_config$failure_threshold) {
    .fhir_conn_config$circuit_open <- TRUE
    warning(sprintf(
      "Circuit breaker OPEN after %d failures. Will retry in %d seconds.",
      .fhir_conn_config$failure_count,
      .fhir_conn_config$reset_timeout
    ))
  }
}

#' Record successful connection
record_success <- function() {
  if (.fhir_conn_config$failure_count > 0 || .fhir_conn_config$circuit_open) {
    message("Circuit breaker: Connection restored")
  }
  .fhir_conn_config$failure_count <- 0
  .fhir_conn_config$circuit_open <- FALSE
  .fhir_conn_config$last_failure_time <- NULL
}

#' Execute FHIR request with circuit breaker pattern
#' 
#' @param request_fn Function to execute (e.g., httr::GET)
#' @param ... Arguments to pass to request_fn
#' @param max_retries Override default max retries
#' @param timeout Override default timeout
#' @return Response object or NULL on failure
#' @export
fhir_request_with_circuit_breaker <- function(request_fn, ..., 
                                             max_retries = NULL,
                                             timeout = NULL) {
  
  # Check circuit breaker state
  if (.fhir_conn_config$circuit_open && !check_circuit_reset()) {
    warning("Circuit breaker is OPEN - request blocked")
    return(NULL)
  }
  
  # Use configured defaults if not overridden
  max_retries <- max_retries %||% .fhir_conn_config$max_retries
  timeout <- timeout %||% .fhir_conn_config$timeout
  
  # Attempt request with retries
  attempt <- 0
  last_error <- NULL
  
  while (attempt < max_retries) {
    attempt <- attempt + 1
    
    tryCatch({
      # Execute request with timeout
      response <- request_fn(
        ...,
        httr::timeout(timeout)
      )
      
      # Check response status
      if (httr::status_code(response) >= 500) {
        # Server error - might be temporary
        stop(sprintf("Server error: %d", httr::status_code(response)))
      }
      
      # Success
      record_success()
      return(response)
      
    }, error = function(e) {
      last_error <<- e
      
      if (attempt < max_retries) {
        # Exponential backoff
        wait_time <- 2^(attempt - 1)
        message(sprintf(
          "Request failed (attempt %d/%d): %s. Retrying in %d seconds...",
          attempt, max_retries, e$message, wait_time
        ))
        Sys.sleep(wait_time)
      }
    })
  }
  
  # All retries exhausted
  record_failure()
  warning(sprintf(
    "All retry attempts failed. Last error: %s",
    last_error$message
  ))
  
  NULL
}

#' Test FHIR server connectivity
#' 
#' @param base_url FHIR server base URL
#' @param endpoint Test endpoint (default: /metadata)
#' @return TRUE if server is reachable
#' @export
test_fhir_connectivity <- function(base_url, endpoint = "/metadata") {
  
  if (missing(base_url) || !nzchar(base_url)) {
    warning("Base URL is required")
    return(FALSE)
  }
  
  # Build full URL
  url <- paste0(sub("/$", "", base_url), endpoint)
  
  # Test with single attempt
  response <- fhir_request_with_circuit_breaker(
    httr::GET,
    url,
    httr::add_headers(Accept = "application/fhir+json"),
    max_retries = 1,
    timeout = 10
  )
  
  !is.null(response) && httr::status_code(response) == 200
}

#' Get circuit breaker status
#' 
#' @return List with circuit breaker state
#' @export
get_circuit_status <- function() {
  list(
    is_open = .fhir_conn_config$circuit_open,
    failure_count = .fhir_conn_config$failure_count,
    failure_threshold = .fhir_conn_config$failure_threshold,
    last_failure = .fhir_conn_config$last_failure_time,
    can_reset = check_circuit_reset()
  )
}

#' Reset circuit breaker manually
#' 
#' @export
reset_circuit_breaker <- function() {
  .fhir_conn_config$circuit_open <- FALSE
  .fhir_conn_config$failure_count <- 0
  .fhir_conn_config$last_failure_time <- NULL
  message("Circuit breaker manually reset")
  invisible(TRUE)
}

#' Create FHIR bundle request
#' 
#' @param resources List of resources to bundle
#' @param type Bundle type (batch, transaction, etc.)
#' @return Bundle object
#' @export
create_fhir_bundle <- function(resources, type = "batch") {
  
  if (!is.list(resources) || length(resources) == 0) {
    stop("Resources must be a non-empty list")
  }
  
  # Create bundle structure
  bundle <- list(
    resourceType = "Bundle",
    type = type,
    entry = lapply(resources, function(resource) {
      list(
        resource = resource,
        request = list(
          method = "POST",
          url = resource$resourceType
        )
      )
    })
  )
  
  bundle
}