# R/fhir.R
# FHIR client implementation with resilient connection handling
# Provides high-level interface to FHIR servers with authentication,
# automatic retries, caching, and circuit breaker protection.

# Load dependencies - FIXED: relative paths and case sensitivity
source("fhir_connection.R")
source("fhir_auth.R")
source("fhir_cache.R")

# Configuration
.fhir_config <- new.env(parent = emptyenv())
.fhir_config$base_url <- ""
.fhir_config$auth_token <- NULL
.fhir_config$headers <- list()
.fhir_config$version <- "R4"

#' Initialize FHIR client
#' 
#' @param base_url FHIR server base URL
#' @param auth_url OAuth2 authentication URL (optional)
#' @param client_id OAuth2 client ID
#' @param client_secret OAuth2 client secret
#' @param version FHIR version (R4, STU3, DSTU2)
#' @param cache_enabled Enable response caching
#' @return TRUE on successful initialization
#' @export
init_fhir_client <- function(base_url = NULL,
                            auth_url = NULL,
                            client_id = NULL,
                            client_secret = NULL,
                            version = "R4",
                            cache_enabled = TRUE) {
  
  # Use environment variable if base_url not provided
  if (is.null(base_url)) {
    base_url <- Sys.getenv("FHIR_BASE_URL", "")
  }
  
  if (!nzchar(base_url)) {
    stop("FHIR base URL is required. Set FHIR_BASE_URL environment variable or provide base_url parameter.")
  }
  
  # Validate URL format
  if (!grepl("^https?://", base_url)) {
    stop("FHIR base URL must start with http:// or https://")
  }
  
  # Remove trailing slash
  .fhir_config$base_url <- sub("/$", "", base_url)
  .fhir_config$version <- version
  
  # Initialize authentication if credentials provided
  if (!is.null(auth_url) || nzchar(Sys.getenv("FHIR_AUTH_URL"))) {
    init_fhir_auth(
      auth_url = auth_url,
      client_id = client_id,
      client_secret = client_secret
    )
  }
  
  # Configure caching
  if (cache_enabled) {
    configure_fhir_cache(
      default_ttl = 300,
      max_cache_size = 100,
      enable_compression = TRUE
    )
  }
  
  # Test connection
  if (test_fhir_connection()) {
    message(sprintf("FHIR client initialized successfully: %s", .fhir_config$base_url))
    return(TRUE)
  } else {
    warning("FHIR client initialized but connection test failed")
    return(FALSE)
  }
}

#' Test FHIR server connection
#' 
#' @return TRUE if server is reachable, FALSE otherwise
#' @export
test_fhir_connection <- function() {
  tryCatch({
    # Try to fetch server metadata
    response <- fhir_request_with_circuit_breaker(
      request_fn = httr::GET,
      url = sprintf("%s/metadata", .fhir_config$base_url),
      httr::add_headers(
        Accept = "application/fhir+json"
      ),
      max_retries = 1,
      timeout = 10
    )
    
    if (!is.null(response)) {
      content <- httr::content(response, as = "parsed", type = "application/json")
      
      # Verify it's a FHIR server
      if (!is.null(content$resourceType) && content$resourceType == "CapabilityStatement") {
        message(sprintf("Connected to FHIR server: %s", 
                       content$software$name %||% "Unknown"))
        return(TRUE)
      }
    }
    
    return(FALSE)
    
  }, error = function(e) {
    warning(sprintf("FHIR connection test failed: %s", e$message))
    return(FALSE)
  })
}

#' Get observations with full error handling and caching
#' 
#' @param patient_id Patient identifier (required)
#' @param code LOINC or other code for filtering
#' @param date_from Start date for filtering (Date or character)
#' @param date_to End date for filtering (Date or character)
#' @param use_cache Use cached results if available
#' @param include_all Fetch all pages of results
#' @return FHIR Bundle or NULL on error
#' @export
fhir_get_observations <- function(patient_id, 
                                 code = NULL,
                                 date_from = NULL,
                                 date_to = NULL,
                                 use_cache = TRUE,
                                 include_all = FALSE) {
  
  # Input validation
  if (missing(patient_id) || is.null(patient_id) || !nzchar(patient_id)) {
    stop("patient_id is required and must be non-empty")
  }
  
  # Ensure base URL is set
  if (!nzchar(.fhir_config$base_url)) {
    stop("FHIR client not initialized. Run init_fhir_client() first.")
  }
  
  # Build query parameters
  params <- list(
    patient = patient_id,
    _count = 100
  )
  
  if (!is.null(code)) {
    params$code <- code
  }
  
  if (!is.null(date_from)) {
    params$date <- paste0("ge", format(as.Date(date_from), "%Y-%m-%d"))
  }
  
  if (!is.null(date_to)) {
    if (!is.null(params$date)) {
      params$date <- paste0(params$date, "&le", format(as.Date(date_to), "%Y-%m-%d"))
    } else {
      params$date <- paste0("le", format(as.Date(date_to), "%Y-%m-%d"))
    }
  }
  
  # Check cache if enabled
  cache_key <- NULL
  if (use_cache && exists("get_fhir_cache")) {
    cache_key <- digest::digest(list("Observation", params))
    cached <- get_fhir_cache(cache_key)
    if (!is.null(cached)) {
      message("Using cached FHIR response")
      return(cached)
    }
  }
  
  # Build URL
  url <- sprintf("%s/Observation", .fhir_config$base_url)
  
  # Add authentication headers if available
  headers <- httr::add_headers(
    Accept = "application/fhir+json"
  )
  
  if (!is.null(.fhir_config$auth_token)) {
    headers <- httr::add_headers(
      Accept = "application/fhir+json",
      Authorization = paste("Bearer", .fhir_config$auth_token)
    )
  }
  
  # Execute request
  response <- fhir_request_with_circuit_breaker(
    request_fn = httr::GET,
    url = url,
    query = params,
    headers
  )
  
  if (is.null(response)) {
    warning("Failed to fetch observations from FHIR server")
    return(NULL)
  }
  
  # Parse response
  bundle <- httr::content(response, as = "parsed", type = "application/json")
  
  # Handle pagination if requested
  if (include_all && !is.null(bundle$link)) {
    all_entries <- bundle$entry %||% list()
    
    while (TRUE) {
      next_link <- NULL
      for (link in bundle$link) {
        if (link$relation == "next") {
          next_link <- link$url
          break
        }
      }
      
      if (is.null(next_link)) break
      
      # Fetch next page
      response <- fhir_request_with_circuit_breaker(
        request_fn = httr::GET,
        url = next_link,
        headers
      )
      
      if (is.null(response)) break
      
      bundle <- httr::content(response, as = "parsed", type = "application/json")
      if (!is.null(bundle$entry)) {
        all_entries <- c(all_entries, bundle$entry)
      }
    }
    
    bundle$entry <- all_entries
  }
  
  # Cache response if enabled
  if (use_cache && !is.null(cache_key) && exists("set_fhir_cache")) {
    set_fhir_cache(cache_key, bundle)
  }
  
  bundle
}

#' Convert FHIR observations to TDM format
#' 
#' @param bundle FHIR Bundle containing observations
#' @return Data frame with TDM-compatible format
#' @export
fhir_observations_to_tdm <- function(bundle) {
  
  if (is.null(bundle) || is.null(bundle$entry)) {
    return(data.frame(
      patient_id = character(),
      datetime = character(),
      value = numeric(),
      unit = character(),
      code = character(),
      display = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Extract observations
  observations <- lapply(bundle$entry, function(entry) {
    obs <- entry$resource
    
    if (is.null(obs) || obs$resourceType != "Observation") {
      return(NULL)
    }
    
    # Extract key fields
    patient_id <- sub("Patient/", "", obs$subject$reference %||% "")
    datetime <- obs$effectiveDateTime %||% obs$issued %||% ""
    
    # Extract value (handle different value types)
    value <- NA_real_
    unit <- NA_character_
    
    if (!is.null(obs$valueQuantity)) {
      value <- obs$valueQuantity$value
      unit <- obs$valueQuantity$unit %||% obs$valueQuantity$code %||% ""
    } else if (!is.null(obs$valueCodeableConcept)) {
      value <- NA_real_
      unit <- obs$valueCodeableConcept$text %||% ""
    }
    
    # Extract code
    code <- ""
    display <- ""
    if (!is.null(obs$code$coding) && length(obs$code$coding) > 0) {
      code <- obs$code$coding[[1]]$code %||% ""
      display <- obs$code$coding[[1]]$display %||% ""
    }
    
    data.frame(
      patient_id = patient_id,
      datetime = datetime,
      value = value,
      unit = unit,
      code = code,
      display = display,
      stringsAsFactors = FALSE
    )
  })
  
  # Combine all observations
  do.call(rbind, observations[!sapply(observations, is.null)])
}