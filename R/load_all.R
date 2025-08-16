# R/load_all.R
# Central loading function for all dependencies and source files
# FIXED: Removed non-existent module references for Linux/Mac compatibility

#' Load all required source files and dependencies
#' @param check_dependencies Whether to check package dependencies
#' @param install_missing Whether to install missing packages
#' @return invisible TRUE
#' @export
load_all_sources <- function(check_dependencies = TRUE, install_missing = FALSE) {
  
  # Load dependency management system
  dep_mgr_path <- file.path("R", "dependencies.R")
  if (file.exists(dep_mgr_path)) {
    source(dep_mgr_path, local = FALSE)
    message("✓ Loaded dependency management system")
  } else {
    stop("CRITICAL: dependencies.R not found - cannot manage package dependencies")
  }
  
  # Check and load dependencies
  if (check_dependencies) {
    tryCatch({
      load_dependencies(install_missing = install_missing)
      message("✓ All package dependencies satisfied")
    }, error = function(e) {
      stop(sprintf("Dependency check failed: %s", e$message))
    })
  }
  
  # CRITICAL: Load utils.R first - contains %||% operator and other utilities
  utils_path <- file.path("R", "utils.R")
  if (file.exists(utils_path)) {
    source(utils_path, local = FALSE)
    message("✓ Loaded core utilities: utils.R")
  } else {
    stop("CRITICAL: utils.R not found - this file contains essential utilities including %||% operator")
  }
  
  # Define source files in loading order
  # FIXED: Only reference files that actually exist
  source_files <- c(
    # Safe I/O operations (requires utils.R for %||%)
    "safe_io",
    
    # Database and authentication
    "db",  # Core database functions
    "auth",  # Authentication
    "audit",  # Audit logging
    
    # Error monitoring (now uses :: notation)
    "error_monitor",
    
    # Health checks
    "health_checks",
    
    # FHIR modules (require utils.R for %||%)
    "fhir_auth",
    "fhir_cache",
    "fhir_circuit",
    "fhir_connection",
    "fhir",
    
    # Core modules
    "antibiogram",
    "cf_resistance",
    
    # PK/PD specific
    "pk_models",
    "pk_calculations",
    
    # Analysis functions
    "run_fit_jags",  # Main fitting function
    "diagnostics",
    "optimize_regimen",
    
    # Plotting functions
    "plotting_functions",
    
    # Backend modules
    "backend_bayes",
    "parallel_utils",
    "async_fits",
    "job_queue",
    "pta_cfr",
    
    # Additional modules (if they exist)
    "cache_manager",
    "warmstart_manager",
    "optimization_utils",
    "units_checks",
    "ode_grid",
    "error_models",
    "crrt",
    "prior_db"
  )
  
  # Track loading results
  loading_results <- list(
    loaded = character(),
    skipped = character(),
    failed = character()
  )
  
  # Source all files with error handling
  for (file in source_files) {
    file_path <- file.path("R", paste0(file, ".R"))
    if (file.exists(file_path)) {
      tryCatch({
        source(file_path, local = FALSE)
        loading_results$loaded <- c(loading_results$loaded, file_path)
        message(paste("✓ Loaded:", file_path))
      }, error = function(e) {
        loading_results$failed <- c(loading_results$failed, file_path)
        warning(paste("✗ Failed to load:", file_path, "-", e$message))
      })
    } else {
      loading_results$skipped <- c(loading_results$skipped, file_path)
      # Only show info for important missing files
      if (file %in% c("pk_models", "backend_bayes", "db")) {
        message(paste("ℹ File not found (skipping):", file_path))
      }
    }
  }
  
  # Load all modules
  module_dir <- file.path("R", "modules")
  if (dir.exists(module_dir)) {
    module_files <- list.files(
      path = module_dir,
      pattern = "^mod_.*\\.R$",
      full.names = TRUE
    )
    
    for (module_file in module_files) {
      tryCatch({
        source(module_file, local = FALSE)
        loading_results$loaded <- c(loading_results$loaded, module_file)
        message(paste("✓ Loaded module:", basename(module_file)))
      }, error = function(e) {
        loading_results$failed <- c(loading_results$failed, module_file)
        warning(paste("✗ Failed to load module:", basename(module_file), "-", e$message))
      })
    }
  }
  
  # Summary
  message("\n=== Loading Summary ===")
  message(sprintf("✓ Loaded: %d files", length(loading_results$loaded)))
  if (length(loading_results$skipped) > 0) {
    message(sprintf("ℹ Skipped: %d files (not found)", length(loading_results$skipped)))
  }
  if (length(loading_results$failed) > 0) {
    message(sprintf("✗ Failed: %d files", length(loading_results$failed)))
    warning("Some files failed to load. Check warnings above for details.")
  }
  
  # Set global options
  options(
    stringsAsFactors = FALSE,
    scipen = 999,
    digits = 4,
    encoding = "UTF-8"  # Force UTF-8 encoding
  )
  
  invisible(TRUE)
}

# Auto-load if sourced directly
if (!interactive() || isTRUE(getOption("auto_load_sources"))) {
  load_all_sources(check_dependencies = FALSE)
}