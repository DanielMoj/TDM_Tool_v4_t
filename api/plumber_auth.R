# api/plumber_auth.R
# Authentication endpoints and middleware for Plumber API
# FIXED: Corrected source paths for Linux/Mac compatibility

# Load required modules from parent directory
source("../R/auth.R")
source("../R/db.R")
source("../R/utils.R")

# Session storage
.sessions <- new.env(parent = emptyenv())
.auth_cache <- new.env(parent = emptyenv())

# Configuration
AUTH_CONFIG <- list(
  session_timeout = as.numeric(Sys.getenv("AUTH_SESSION_TIMEOUT", 3600)),
  max_attempts = as.numeric(Sys.getenv("AUTH_MAX_ATTEMPTS", 5)),
  lockout_duration = as.numeric(Sys.getenv("AUTH_LOCKOUT_DURATION", 900)),
  require_2fa = as.logical(Sys.getenv("AUTH_REQUIRE_2FA", FALSE)),
  jwt_secret = Sys.getenv("AUTH_JWT_SECRET", "change-me-in-production")
)

# Rate limiting storage
.rate_limits <- new.env(parent = emptyenv())

#' Initialize authentication system
#' @export
init_auth_system <- function() {
  # Initialize auth module
  if (exists("auth_init") && is.function(auth_init)) {
    auth_init()
  }
  
  # Clean up expired sessions periodically
  later::later(cleanup_expired_sessions, delay = 300, loop = TRUE)
  
  message("Authentication system initialized")
  invisible(TRUE)
}

#' Check rate limiting
#' @param identifier IP or user identifier
#' @param action Action being performed
#' @return TRUE if allowed, FALSE if rate limited
check_rate_limit <- function(identifier, action = "login") {
  key <- paste(identifier, action, sep = ":")
  
  # Get current attempts
  attempts <- .rate_limits[[key]]
  
  if (is.null(attempts)) {
    attempts <- list(
      count = 0,
      first_attempt = Sys.time(),
      locked_until = NULL
    )
  }
  
  # Check if currently locked
  if (!is.null(attempts$locked_until)) {
    if (Sys.time() < attempts$locked_until) {
      return(FALSE)
    } else {
      # Reset after lockout expires
      attempts <- list(
        count = 0,
        first_attempt = Sys.time(),
        locked_until = NULL
      )
    }
  }
  
  # Reset if window expired (1 hour)
  if (difftime(Sys.time(), attempts$first_attempt, units = "secs") > 3600) {
    attempts <- list(
      count = 1,
      first_attempt = Sys.time(),
      locked_until = NULL
    )
  } else {
    attempts$count <- attempts$count + 1
    
    # Lock if max attempts exceeded
    if (attempts$count >= AUTH_CONFIG$max_attempts) {
      attempts$locked_until <- Sys.time() + AUTH_CONFIG$lockout_duration
    }
  }
  
  .rate_limits[[key]] <- attempts
  
  return(attempts$count < AUTH_CONFIG$max_attempts)
}

#' Clean up expired sessions
cleanup_expired_sessions <- function() {
  current_time <- Sys.time()
  sessions <- ls(.sessions)
  
  for (session_id in sessions) {
    session <- .sessions[[session_id]]
    if (!is.null(session$expires) && session$expires < current_time) {
      rm(list = session_id, envir = .sessions)
    }
  }
  
  invisible(TRUE)
}

#' Generate secure session token
#' @return Session token string
generate_session_token <- function() {
  # Generate cryptographically secure random token
  random_bytes <- openssl::rand_bytes(32)
  openssl::base64_encode(random_bytes)
}

#' Create session for authenticated user
#' @param username Username
#' @param role User role
#' @param metadata Additional session metadata
#' @return Session ID
create_session <- function(username, role = "user", metadata = list()) {
  session_id <- generate_session_token()
  
  .sessions[[session_id]] <- list(
    username = username,
    role = role,
    created = Sys.time(),
    expires = Sys.time() + AUTH_CONFIG$session_timeout,
    last_activity = Sys.time(),
    metadata = metadata
  )
  
  session_id
}

#' Validate session token
#' @param session_id Session token
#' @return Session data or NULL if invalid
validate_session <- function(session_id) {
  if (is.null(session_id) || !nzchar(session_id)) {
    return(NULL)
  }
  
  session <- .sessions[[session_id]]
  
  if (is.null(session)) {
    return(NULL)
  }
  
  # Check expiration
  if (!is.null(session$expires) && session$expires < Sys.time()) {
    rm(list = session_id, envir = .sessions)
    return(NULL)
  }
  
  # Update last activity
  session$last_activity <- Sys.time()
  .sessions[[session_id]] <- session
  
  session
}

#' Authentication middleware for Plumber
#' @param req Request object
#' @param res Response object
#' @export
auth_middleware <- function(req, res) {
  # Skip auth for public endpoints
  public_paths <- c("/", "/health", "/login", "/api/docs")
  if (req$PATH_INFO %in% public_paths) {
    return(forward())
  }
  
  # Extract token from header or cookie
  auth_header <- req$HTTP_AUTHORIZATION
  session_id <- NULL
  
  if (!is.null(auth_header) && grepl("^Bearer ", auth_header)) {
    session_id <- sub("^Bearer ", "", auth_header)
  } else if (!is.null(req$cookies$session)) {
    session_id <- req$cookies$session
  }
  
  # Validate session
  session <- validate_session(session_id)
  
  if (is.null(session)) {
    res$status <- 401
    return(list(
      error = "Unauthorized",
      message = "Valid authentication required"
    ))
  }
  
  # Add user info to request
  req$user <- list(
    username = session$username,
    role = session$role,
    session_id = session_id
  )
  
  forward()
}

#' Login endpoint
#' @param req Request object
#' @param res Response object
#' @param username Username
#' @param password Password
#' @param totp Optional TOTP code for 2FA
#' @post /login
#' @export
login_endpoint <- function(req, res, username, password, totp = NULL) {
  # Get client IP
  ip <- req$HTTP_X_FORWARDED_FOR %||% req$REMOTE_ADDR %||% "unknown"
  
  # Check rate limiting
  if (!check_rate_limit(ip, "login")) {
    res$status <- 429
    return(list(
      error = "Too Many Requests",
      message = "Too many login attempts. Please try again later."
    ))
  }
  
  # Validate credentials
  auth_result <- tryCatch({
    authenticate_user(username, password)
  }, error = function(e) {
    list(success = FALSE, message = e$message)
  })
  
  if (!auth_result$success) {
    # Log failed attempt
    .log_auth_event(username, ip, "login_failed", FALSE, auth_result$message)
    
    res$status <- 401
    return(list(
      error = "Authentication Failed",
      message = "Invalid username or password"
    ))
  }
  
  # Check 2FA if required
  if (AUTH_CONFIG$require_2fa) {
    if (is.null(totp) || !verify_totp(username, totp)) {
      res$status <- 401
      return(list(
        error = "2FA Required",
        message = "Valid TOTP code required"
      ))
    }
  }
  
  # Create session
  session_id <- create_session(username, auth_result$role)
  
  # Set session cookie
  res$setCookie(
    "session",
    session_id,
    httpOnly = TRUE,
    secure = TRUE,
    sameSite = "Strict",
    maxAge = AUTH_CONFIG$session_timeout
  )
  
  # Log successful login
  .log_auth_event(username, ip, "login", TRUE, 
                 sprintf("role=%s,session=%s", auth_result$role, substr(session_id, 1, 8)))
  
  # Audit log for successful login - FIXED: corrected path
  source("../R/audit.R")
  audit_append_hashchain(
    actor = username,
    action = "user_login",
    resource = "auth_system",
    details = jsonlite::toJSON(list(
      ip = ip,
      role = auth_result$role,
      session_start = substr(session_id, 1, 8)
    ), auto_unbox = TRUE),
    ip_address = ip,
    session_id = session_id
  )
  
  list(
    success = TRUE,
    session_id = session_id,
    role = auth_result$role,
    expires_in = AUTH_CONFIG$session_timeout
  )
}

#' Logout endpoint
#' @param req Request object
#' @param res Response object
#' @post /logout
#' @export
logout_endpoint <- function(req, res) {
  # Get session from request (added by middleware)
  if (is.null(req$user) || is.null(req$user$session_id)) {
    res$status <- 400
    return(list(
      error = "Bad Request",
      message = "No active session"
    ))
  }
  
  session_id <- req$user$session_id
  username <- req$user$username
  
  # Remove session
  if (exists(session_id, envir = .sessions)) {
    rm(list = session_id, envir = .sessions)
  }
  
  # Clear cookie
  res$setCookie(
    "session",
    "",
    httpOnly = TRUE,
    secure = TRUE,
    sameSite = "Strict",
    maxAge = 0
  )
  
  # Log logout
  ip <- req$HTTP_X_FORWARDED_FOR %||% req$REMOTE_ADDR %||% "unknown"
  .log_auth_event(username, ip, "logout", TRUE, "")
  
  # Audit log - FIXED: corrected path
  source("../R/audit.R")
  audit_append_hashchain(
    actor = username,
    action = "user_logout",
    resource = "auth_system",
    details = jsonlite::toJSON(list(
      session_end = substr(session_id, 1, 8)
    ), auto_unbox = TRUE),
    ip_address = ip,
    session_id = session_id
  )
  
  list(
    success = TRUE,
    message = "Logged out successfully"
  )
}

#' Get current user info
#' @param req Request object
#' @get /me
#' @export
get_user_info <- function(req) {
  if (is.null(req$user)) {
    return(list(
      authenticated = FALSE
    ))
  }
  
  list(
    authenticated = TRUE,
    username = req$user$username,
    role = req$user$role
  )
}

#' Check authorization for role
#' @param req Request object
#' @param required_role Required role
#' @return TRUE if authorized
#' @export
check_authorization <- function(req, required_role) {
  if (is.null(req$user)) {
    return(FALSE)
  }
  
  user_role <- req$user$role
  
  # Role hierarchy
  role_levels <- c("viewer" = 1, "user" = 2, "admin" = 3, "superadmin" = 4)
  
  user_level <- role_levels[user_role] %||% 0
  required_level <- role_levels[required_role] %||% 99
  
  user_level >= required_level
}

#' Log authentication event
#' @param username Username
#' @param ip IP address
#' @param event Event type
#' @param success Success status
#' @param details Additional details
.log_auth_event <- function(username, ip, event, success, details = "") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  
  log_entry <- sprintf(
    "[%s] AUTH %s: user=%s ip=%s success=%s %s",
    timestamp,
    toupper(event),
    username,
    ip,
    success,
    details
  )
  
  # Write to auth log file
  auth_log_file <- Sys.getenv("AUTH_LOG_FILE", "logs/auth.log")
  
  if (!dir.exists(dirname(auth_log_file))) {
    dir.create(dirname(auth_log_file), recursive = TRUE, showWarnings = FALSE)
  }
  
  cat(log_entry, "\n", file = auth_log_file, append = TRUE)
  
  invisible(TRUE)
}

# Initialize on load
init_auth_system()