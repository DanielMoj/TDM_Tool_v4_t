# R/cf_resistance.R
# Cumulative Fraction of Response calculations
# Handles bacterial resistance and PK/PD target calculations

#' Calculate cumulative fraction of response
#' 
#' @param concentrations Vector of drug concentrations
#' @param mic_distribution Data frame with mic and prob columns
#' @param target PK/PD target (e.g., 40 for fT>MIC)
#' @param metric Type of metric ("time_above", "auc_mic", etc.)
#' @return CFR probability between 0 and 1
#' @export
calculate_cfr <- function(concentrations, mic_distribution, target = 40, metric = "time_above") {
  
  # Validate inputs
  if (length(concentrations) == 0) {
    warning("No concentrations provided")
    return(NA_real_)
  }
  
  if (is.null(mic_distribution) || nrow(mic_distribution) == 0) {
    warning("No MIC distribution provided")
    return(NA_real_)
  }
  
  if (!all(c("mic", "prob") %in% names(mic_distribution))) {
    stop("MIC distribution must have 'mic' and 'prob' columns")
  }
  
  # Calculate PTA for each MIC value
  pta_values <- numeric(nrow(mic_distribution))
  
  for (i in seq_len(nrow(mic_distribution))) {
    mic <- mic_distribution$mic[i]
    
    # Calculate metric based on type
    if (metric == "time_above") {
      # Calculate time above MIC
      time_above <- sum(concentrations > mic) / length(concentrations) * 100
      pta_values[i] <- as.numeric(time_above >= target)
      
    } else if (metric == "auc_mic") {
      # Calculate AUC/MIC ratio
      auc <- sum(concentrations) * (24 / length(concentrations))
      auc_mic_ratio <- auc / mic
      pta_values[i] <- as.numeric(auc_mic_ratio >= target)
      
    } else if (metric == "cmax_mic") {
      # Calculate Cmax/MIC ratio
      cmax_mic_ratio <- max(concentrations) / mic
      pta_values[i] <- as.numeric(cmax_mic_ratio >= target)
      
    } else {
      stop(sprintf("Unknown metric: %s", metric))
    }
  }
  
  # Calculate CFR as weighted sum of PTA values
  cfr <- sum(pta_values * mic_distribution$prob)
  
  # Ensure CFR is between 0 and 1
  cfr <- max(0, min(1, cfr))
  
  cfr
}

#' Calculate PTA for a single MIC value
#' 
#' @param concentrations Vector of drug concentrations
#' @param mic Single MIC value
#' @param target PK/PD target
#' @param metric Type of metric
#' @return PTA probability between 0 and 1
#' @export
calculate_pta <- function(concentrations, mic, target = 40, metric = "time_above") {
  
  # Create single-row MIC distribution
  mic_dist <- data.frame(
    mic = mic,
    prob = 1.0
  )
  
  # Use CFR calculation with single MIC
  calculate_cfr(concentrations, mic_dist, target, metric)
}

#' Get resistance breakpoints for a drug
#' 
#' @param drug Drug name
#' @param organism Organism name (optional)
#' @return Data frame with breakpoints
#' @export
get_resistance_breakpoints <- function(drug, organism = NULL) {
  
  # Default breakpoints (EUCAST/CLSI)
  breakpoints <- list(
    "Meropenem" = data.frame(
      organism = c("E. coli", "P. aeruginosa", "A. baumannii"),
      susceptible = c(2, 2, 2),
      resistant = c(8, 8, 8)
    ),
    "Vancomycin" = data.frame(
      organism = c("S. aureus", "E. faecalis", "E. faecium"),
      susceptible = c(2, 4, 4),
      resistant = c(2, 4, 4)
    ),
    "Piperacillin/Tazobactam" = data.frame(
      organism = c("E. coli", "P. aeruginosa", "Enterobacterales"),
      susceptible = c(8, 16, 8),
      resistant = c(16, 128, 16)
    )
  )
  
  # Get breakpoints for drug
  if (!drug %in% names(breakpoints)) {
    warning(sprintf("No breakpoints defined for drug: %s", drug))
    return(NULL)
  }
  
  bp <- breakpoints[[drug]]
  
  # Filter by organism if specified
  if (!is.null(organism)) {
    bp <- bp[bp$organism == organism, ]
    if (nrow(bp) == 0) {
      warning(sprintf("No breakpoints for %s against %s", drug, organism))
      return(NULL)
    }
  }
  
  bp
}

#' Categorize MIC value as S/I/R
#' 
#' @param mic MIC value
#' @param drug Drug name
#' @param organism Organism name
#' @return Character: "S", "I", or "R"
#' @export
categorize_susceptibility <- function(mic, drug, organism = NULL) {
  
  # Get breakpoints
  bp <- get_resistance_breakpoints(drug, organism)
  
  if (is.null(bp) || nrow(bp) == 0) {
    return(NA_character_)
  }
  
  # Use first breakpoint if multiple
  if (nrow(bp) > 1) {
    bp <- bp[1, ]
  }
  
  # Categorize
  if (mic <= bp$susceptible) {
    return("S")
  } else if (mic >= bp$resistant) {
    return("R")
  } else {
    return("I")
  }
}

#' Calculate resistance probability from MIC distribution
#' 
#' @param mic_distribution Data frame with mic and prob columns
#' @param drug Drug name
#' @param organism Organism name
#' @return List with probabilities for S, I, R
#' @export
calculate_resistance_probability <- function(mic_distribution, drug, organism = NULL) {
  
  if (is.null(mic_distribution) || nrow(mic_distribution) == 0) {
    return(list(S = NA_real_, I = NA_real_, R = NA_real_))
  }
  
  # Get breakpoints
  bp <- get_resistance_breakpoints(drug, organism)
  
  if (is.null(bp) || nrow(bp) == 0) {
    return(list(S = NA_real_, I = NA_real_, R = NA_real_))
  }
  
  # Use first breakpoint if multiple
  if (nrow(bp) > 1) {
    bp <- bp[1, ]
  }
  
  # Calculate probabilities
  prob_s <- sum(mic_distribution$prob[mic_distribution$mic <= bp$susceptible])
  prob_r <- sum(mic_distribution$prob[mic_distribution$mic >= bp$resistant])
  prob_i <- 1 - prob_s - prob_r
  
  list(
    S = prob_s,
    I = prob_i,
    R = prob_r
  )
}