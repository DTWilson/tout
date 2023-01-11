#' Find optimal progression criteria for a fixed sample size and binary outcome
#'
#' Given a fixed sample size and a null and alternative hypothesis, this function
#' will determine upper and lower progression criteria such that alpha is controlled
#' exactly, beta is controlled or minimised when control is not feasible, and gamma
#' is minimised.
#'
#' @param n Sample size.
#' @param rho_0 Null hypothesis.
#' @param rho_1 Alternative hypothesis.
#' @param alpha_nom Nominal upper constraint on alpha.
#' @param beta_nom Nominal upper constraint on beta.
#' @param gamma_nom Nominal upper constraint on gamma.
#' @param eta Probability of an incorrect decision under the null or alternative
#' after an intermediate result. Defaults to 0.5.
#' @param sigma Standard deviation of outcome (in the continuous case).
#' @param binary Logical. Is the outcome binary (TRUE, default) or continuous
#' (FALSE)?
#'
#' @return A numeric vector containing the sample size, lower decision threshold,
#' and upper decision threshold; or NULL when no valid designs exist. Decision 
#' thresholds are on the outcome (binary) or z-statistic (continuous) scale.
#' @export
#'
#' @examples
#' n <- 100
#' rho_0 <- 0.5
#' rho_1 <- 0.7
#' alpha_nom <- 0.05
#' beta_nom <- 0.2
#' gamma_nom <- 0.9
#'
#' opt_pc(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom)
#' 
#' n <- 200
#' rho_0 <- 0
#' rho_1 <- 0.3
#' alpha_nom <- 0.05
#' beta_nom <- 0.1
#' gamma_nom <- 0.9
#' eta <- 0.4
#' sigma <- 1
#'
#' opt_pc(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom, eta, sigma, binary = FALSE)
#' 
opt_pc <- function(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom, eta = 0.5, sigma = 1, binary = TRUE){
  
  # Check that the arguments are specified correctly
  check_arguments(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom, eta, sigma, binary)
  
  # Get minimum x_1 s.t. alpha can be controlled, and default max x_1
  if(binary){
    min_x_1 <- min_x_1_bin(n, rho_0, alpha_nom, eta)
    max_x_1 <- n
  } else {
    min_x_1 <- min_x_1_cont(alpha_nom, eta)
    # Use the mean of the test statistic distribution under the alternative
    # hypothesis as the upper limit of x_1
    max_x_1 <- sqrt(n)*(rho_1 - rho_0)/sigma
    if(max_x_1 < min_x_1) return(rep(NA, 6))
  }
  
  # Find optimal choice of x_1 - that which gives beta ~ beta_nom
  if(binary){
    # Run an exhaustive search over all possible choices of x_1
    x_1s <- min_x_1:max_x_1
    # For each, find the optimal x_0
    x_0s <- opt_x_0_bin(x_1s, n, rho_0, alpha_nom, eta)
    # Get resulting betas
    betas <- get_ocs_bin(n, x_0s, x_1s, rho_0, rho_1, eta)[,2]
    # Convert betas to a measure to be minimised when optimising
    to_min <- beta_objective(betas, beta_nom, x_0s, x_1s)
    x_0 <- x_0s[which.min(to_min)]
    x_1 <- x_1s[which.min(to_min)]
    
    ocs <- get_ocs_bin(n, x_0, x_1, rho_0, rho_1)
  } else {
    # Use optimise() to find the best choice of x_1
    x_1 <- stats::optimize(opt_x_1_cont, lower = min_x_1, upper = max_x_1,
                           n=n, rho_0=rho_0, rho_1=rho_1, sigma=sigma, 
                           alpha_nom=alpha_nom, beta_nom=beta_nom, eta=eta)$minimum
    
    # Determine the corresponding x_0 to give alpha = alpha_nom
    x_0 <- opt_x_0_cont(x_1, alpha_nom, eta)
    
    ocs <- get_ocs_cont_z(n, x_0, x_1, rho_0, rho_1, sigma, eta)
  }
  
  # Check if all constraints are (approximately) satisfied
  if(all(ocs < (c(alpha_nom, beta_nom, gamma_nom) + 0.0001))){
    design <- c(n, x_0, x_1)
  } else {
    design <- c(NA, NA, NA)
  }
  
  return(c(design, ocs))
}

# Functions for binary case

min_x_1_bin <- function(n, rho_0, alpha_nom, eta){
  # For given n, find the minimum x_1 which can lead to a valid choice of
  # x_0 (i.e. one which will give alpha <= alpha_nom).
  stats::qbinom((1 - 1/eta + alpha_nom/eta)/(1 - 1/eta), n, rho_0)
}

opt_x_0_bin <- function(x_1, n, rho_0, alpha_nom, eta){
  # For given x_1 find the x_0 which best satisfies alpha_nom
  z <- 1/eta - alpha_nom/eta + (1 - 1/eta)*stats::pbinom(x_1, n, rho_0)
  x_0 <- stats::qbinom(z, n, rho_0)
  return(x_0)
}

# Functions for continuous case

min_x_1_cont <- function(alpha_nom, eta){
  # For given n, find the minimum x_1 which can lead to a valid choice of
  # x_0 (i.e. one which will give alpha <= alpha_nom).
  stats::qnorm((1 - 1/eta + alpha_nom/eta)/(1 - 1/eta), mean = 0, sd = 1)
}

opt_x_0_cont <- function(x_1, alpha_nom, eta){
  # For given x_1 find the x_0 which best satisfies alpha_nom
  z <- 1/eta - alpha_nom/eta + (1 - 1/eta)*stats::pnorm(x_1, mean = 0, sd = 1)
  x_0 <- stats::qnorm(z, mean = 0, sd = 1)
  return(x_0)
}

opt_x_1_cont <- function(x_1, n, rho_0, rho_1, sigma, alpha_nom, beta_nom, eta){
  x_0 <- opt_x_0_cont(x_1, alpha_nom, eta)
  # Get corresponding error rates
  beta <- get_ocs_cont_z(n, x_0, x_1, rho_0, rho_1, sigma, eta)[2]
  # Covert to objective function to be minimised
  to_min <- beta_objective(beta, beta_nom, x_0, x_1)
  return(to_min)
}

# Common functions

beta_objective <- function(beta, beta_nom, x_0, x_1){
  # Take absolute value to minimise
  beta2 <- abs(beta - beta_nom)
  # Find largest x_1 while penalising (i) beta constrain violation, 
  # and (ii) cases where x_0 > x_1
  return(-x_1 + 1000*(beta > beta_nom) + (x_0 > x_1)*1000)
}

check_arguments <- function(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom, eta, sigma, binary){
  if(n < 0){
    stop("Sample size n is negative.")
  }
  
  if(alpha_nom < 0 | alpha_nom > 1){
    stop("Constraint alpha_nom is outside the [0, 1] interval.")
  }
  if(beta_nom < 0 | beta_nom > 1){
    stop("Constraint beta_nom is outside the [0, 1] interval.")
  }
  if(gamma_nom < 0 | gamma_nom > 1){
    stop("Constraint gamma_nom is outside the [0, 1] interval.")
  }
  
  if(eta < 0 | eta > 1){
    stop("Probability eta is outside the [0, 1] interval")
  }
  
  if(binary){
    if(rho_0 < 0 | rho_0 > 1){
      stop("Hypothesis rho_0 is outside the [0, 1] interval.")
    }
    if(rho_1 < 0 | rho_1 > 1){
      stop("Hypothesis rho_1 is outside the [0, 1] interval.")
    }
  } else {
    if(sigma < 0){
      stop("Standard deviation sigma is negative.")
    }
  }
}
