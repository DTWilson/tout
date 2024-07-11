#' Find optimal progression criteria for a fixed sample size and continuous outcome
#'
#' Given a fixed sample size and a null and alternative hypothesis, this function
#' will determine upper and lower progression criteria such that alpha is controlled
#' exactly, beta is controlled or minimised when control is not feasible, and gamma
#' is minimised.
#'
#' @param n sample size.
#' @param rho_0 null hypothesis.
#' @param rho_1 alternative hypothesis.
#' @param sigma standard deviation of the outcome.
#' @param alpha_nom nominal upper constraint on alpha.
#' @param beta_nom nominal upper constraint on beta.
#' @param tau vector with lower and upper bounds of adjustment effect.
#' @param eta_0 probability of an incorrect go after an intermediate result. 
#' Defaults to 0.5.
#' @param eta_1 probability of an incorrect stop after an intermediate result. 
#' Defaults to eta_0.
#'
#' @return A numeric vector containing the sample size, lower decision threshold,
#' and upper decision threshold (or NA when no valid designs exist), and 
#' operating characteristics alpha, beta, gamma. Decision thresholds are on the 
#' z-statistic scale.
#' @export
#'
#' @examples
#' n <- 200
#' rho_0 <- 0
#' rho_1 <- 0.3
#' sigma <- 1
#' alpha_nom <- 0.05
#' beta_nom <- 0.1
#' 
#' opt_pc_cont(n, rho_0, rho_1, sigma, alpha_nom, beta_nom)
#' 
opt_pc_cont <- function(n, rho_0, rho_1, sigma, alpha_nom, beta_nom,
                        tau = c(0,0), eta_0 = 0.5, eta_1 = NULL){
  
  tau_min <- tau[1]
  tau_max <- tau[2]
  
  if(is.null(eta_1)) eta_1 <- eta_0
  
  if(n <= 0) return(null_design_cont(n, rho_0, rho_1, sigma, tau, eta_0, eta_1))
  
  # Check that the arguments are specified correctly
  check_arguments(n, alpha_nom, beta_nom, eta_0)
  check_arguments_cont(sigma)
  
  # Get minimum x_1 s.t. alpha can be controlled, and default max x_1
  min_x_1 <- min_x_1_cont(n, sigma, alpha_nom, tau_min, eta_0)
  max_x_1 <- max_x_1_cont(alpha_nom, eta_0, rho_0, rho_1, sigma, n)
  if(max_x_1 < min_x_1) return(null_design_cont(n, rho_0, rho_1, sigma, tau, eta_0, eta_1))
  
  # Find optimal choice of x_1 - this will be the largest value such that
  # beta ~ beta_nom
  # Use optimise() to find the best choice of x_1
  x_1 <- stats::optimize(opt_x_1_cont, lower = min_x_1, upper = max_x_1,
                         n=n, rho_0=rho_0, rho_1=rho_1, sigma=sigma, 
                         alpha_nom=alpha_nom, beta_nom=beta_nom, 
                         tau_min=tau_min, tau_max=tau_max, eta_0=eta_0, eta_1=eta_1)$minimum
  
  # Determine the corresponding x_0 to give alpha = alpha_nom
  x_0 <- opt_x_0_cont(x_1, n, sigma, alpha_nom, tau_min, eta_0)
  
  ocs <- get_ocs_cont_z(n, x_0, x_1, rho_0, rho_1, sigma, tau_min, tau_max, eta_0, eta_1)
  
  # Check if all constraints are (approximately) satisfied
  valid <- FALSE
  if(all(ocs[1:2] < (c(alpha_nom, beta_nom) + 0.001))){
    valid <- TRUE
  }
  
  structure(list(valid = valid, n = n, thresholds = c(x_0, x_1), 
                 alpha = ocs[1], beta = ocs[2], gamma = ocs[3],
                 hyps = c(rho_0, rho_1), sigma = sigma, tau = tau, eta = c(eta_0, eta_1)),
            class = "tout_design_cont")
}

#' @export
print.tout_design_cont <- function(x, ...){
  cat("Three-Outcome design (continuous outcome)\n")
  cat("\n")
  cat("Sample size:", x$n, "\n")
  cat("Decision thresholds:", x$thresholds, "\n")
  cat("\n")
  cat("alpha =", x$alpha, "\nbeta =", x$beta, "\ngamma =", x$gamma, "\n")
  cat("\n")
  cat("Hypotheses:", x$hyps[1], "(null),", x$hyps[2], "(alternative)\n")
  cat("Satndard deviation:", x$sigma, "\n")
  cat("Modification effect range:", x$tau, "\n")
  cat("Error probability following an intermediate result:", x$eta, "\n")
}

min_x_1_cont <- function(n, sigma, alpha_nom, tau_min, eta_0){
  # For given n, find the minimum x_1 which can lead to a valid choice of
  # x_0 (i.e. one which will give alpha <= alpha_nom).
  
  # For the first element, under rho = rho_0:
  min1 <- stats::qnorm(1 - alpha_nom, mean = 0, sd = 1)
  
  # Get ncp for the sampling distribution under rho = rho_0 - tau_min
  ncp <- sqrt(n)*(- tau_min)/sigma
  min2 <- stats::qnorm((1 - 1/eta_0 + alpha_nom/eta_0)/(1 - 1/eta_0), mean = ncp, sd = 1)
  
  return(max(min1, min2))
}

max_x_1_cont <- function(alpha_nom, eta_0, rho_0, rho_1, sigma, n){
  if(eta_0 <= alpha_nom){
    stop("The probability of an error following in intermediate outcome should
         not be less than the nominal type I error rate.")
  } else {
    # Use a high upper quantile of the test statistic distribution under the alternative
    # hypothesis as the upper limit of x_1 since this will always mean a
    # type II error rate of at least eta*0.5
    return(stats::qnorm(0.99, mean = sqrt(n)*(rho_1 - rho_0)/sigma))
  }
}

opt_x_0_cont <- function(x_1, n, sigma, alpha_nom, tau_min, eta_0){
  # For given x_1 find the x_0 which best alpha_nom
  # Get ncp for the sampling distribution under rho = rho_0 - tau_min
  ncp <- sqrt(n)*(- tau_min)/sigma
  z <- 1/eta_0 - alpha_nom/eta_0 + (1 - 1/eta_0)*stats::pnorm(x_1, mean = ncp, sd = 1)
  x_0 <- stats::qnorm(z, mean = ncp, sd = 1)
  return(x_0)
}

opt_x_1_cont <- function(x_1, n, rho_0, rho_1, sigma, alpha_nom, beta_nom, tau_min, tau_max, eta_0, eta_1){
  x_0 <- opt_x_0_cont(x_1, n, sigma, alpha_nom, tau_min, eta_0)
  # Get corresponding error rates
  beta <- get_ocs_cont_z(n, x_0, x_1, rho_0, rho_1, sigma, tau_min, tau_max, eta_0, eta_1)[2]
  # Covert to objective function to be minimised
  to_min <- beta_objective(beta, beta_nom, x_0, x_1)
  return(to_min)
}

check_arguments_cont <- function(sigma){
  if(sigma < 0){
    stop("Standard deviation sigma is negative.")
  }
}

null_design_cont <- function(n, rho_0, rho_1, sigma, tau, eta_0, eta_1){
  structure(list(valid = FALSE, n = n, thresholds = c(NA, NA), 
                 alpha = NA, beta = NA, gamma = NA,
                 hyps = c(rho_0, rho_1), sigma = sigma, tau = tau, eta = c(eta_0, eta_1)),
            class = "tout_design_cont")
}