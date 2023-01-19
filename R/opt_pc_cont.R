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
#' @param gamma_nom nominal upper constraint on gamma.
#' @param eta probability of an incorrect decision under the null or alternative
#' after an intermediate result. Defaults to 0.5.
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
#' gamma_nom <- 0.9
#' eta <- 0.4
#' 
#' opt_pc_cont(n, rho_0, rho_1, sigma, alpha_nom, beta_nom, gamma_nom, eta)
#' 
opt_pc_cont <- function(n, rho_0, rho_1, sigma, alpha_nom, beta_nom, gamma_nom, eta = 0.5){
  
  # Check that the arguments are specified correctly
  check_arguments(n, alpha_nom, beta_nom, gamma_nom, eta)
  check_arguments_cont(sigma)
  
  # Get minimum x_1 s.t. alpha can be controlled, and default max x_1
  min_x_1 <- min_x_1_cont(alpha_nom, eta)
  max_x_1 <- max_x_1_cont(alpha_nom, eta, rho_0, rho_1, sigma, n)
  if(max_x_1 < min_x_1) return(rep(NA, 6))
  
  # Find optimal choice of x_1 - this will be the largest value such that
  # beta ~ beta_nom
  # Use optimise() to find the best choice of x_1
  x_1 <- stats::optimize(opt_x_1_cont, lower = min_x_1, upper = max_x_1,
                         n=n, rho_0=rho_0, rho_1=rho_1, sigma=sigma, 
                         alpha_nom=alpha_nom, beta_nom=beta_nom, eta=eta)$minimum
  
  # Determine the corresponding x_0 to give alpha = alpha_nom
  x_0 <- opt_x_0_cont(x_1, alpha_nom, eta)
  
  ocs <- get_ocs_cont_z(n, x_0, x_1, rho_0, rho_1, sigma, eta)
  
  # Check if all constraints are (approximately) satisfied
  if(all(ocs < (c(alpha_nom, beta_nom, gamma_nom) + 0.0001))){
    design <- c(n, x_0, x_1)
  } else {
    design <- c(NA, NA, NA)
  }
  
  return(c(design, ocs))
}

min_x_1_cont <- function(alpha_nom, eta){
  # For given n, find the minimum x_1 which can lead to a valid choice of
  # x_0 (i.e. one which will give alpha <= alpha_nom).
  stats::qnorm((1 - 1/eta + alpha_nom/eta)/(1 - 1/eta), mean = 0, sd = 1)
}

max_x_1_cont <- function(alpha_nom, eta, rho_0, rho_1, sigma, n){
  # For given n, find the minimum x_1 which can lead to a valid choice of
  # x_0 (i.e. one which will give alpha <= alpha_nom).
  if(eta <= alpha_nom){
    stop("The probability of an error following in intermediate outcome should
         not be less than the nominal type I error rate.")
  } else {
    # Use a high upper quantile of the test statistic distribution under the alternative
    # hypothesis as the upper limit of x_1 since this will always mean a
    # type II error rate of at least eta*0.5
    return(stats::qnorm(0.99, mean = sqrt(n)*(rho_1 - rho_0)/sigma))
  }
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

check_arguments_cont <- function(sigma){
  if(sigma < 0){
    stop("Standard deviation sigma is negative.")
  }
}