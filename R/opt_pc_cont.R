#' Find optimal progression criteria for a fixed sample size and continuous outcome
#'
#' Given a fixed sample size and a null and alternative hypothesis, this function
#' will determine upper and lower progression criteria such that alpha is controlled
#' exactly, beta is controlled or minimised when control is not feasible, and gamma
#' is minimised.
#'
#' @param n Sample size.
#' @param rho_0 Null hypothesis.
#' @param rho_1 Alternative hypothesis.
#' @param sigma Standard deviation of outcome.
#' @param alpha_nom Nominal upper constraint on alpha.
#' @param beta_nom Nominal upper constraint on beta.
#' @param gamma_nom Nominal upper constraint on gamma.
#' @param eta Probability of an incorrect decision under the null or alternative
#' after an intermediate result. Defaults to 0.5.
#'
#' @return A numeric vector containing the sample size, lower decision threshold,
#' and upper decision threshold; or NULL when no valid designs exist. Decision 
#' thresholds are on the Z-statistic scale.
#' @export
#'
#' @examples
#' n <- 100
#' rho_0 <- 0
#' rho_1 <- 0.3
#' sigma <- 1
#' alpha_nom <- 0.05
#' beta_nom <- 0.2
#' gamma_nom <- 0.9
#'
#' opt_pc_cont(n, rho_0, rho_1, sigma, alpha_nom, beta_nom, gamma_nom)
#' 
opt_pc_cont <- function(n, rho_0, rho_1, sigma, alpha_nom, beta_nom, gamma_nom, eta = 0.5){
  
  # Find the value of x_1 which gives a value of beta close to beta_nom
  x_1 <- stats::optimize(beta_objective, interval = c(min_x_1_cont(n, alpha_nom), 5),
           n=n, rho_0=rho_0, rho_1=rho_1, sigma=sigma, 
           alpha_nom=alpha_nom, beta_nom=beta_nom)$minimum
  
  # Determine the corresponding x_0 to give alpha = alpha_nom
  z <- 1/eta - alpha_nom/eta + (1 - 1/eta)*stats::pnorm(x_1)
  x_0 <- stats::qnorm(z, mean = 0, sd = 1)

  # Get operating characteristics and check if gamma_nom is satisfied
  ocs <- get_ocs_cont_z(n, x_0, x_1, rho_0, rho_1, sigma)

  if(ocs[3] <= gamma_nom){
    design <- c(n, x_0, x_1)
  } else {
    design <- NULL
  }

  return(design)
}

min_x_1_cont <- function(n, alpha_nom, eta = 0.5){
  # For given n, find the minimum x_1 which can lead to a valid choice of
  # x_0 (i.e. one which will give alpha <= alpha_nom).
  stats::qnorm((1 - 1/eta + alpha_nom/eta)/(1 - 1/eta), mean = 0, sd = 1)
}


beta_objective <- function(x_1, n, rho_0, rho_1, sigma, alpha_nom, beta_nom, eta = 0.5){
  # Objective function to minimise when choosing x_1 to give beta = beta_nom
  # Find x_0 which gives alpha = alpha_nom
  z <- 1/eta - alpha_nom/eta + (1 - 1/eta)*stats::pnorm(x_1)
  x_0 <- stats::qnorm(z, mean = 0, sd = 1)
  # Get corresponding error rates
  beta <- get_ocs_cont_z(n, x_0, x_1, rho_0, rho_1, sigma)[2]
  # Penalise if x_0 > x_1
  beta2 <- abs(beta - beta_nom)
  return(beta2 + (beta > beta_nom)*10 + (x_0 > x_1)*10)
}

