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
#' opt_pc_cont(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom)
#' 
opt_pc_cont <- function(n, rho_0, rho_1, sigma, alpha_nom, beta_nom, gamma_nom){
  # Find the value of x_1 which gives a value of beta close to beta_nom
  x_1 <- stats::optimize(objective_f, interval = c(-5, 5),
           n=n, rho_0=rho_0, rho_1=rho_1, sigma=sigma, 
           alpha_nom=alpha_nom, beta_nom=beta_nom)$minimum
  
  # Determine the corresponding x_0 to give alpha = alpha_nom
  z <- 2 - 2*alpha_nom - stats::pnorm(x_1)
  x_0 <- stats::qnorm(2 - 2*alpha_nom - stats::pnorm(x_1))
  
  # Get operating characteristics and check if gamma_nom is satisfied
  ocs <- get_ocs_cont_z(n, x_0, x_1, rho_0, rho_1, sigma)

  if(ocs[3] <= gamma_nom){
    design <- c(n, x_0, x_1)
  } else {
    design <- NULL
  }

  return(design)
}

objective_f <- function(x_1, n, rho_0, rho_1, sigma, alpha_nom, beta_nom){
  z <- 2 - 2*alpha_nom - stats::pnorm(x_1)
  if(z <= 1){
    x_0 <- stats::qnorm(2 - 2*alpha_nom - stats::pnorm(x_1))
    if(x_0 <= x_1){
      ocs <- get_ocs_cont_z(n, x_0, x_1, rho_0, rho_1, sigma)
      return((ocs[2] - beta_nom)^2)
    } else {
      return(10)
    }
  } else {
    return(10)
  }
}

