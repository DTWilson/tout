#' Find optimal progression criteria for a fixed sample size and continuous
#' outcome when an interval adjustment effect is known
#'
#' Given a fixed sample size and a null and alternative hypothesis, this function
#' will determine upper and lower progression criteria such that alpha is controlled
#' exactly, beta is minimised, and the width of the adjustment region is maximised.
#' Note that alpha and beta are defined taking into account an effect of adjustment 
#' which will be in the given range.
#' 
#' @param n Sample size.
#' @param rho_0 Null hypothesis.
#' @param rho_1 Alternative hypothesis.
#' @param alpha_nom Nominal upper constraint on alpha.
#' @param beta_nom Nominal upper constraint on beta.
#' @param sigma Standard deviation of outcome (in the continuous case).
#' @param tau_min 
#' @param tau_max 
#'
#' @return A numeric vector containing the sample size, lower decision threshold,
#' and upper decision threshold (or NA when no valid designs exist), and 
#' operating characteristics alpha, beta. Decision thresholds are on the outcome 
#' z-statistic scale.
#' @export
#' @export
#'
#' @examples
#' n <- 160
#' rho_0 <- 0
#' rho_1 <- 0.3
#' alpha_nom <- 0.05
#' beta_nom <- 0.2
#' sigma <- 1
#' tau_min <- 0.05
#' tau_max <- 0.1
#'
#' opt_pc_adjust_cont(n, rho_0, rho_1, alpha_nom, beta_nom, sigma, tau_min, tau_max)
opt_pc_adjust_cont <- function(n, rho_0, rho_1, alpha_nom, beta_nom, sigma, tau_min, tau_max){
  # Find x_1 such that alpha_1 = alpha_nom
  x_1 <- stats::qnorm(1 - alpha_nom)
  
  # Find x_0 such that alpha_2 = alpha_nom - need to choose min of domain properly
  x_0 <- stats::optimise(get_alpha_2_cont, lower = -10, upper = x_1,
                  x_1=x_1, alpha_nom=alpha_nom, sigma=sigma, n=n, 
                  tau_min=tau_min, tau_max=tau_max)$minimum
  
  # Find beta_1, which will be maximised at tau_max distance from rho_1
  beta <- stats::pnorm(x_0, mean = ((rho_1 - rho_0) - tau_max)*sqrt(n)/sigma)
  
  ocs <- c(alpha_nom, beta)
  
  if(all(ocs < (c(alpha_nom, beta_nom) + 0.0001))){
    design <- c(n, x_0, x_1)
  } else {
    design <- c(NA, NA, NA)
  }
  
  return(c(design, ocs))
}

get_alpha_2_cont <- function(x_0, x_1, alpha_nom, sigma, n, tau_min, tau_max){
  
  # Get mid point of the adjustment interval
  mid_point <- (x_0 + x_1)/2
  
  # alpha_2 will be maximised at this mid point, or as close to it as 
  # allowed by the range of tau.
  tau_ncp <- max(min(mid_point, -tau_min*sqrt(n)/sigma), -tau_max*sqrt(n)/sigma)
  
  alpha_2 <- stats::pnorm(x_1, mean = tau_ncp) - stats::pnorm(x_0, mean = tau_ncp)
  
  # Return squared difference to minimise
  return((alpha_2 - alpha_nom)^2)
}