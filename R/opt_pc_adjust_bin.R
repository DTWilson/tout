#' Find optimal progression criteria for a fixed sample size and binary
#' outcome when an interval adjustment effect is known
#'
#' Given a fixed sample size and a null and alternative hypothesis, this function
#' will determine upper and lower progression criteria such that alpha is controlled, 
#' beta is minimised, and the width of the adjustment region is maximised.
#' Note that alpha and beta are defined taking into account an effect of adjustment 
#' which will be in the given range.
#' 
#' @param n sample size.
#' @param rho_0 null hypothesis.
#' @param rho_1 alternative hypothesis.
#' @param alpha_nom nominal upper constraint on alpha.
#' @param beta_nom nominal upper constraint on beta.
#' @param tau_min lower limit of adjustment effect.
#' @param tau_max Upper limit if adjustment effect.
#'
#' @return A numeric vector containing the sample size, lower decision threshold,
#' and upper decision threshold (or NA when no valid designs exist), and 
#' operating characteristics alpha, beta. Decision thresholds are on the outcome 
#' scale.
#' @export
#' @export
#'
#' @examples
#' n <- 70
#' rho_0 <- 0.3
#' rho_1 <- 0.5
#' alpha_nom <- 0.05
#' beta_nom <- 0.2
#' tau_min <- 0.05
#' tau_max <- 0.1
#'
#' opt_pc_adjust_bin(n, rho_0, rho_1, alpha_nom, beta_nom, tau_min, tau_max)
#' 
opt_pc_adjust_bin <- function(n, rho_0, rho_1, alpha_nom, beta_nom, tau_min, tau_max){
  # Find x_1 such that alpha_1 = alpha_nom
  x_1 <- stats::qbinom(1 - alpha_nom, n, rho_0)
  
  # Find x_0 such that alpha_2 = alpha_nom
  x_0 <- stats::qbinom(1 - alpha_nom, n, rho_0 - tau_min)
  
  # Find beta_1, which will be maximised at tau_max distance from rho_1
  beta <- stats::pbinom(x_0, n, rho_1 - tau_max)
  
  alpha <- max(1 - stats::pbinom(x_1, n, rho_0), 1 - stats::pbinom(x_0, n, rho_0 - tau_min) )
  
  ocs <- c(alpha_nom, beta)
  
  if(all(ocs < (c(alpha_nom, beta_nom) + 0.0001))){
    design <- c(n, x_0, x_1)
  } else {
    design <- c(NA, NA, NA)
  }
  
  return(c(design, ocs))
}