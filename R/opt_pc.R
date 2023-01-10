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
#'
#' @return A numeric vector containing the sample size, lower decision threshold,
#' and upper decision threshold; or NULL when no valid designs exist. Decision 
#' thresholds are on the outcome scale.
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
opt_pc <- function(n, rho_0, rho_1, alpha_nom, beta_nom, gamma_nom){
  
  x_1s <- 0:n
  
  # For each possible x_1, find the x_0 which gives "exact" control of alpha.
  x_0s <- opt_x_0(x_1s, n, rho_0, alpha_nom)
  
  # Get cosponsoring error rates and extract beta
  ocs <- get_ocs(n, x_0s, x_1s, rho_0, rho_1)
  beta <- ocs[,2]
  
  # Convert this beta into an objective to minimise, penalising any beta
  # constraint violations and any cases where x_0 > x_1
  beta2 <- abs(beta - beta_nom)
  beta3 <- beta2 + (beta > beta_nom)*10 + (x_0s > x_1s)*10
  
  # Use this objective to determine the best x_1
  x_1 <- x_1s[which.min(beta3)]
  x_0 <- x_0s[which.min(beta3)]
  
  # Get operating characteristics and check if gamma_nom is satisfied
  ocs <- get_ocs(n, x_0, x_1, rho_0, rho_1)
  
  if(ocs[1] <= alpha_nom & ocs[2] <= beta_nom & ocs[3] <= gamma_nom){
    design <- c(n, x_0, x_1)
  } else {
    design <- NULL
  }
  
  return(design)
}

opt_x_0 <- function(x_1, n, rho_0, alpha_nom){
  # For given x_1 find the x_0 which best satisfies alpha_nom
  z <- 2 - 2*alpha_nom - stats::pbinom(x_1, n, rho_0)
  x_0 <- stats::qbinom(z, n, rho_0)
  return(x_0)
}
