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


