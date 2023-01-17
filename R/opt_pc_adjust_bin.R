opt_pc_adjust_bin <- function(n, rho_0, rho_1, alpha_nom, beta_nom, tau_min, tau_max){
  # Find x_1 such that alpha_1 = alpha_nom
  x_1 <- stats::qbinom(1 - alpha_nom, n, rho_0)
  
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

get_alpha_2_bin <- function(x_0, x_1, alpha_nom, sigma, n, tau_min, tau_max){
  
  # Get mid point of the adjustment interval
  mid_point <- (x_0 + x_1)/2
  
  # alpha_2 will be maximised at this mid point, or as close to it as 
  # allowed by the range of tau.
  tau_ncp <- max(min(mid_point, -tau_min*sqrt(n)/sigma), -tau_max*sqrt(n)/sigma)
  
  alpha_2 <- stats::pnorm(x_1, mean = tau_ncp) - stats::pnorm(x_0, mean = tau_ncp)
  
  # Return squared difference to minimise
  return((alpha_2 - alpha_nom)^2)
}