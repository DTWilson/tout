min_x_1_bin <- function(n, rho_0, alpha_nom, tau_min, eta_0){
  if(eta_0 <= alpha_nom){
    stop("The probability of an error following in intermediate outcome should
         not be less than the nominal type I error rate.")
  } else {
    # For given n, find the minimum x_1 which can lead to a valid choice of
    # x_0 (i.e. one which will give alpha <= alpha_nom).
    #stats::qbinom((1 - alpha_nom - 1/eta_0 + alpha_nom/eta_0)/(1 - 1/eta_0), n, rho_0 - tau_min)
    stats::qbinom(1 - alpha_nom, n, rho_0)
  }
}

opt_x_0_bin <- function(x_1, n, rho_0, alpha_nom, tau_min, eta_0){
  # For given x_1 find the x_0 which best satisfies alpha_nom
  z <- 1/eta_0 - alpha_nom/eta_0 + (1 - 1/eta_0)*stats::pbinom(x_1, n, rho_0 - tau_min)
  x_0 <- stats::qbinom(z, n, rho_0 - tau_min)
  return(x_0)
}


