get_ocs <- function(n, x_0, x_1, rho_0, rho_1){
  # Calculate the error rates for each design
  alpha <- 1 - stats::pbinom(x_1, n, rho_0) +
    0.5*(stats::pbinom(x_1, n, rho_0) - stats::pbinom(x_0, n, rho_0))
  
  beta <- stats::pbinom(x_0, n, rho_1) +
    0.5*(stats::pbinom(x_1, n, rho_1) - stats::pbinom(x_0, n, rho_1))
  
  gamma_U <- 1 - stats::pbinom(x_1, n, rho_0 + (rho_1 - rho_0)/2)
  gamma_L <- stats::pbinom(x_0, n, rho_0 + (rho_1 - rho_0)/2)
  gamma <- gamma_L + gamma_U
  
  return(cbind(alpha, beta, gamma))
}