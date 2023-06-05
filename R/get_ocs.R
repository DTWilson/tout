
#' @export
get_ocs_bin <- function(n, x_0, x_1, rho_0, rho_1, tau_min, tau_max, eta){
  
  # Calculate the error rates for each design - binary case
  
  alpha <- max(1 - stats::pbinom(x_1, n, rho_0), 
               1 - stats::pbinom(x_1, n, rho_0 - tau_min) +
                 eta*(stats::pbinom(x_1, n, rho_0 - tau_min) - stats::pbinom(x_0, n, rho_0 - tau_min)))
  
  beta <- stats::pbinom(x_0, n, rho_1 - tau_max) +
    eta*(stats::pbinom(x_1, n, rho_1 - tau_max) - stats::pbinom(x_0, n, rho_1 - tau_max))
  
  mid <- 0.5*(rho_0 + rho_1 - tau_max - tau_min)
  gamma_U <- 1 - stats::pbinom(x_1, n, mid)
  gamma_L <- stats::pbinom(x_0, n, mid)
  gamma <- gamma_L + gamma_U
  
  return(cbind(alpha, beta, gamma))
}

get_ocs_cont_z <- function(n, x_0, x_1, rho_0, rho_1, sigma, tau_min, tau_max, eta){
  
  ncp_alpha <- sqrt(n)*(-tau_min)/sigma
  alpha <- max(1 - stats::pnorm(x_1, mean = 0, sd = 1),
               1 - stats::pnorm(x_1, mean = ncp_alpha, sd = 1) + 
                 eta*(stats::pnorm(x_1, mean = ncp_alpha, sd = 1) - stats::pnorm(x_0, mean = ncp_alpha, sd = 1)))

  ncp_beta <- sqrt(n)*(rho_1 - rho_0 - tau_max)/sigma
  beta <- stats::pnorm(x_0, mean = ncp_beta, sd = 1) + 
    eta*(stats::pnorm(x_1, mean = ncp_beta, sd = 1) - stats::pnorm(x_0, mean = ncp_beta, sd = 1))
  
  ncp_gamma <- 0.5*sqrt(n)*(rho_1 - rho_0 - tau_max - tau_min)/sigma
  gamma_U <- 1 - stats::pnorm(x_1, mean = ncp_gamma, sd = 1)
  gamma_L <- stats::pnorm(x_0, mean = ncp_gamma, sd = 1)
  gamma <- gamma_L + gamma_U
  
  return(cbind(alpha, beta, gamma))
}

# TO DO: implement case for T test / unknown variance