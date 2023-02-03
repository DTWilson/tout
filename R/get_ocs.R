get_ocs_bin <- function(n, x_0, x_1, rho_0, rho_1, eta = 0.5){
  
  # Calculate the error rates for each design - binary case
  
  alpha <- 1 - stats::pbinom(x_1, n, rho_0) +
    eta*(stats::pbinom(x_1, n, rho_0) - stats::pbinom(x_0, n, rho_0))
  
  beta <- stats::pbinom(x_0, n, rho_1) +
    eta*(stats::pbinom(x_1, n, rho_1) - stats::pbinom(x_0, n, rho_1))
  
  gamma_U <- 1 - stats::pbinom(x_1, n, rho_0 + (rho_1 - rho_0)/2)
  gamma_L <- stats::pbinom(x_0, n, rho_0 + (rho_1 - rho_0)/2)
  gamma <- gamma_L + gamma_U
  
  return(cbind(alpha, beta, gamma))
}

get_ocs_cont_z <- function(n, x_0, x_1, rho_0, rho_1, sigma, tau_min, tau_max, eta = 0.5){
  
  ncp_alpha <- sqrt(n)*(-tau_min)/sigma
  alpha <- max(1 - stats::pnorm(x_1, mean = 0, sd = 1),
               1 - stats::pnorm(x_1, mean = ncp_alpha, sd = 1) + 
                 eta*(stats::pnorm(x_1, mean = ncp_alpha, sd = 1) - stats::pnorm(x_0, mean = ncp_alpha, sd = 1)))

  ncp_beta <- sqrt(n)*(rho_1 - tau_max - rho_0)/sigma
  beta <- stats::pnorm(x_0, mean = ncp_beta, sd = 1) + 
    eta*(stats::pnorm(x_1, mean = ncp_beta, sd = 1) - stats::pnorm(x_0, mean = ncp_beta, sd = 1))
  
  # Our gamma OC now generalised to allow for adjustment - want to 
  # minimise prob of hard decision when we are between null and most
  # optimistic alt, where we then want to use other info to make
  # decision.
  ncp_gamma <- 0.5*sqrt(n)*(rho_1 - tau_max - rho_0)/sigma
  gamma_U <- 1 - stats::pnorm(x_1, mean = ncp_gamma, sd = 1)
  gamma_L <- stats::pnorm(x_0, mean = ncp_gamma, sd = 1)
  gamma <- gamma_L + gamma_U
  
  return(cbind(alpha, beta, gamma))
}

# TO DO: implement case for T test / unknown variance